#!/usr/bin/env Rscript

# Run from the package root: Rscript --vanilla tools/test-stan-build.R
# An optional argument selects another source package. No models are compiled.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L)
  stop("Usage: Rscript --vanilla tools/test-stan-build.R [PACKAGE]", call. = FALSE)
package <- normalizePath(if (length(args)) args[1L] else getwd(), mustWork = TRUE)
if (!file.exists(file.path(package, "tools", "update-stan.R")))
  stop("The package must contain tools/update-stan.R", call. = FALSE)

scratch <- tempfile("stan-build-tests-")
dir.create(scratch)
# R also removes this session's temporary directory if a test fails.
cat("Testing an isolated source copy in", scratch, "\n")

source_dirs <- c("R", "inst/stan", "inst/include", "src", "tools")
files <- c("DESCRIPTION", "NAMESPACE", unlist(lapply(source_dirs, function(path) {
  entries <- list.files(file.path(package, path), recursive = TRUE,
                        all.files = TRUE, no.. = TRUE)
  file.path(path, entries)
})))
files <- files[!grepl("\\.(o|so|dll|dylib|rds)$", files, ignore.case = TRUE)]
for (file in files) {
  destination <- file.path(scratch, file)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(file.path(package, file), destination))
    stop("Could not copy ", file, call. = FALSE)
}

native_files <- c("src/stanExports_simple.h", "src/stanExports_complex.h",
                  "src/stanExports_simple.cc", "src/stanExports_complex.cc",
                  "src/RcppExports.cpp")
rscript <- file.path(R.home("bin"),
                     if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
update_script <- file.path(scratch, "tools", "update-stan.R")

expect <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, "\nTest copy: ", scratch, call. = FALSE)
}

hashes <- function(paths = list.files(scratch, recursive = TRUE,
                                     all.files = TRUE, no.. = TRUE)) {
  paths <- sort(paths)
  values <- tools::md5sum(file.path(scratch, paths))
  names(values) <- paths
  values
}

run_update <- function(mode, succeeds) {
  output <- suppressWarnings(system2(rscript,
    c("--vanilla", shQuote(update_script), mode, shQuote(scratch)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  expect((status == 0L) == succeeds,
         paste(c(paste("Unexpected exit status for", mode, ":", status),
                 output), collapse = "\n"))
  if (!succeeds) expect(length(output) > 0L, "A failure must explain its cause.")
  invisible(output)
}

restore <- function(paths) {
  copied <- file.copy(file.path(package, paths), file.path(scratch, paths),
                      overwrite = TRUE)
  expect(all(copied), "Could not restore the fixture.")
}

# A current checkout must pass, and checking must not write into the package.
baseline <- hashes()
run_update("--check", TRUE)
expect(identical(hashes(), baseline), "--check modified the source package.")
cat("PASS: baseline check is read-only.\n")

run_update("--invalid-option", FALSE)
expect(identical(hashes(), baseline), "An invalid command modified the source package.")
cat("PASS: invalid command is rejected without writes.\n")

# Neither checking nor regeneration may silently drop a missing model.
missing_stan <- "inst/stan/complex.stan"
unlink(file.path(scratch, missing_stan))
before <- hashes()
run_update("--check", FALSE)
expect(identical(hashes(), before), "Checking a missing model modified the package.")
run_update("--write", FALSE)
expect(identical(hashes(), before), "A missing model caused partial regeneration.")
restore(missing_stan)
expect(identical(hashes(), baseline), "The missing-model fixture was not restored.")
cat("PASS: missing Stan source is rejected without writes.\n")

# Missing generated files should be recoverable from the intact Stan sources.
missing_header <- "src/stanExports_simple.h"
unlink(file.path(scratch, missing_header))
before <- hashes()
run_update("--check", FALSE)
expect(identical(hashes(), before), "Checking a missing header modified the package.")
run_update("--write", TRUE)
expect(identical(readLines(file.path(scratch, missing_header), warn = FALSE),
                 readLines(file.path(package, missing_header), warn = FALSE)),
       "Regeneration did not restore the missing header.")
restore(native_files)
expect(identical(hashes(), baseline), "Missing-header regeneration modified unrelated files.")
cat("PASS: missing generated header is detected and restored.\n")

# Test both models, both wrappers, and their shared native registration.
for (file in native_files) {
  cat("\n// Deliberately stale generated output.\n",
      file = file.path(scratch, file), append = TRUE)
  before <- hashes()
  output <- run_update("--check", FALSE)
  expect(any(grepl(basename(file), output, fixed = TRUE)),
         paste("The stale-file error did not identify", file))
  expect(identical(hashes(), before), paste("--check modified", file))
  run_update("--write", TRUE)
  for (generated in native_files) {
    expect(identical(readLines(file.path(scratch, generated), warn = FALSE),
                     readLines(file.path(package, generated), warn = FALSE)),
           paste("Regeneration did not repair", generated))
  }
  # Preserve the original checkout's line endings for the next fixture.
  restore(native_files)
  expect(identical(hashes(), baseline), "Regeneration modified unrelated source files.")
  cat("PASS: stale", file, "is detected and repaired.\n")
}

# A valid change in each Stan model must invalidate its generated header.
for (model in c("simple", "complex")) {
  stan_file <- paste0("inst/stan/", model, ".stan")
  text <- readLines(file.path(scratch, stan_file), warn = FALSE)
  changed <- sub("generated quantities {",
                 "generated quantities {\n  real build_test_value = 42;",
                 text, fixed = TRUE)
  expect(!identical(text, changed), "Could not insert the valid Stan test change.")
  writeLines(changed, file.path(scratch, stan_file))
  before <- hashes()
  run_update("--check", FALSE)
  expect(identical(hashes(), before), "A stale-source check modified the package.")
  run_update("--write", TRUE)
  header <- paste0("src/stanExports_", model, ".h")
  expect(hashes(header) != baseline[header], "Changed Stan did not change its generated header.")
  run_update("--check", TRUE)
  restore(c(stan_file, native_files))
  expect(identical(hashes(), baseline), "The valid-change fixture was not restored.")
  cat("PASS:", model, "Stan changes require and survive regeneration.\n")
}

# Even when one model is valid but changed, failure to translate the other
# model must leave every generated output untouched.
simple <- "inst/stan/simple.stan"
complex <- "inst/stan/complex.stan"
text <- readLines(file.path(scratch, simple), warn = FALSE)
writeLines(sub("generated quantities {",
               "generated quantities {\n  real build_test_value = 42;",
               text, fixed = TRUE), file.path(scratch, simple))
cat("\nthis is deliberately invalid Stan;\n",
    file = file.path(scratch, complex), append = TRUE)
before <- hashes()
run_update("--check", FALSE)
expect(identical(hashes(), before), "A failed check modified the source package.")
run_update("--write", FALSE)
expect(identical(hashes(), before), "Failed translation partially rewrote the package.")
restore(c(simple, complex))
expect(identical(hashes(), baseline), "The malformed-Stan fixture was not restored.")
cat("PASS: malformed Stan leaves all generated outputs untouched.\n")

unlink(scratch, recursive = TRUE)
cat("All Stan generation checks passed; no C++ compilation or sampling was performed.\n")
