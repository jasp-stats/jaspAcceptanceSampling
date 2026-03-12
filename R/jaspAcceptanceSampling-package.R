#' jaspAcceptanceSampling: Lot Sampling for Acceptance/Rejection of Lots
#'
#' Package-level imports and shared registration for the Acceptance Sampling
#' module. The Stan toolchain is bootstrapped via rstantools configure scripts
#' to keep the generated build configuration aligned across platforms.
#'
#' @keywords internal
#' @docType package
#' @name jaspAcceptanceSampling-package
#' @aliases jaspAcceptanceSampling jaspAcceptanceSampling-package
#' @import Rcpp
#' @import rstantools
#' @importFrom RcppParallel RcppParallelLibs
#' @useDynLib jaspAcceptanceSampling, .registration = TRUE
"_PACKAGE"
