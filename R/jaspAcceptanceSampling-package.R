#' jaspAcceptanceSampling: Lot Sampling for Acceptance/Rejection of Lots
#'
#' Package-level imports and shared registration for the Acceptance Sampling
#' module. RcppParallel runtime libraries are imported explicitly so the
#' package DLL can resolve its TBB dependency during loading on Windows.
#'
#' @keywords internal
#' @docType package
#' @name jaspAcceptanceSampling-package
#' @aliases jaspAcceptanceSampling jaspAcceptanceSampling-package
#' @import Rcpp
#' @importFrom RcppParallel RcppParallelLibs
#' @useDynLib jaspAcceptanceSampling, .registration = TRUE
"_PACKAGE"
