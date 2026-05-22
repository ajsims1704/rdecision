#' rdecision: Decision Analytic Modelling in Health Economics.
#'
#' The goal of `rdecision` is to provide methods for assessing health care
#' interventions using cohort models (decision trees and semi-Markov models)
#' which can be constructed using only a few lines of R code. Mechanisms
#' are provided for associating an uncertainty distribution with each source
#' variable and for ensuring transparency of the mathematical relationships
#' between
#' variables. The package terminology follows Briggs \emph{et al} "Decision
#' Modelling for Health Economic Evaluation" (2006, ISBN:978-0-19-852662-9).
#'
#' @keywords internal
"_PACKAGE"
#'
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics rect
#' @importFrom graphics strwidth
#' @importFrom graphics text
#'
#' @importFrom grDevices dev.size
#'
#' @importFrom grid addGrob
#' @importFrom grid convertUnit
#' @importFrom grid grobTree
#' @importFrom grid is.unit
#' @importFrom grid lineToGrob
#' @importFrom grid moveToGrob
#' @importFrom grid textGrob
#' @importFrom grid viewport
#'
#' @importFrom R6 R6Class
#'
#' @importFrom rlang eval_bare
#' @importFrom rlang eval_tidy
#' @importFrom rlang is_quosure
#' @importFrom rlang new_quosure
#' @importFrom rlang quo
#' @importFrom rlang quos
#' @importFrom rlang quo_get_env
#' @importFrom rlang quo_get_expr
#'
#' @importFrom stats complete.cases
#' @importFrom stats quantile
#' @importFrom stats qbeta
#' @importFrom stats rgamma
#' @importFrom stats runif
#' @importFrom stats sd
#'
#' @importFrom withr with_par
#'
NULL
