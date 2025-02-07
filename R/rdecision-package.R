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
#' @importFrom graphics
#'   abline
#'   axis
#'   rect
#'   strwidth
#'   text
#' @importFrom grDevices
#'   dev.size
#' @importFrom grid
#'   addGrob
#'   convertUnit
#'   grobTree
#'   is.unit
#'   lineToGrob
#'   moveToGrob
#'   textGrob
#'   viewport
#' @importFrom R6
#'   R6Class
#' @importFrom rlang
#'   eval_bare
#'   eval_tidy
#'   is_quosure
#'   new_quosure
#'   quo
#'   quos
#'   quo_get_env
#'   quo_get_expr
#' @importFrom stats
#'   complete.cases
#'   quantile
#'   qbeta
#'   rgamma
#'   runif
#'   sd
#' @importFrom withr
#'   with_par
#'
NULL
