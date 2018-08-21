#' @title Turnip data for Example 5
#' @name   turnip
#' @docType data
#' @keywords datasets
#' @usage data(turnip)
#' @description Mead (1988, p. 323) describes an experiment on spacing effects with turnips, which was laid out in three complete blocks. Five different seed rates (0.5, 2, 8, 20, 32 lb/acre) were tested in combination with four row widths (4, 8, 16, 32 inches), giving rise to a total of 20 treatments. Turnip yields (in lb per plot) were logarithmically transformed for analysis because this stabilized the variance (Mead, 1988; also see Figure 12).
#' @format An object of class \code{data.frame} with 60 rows and 6 columns.
#' @details
#'        \itemize{
#'        \item{log_yield} log_yield
#'        \item{Replicate} Replicate
#'        \item{Density} Density
#'        \item{Spacing} Spacing
#'        \item{rowspacing} rowspacing        
#'        \item{density} density
#'        }
#'
#' @references
#' Mead, R. (1990). \emph{The Design of Experiments: Statistical Principles for Practical Applications}. 
#' Cambridge University Press.
#'
#'
#' @examples
#' data(turnip)
#'
NULL
