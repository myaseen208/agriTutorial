#' @name example2
#' @title  Example 2: Lack-of-fit and marginality for a single quantitative treatment factor
#' @description
#' Petersen (1994, p. 125) describes an experiment conducted to assess the effects
#' of five different quantities of N-fertiliser (0, 35, 70, 105 and 140 kg N/ha) on root dry
#' matter yield of sugar beet (t/ha) with three complete replications laid out in three
#' randomized complete blocks. One objective of this experiment was to determine the amount
#' of fertilizer for maximizing yield.
#'
#' @details
#' The first stage of the analysis is the calculation of raw polynomial powers of N using the poly() function.
#' The N rates are re-scaled by division by 100 to improve numerical stability.
#'
#' The second stage fits a full polynomial analysis of variance based on polynomial contrasts which are
#' fitted in sequence from the lowest to the highest. This is equivalent to the analysis shown in Tables 4 and 5
#' of Piepho and Edmondson (2018) except that a complete partition into single degree of freedom polynomial contrasts is
#' shown here compared with the pooled 'lack of fit' term shown in Tables 4 and 5.
#'
#' The third stage fits a quadratic regression model with linear and quadratic terms only.
#' This model provides the model coefficients, standard errors and the confidence intervals
#' shown in Table 6 of Piepho and Edmondson (2018). A set of diagnostic plots are fitted for
#' the fitted quadratic regression model to check the validity of the model assumptions.
#'
#' Finally, a smoothed quadratic graph of the yield versus the N rate is plotted to show the
#' goodness of fit of the quadratic regression model. This plot corresponds to plot Fig 3 in
#' Piepho and Edmondson (2018).
#'
#' \code{\link[agriTutorial]{agriTutorial}}: return to home page if you want to select a different example \cr
#'
#' @references
#' Petersen, R.G. (1994). Agricultural field experiments. Design and analysis. New York: Marcel Dekker.
#'
#' Piepho, H. P, and Edmondson. R. N. (2018). A tutorial on the statistical analysis of factorial experiments with qualitative and quantitative
#' treatment factor levels. Journal of Agronomy and Crop Science. DOI: 10.1111/jac.12267.
#' \href{http://onlinelibrary.wiley.com/doi/10.1111/jac.12267/full}{Early View}
#'
#' @examples
#'
#' ## *************************************************************************************
#' ##                       How to run the code
#' ## *************************************************************************************
#'
#' ## Either type example("example2") to run ALL the examples succesively
#' ## or copy and paste examples sucessively, as required
#'
#' ## *************************************************************************************
#' ##                         Options and required packages
#' ## *************************************************************************************
#'
#' options(contrasts = c('contr.treatment', 'contr.poly'))
#' ## ggplot2 MUST be installed
#' require(ggplot2)
#'
#' ## *************************************************************************************
#' ##         Polynomial analysis and graphical plots of factorial treatment effects
#' ## *************************************************************************************
#'
#' N = poly((beet$nrate/100), degree = 4, raw = TRUE)
#' colnames(N) = c("Linear_N", "Quadratic_N", "Cubic_N", "Quartic_N")
#' beet = cbind(beet, N)
#'
#' ## Tables 4 and 5: Full polynomial analysis of variance based on raw polynomials
#' anova(lm(yield ~ Replicate + Linear_N + Quadratic_N + Cubic_N + Quartic_N, data = beet))
#'
#' ##  Table 6: showing quadratic model coefficients with standard errors and confidence intervals
#' quadratic = lm(yield ~ Replicate + Linear_N + Quadratic_N, data = beet)
#' summary(quadratic)
#' confint(quadratic, level = 0.95)
#'
#' par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
#' plot(quadratic, sub.caption = NA)
#' title(main = "Diagnostic plots for quadratic nitrogen effects model", outer = TRUE)
#'
#' ggplot(beet, aes(x = nrate, y = yield)) +
#'  ggtitle("Fig 3 Yield versus N for sugar beet with 95% confidence band") +
#'  geom_point(shape = 1) + stat_summary(fun.y = mean, geom = "point") +
#'  geom_smooth(method = lm, formula = y ~ poly(x, 2)) + theme_bw()
#'
#' @importFrom ggplot2 ggplot
#'
NULL
