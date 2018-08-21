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
#' @author
#' \enumerate{
#'          \item Rodney N. Edmondson (\email{rodney.edmondson@@gmail.com})
#'          \item Hans-Peter Piepho (\email{piepho@@uni-hohenheim.de})
#'          \item Muhammad Yaseen (\email{myaseen208@@gmail.com})
#'          }
#' 
#'
#'
#' @references
#' \enumerate{
#'          \item  Petersen, R. G. (1994). \emph{Agricultural Field Experiments: Design and Analysis}. CRC Press.
#'          \item  Kenward, M. G., & Roger, J. H. (1997). Small sample inference for fixed effects from restricted maximum likelihood. \emph{Biometrics},  \strong{53}, 983–997.
#'          \item Piepho, H., & Edmondson, R. (2018). A tutorial on the Statistical Analysis of Factorial Experiments with Qualitative and Quantitative treatment factor levels. 
#'                \emph{Journal of Agronomy and Crop Science.} (\url{https://onlinelibrary.wiley.com/doi/full/10.1111/jac.12267}).
#'          }
#' 
#' @import broom broom.mixed dplyr emmeans ggfortify ggplot2 lmerTest magrittr nlme pbkrtest
#'
#'
#' @examples
#' library(broom) 
#' library(broom.mixed) 
#' library(dplyr)
#' library(emmeans) 
#' library(ggfortify) 
#' library(ggplot2)
#' library(lmerTest) 
#' library(magrittr) 
#' library(nlme) 
#' library(pbkrtest)
#' 
#' options(contrasts = c('contr.treatment', 'contr.poly'))
#' 
#' ##----fm2.1----
#' fm2.1 <- lm(yield ~ Replicate + nrate + I(nrate^2) + I(nrate^3) + I(nrate^4), data = beet)
#' fm2.1.ANOVA <- anova(fm2.1)
#' 
#' ##----fm2.1.ANOVA----
#'  fm2.1.ANOVA
#' 
#' 
#' ##----fm2.2----
#' fm2.2 <- lm(yield ~ Replicate + nrate + I(nrate^2), data = beet)
#' fm2.2.Coef <- summary(fm2.2)$coef
#' 
#' ##----fm2.2.Coef----
#' fm2.2.Coef
#' 
#' ##----fm2.2.Coefs----
#' fm2.2.Coef[1, 1] + sum(fm2.2.Coef[2:3, 1])/3
#' fm2.2.Coef[4, 1]
#' fm2.2.Coef[5, 1]
#' 
#' confint(fm2.2, level = 0.95)
#' 
#' ##----fm2.2.Plot1----
#' ggplot2::autoplot(fm2.2)
#' 
#' ##----fm2.2.Plot2----
#' ggplot(data = beet, mapping = aes(x = nrate, y = yield)) +
#'   geom_point(shape = 1) + 
#'   stat_summary(fun.y = mean, geom = "point") +
#'   geom_smooth(method = lm, formula = y ~ poly(x, 2)) +
#'   labs(
#'     x = "Amont of nitrogen (kg)"
#'     ,  y = "Yield"
#'     , title = "Fig 3 Yield versus N for sugar beet with 95% confidence band"
#'   ) +
#'   theme_bw() 
#' 
#' 
NULL
