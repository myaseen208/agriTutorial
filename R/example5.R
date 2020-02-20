#' @name example5
#' @title  Example 5: Transformation of treatment levels to improve model fit
#' @description
#' Mead (1988, p. 323) describes an experiment on spacing effects with turnips,
#' which was laid out in three complete blocks. Five different seed rates
#' (0.5, 2, 8, 20, 32 lb/acre) were tested in combination with four different row widths
#' (4, 8, 16, 32 inches), giving rise to a total of 20 treatments.
#' @details
#' Transformation of the dependent variable will often stabilize the variance of the observations
#' whereas transformation of the regressor variables will often simplify the fitted model. In this
#' example, the fit of a regression model based on the original seed rate and row width variables is compared
#' with the fit of a regression model based on the log transformed seed rates and log transformed row widths.
#' In each case, the model lack-of-fit is examined by assessing the extra variability explained when the
#' Density and Spacing treatment factors and their interactions are added to the quadratic regression models.
#' All yields are logarithmically transformed to stabilize the variance.
#'
#' The first analysis fits a quadratic regression model of log yields on the untransformed seed rates and row
#' widths (Table 16) while the second analysis fits a quadratic regression model of log yields on the log
#' transformed seed rates and log transformed row widths (Table 17). The analysis of variance of the first model
#' shows that significant extra variability is explained by the Density and
#' Spacing factors and this shows that a quadratic regression model is inadequate for the untransformed regressor
#' variables. The analysis of variance of the second model, however, shows no significant extra variability
#' explained by the Density and Spacing factors and this shows that the quadratic regression model with the log
#' transformed regressor variables gives a good fit to the data and therefore is the preferred model for the
#' observed data.
#'
#' The superiority of the model with the log transformed regressor variables is confirmed by comparing the fit of the
#' quadratic regression model for the untransformed regressor variables (Figs 8 and 9) versus the fit of the
#' quadratic regression model for the log transformed regressor variables (Figs 10 and 11).
#'
#' Fig 12a shows diagnostic plots for the fit of a quadratic model with untransformed regressor variables
#' while Fig 12b shows corresponding diagnostic plots for the fit of a quadratic model with
#' loge transformed regressor variables. Each of the four types of diagnostic plots in the two figures
#' shows an improvement in fit for the transformed versus the untransformed regressor variables.
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
#'          \item  Mead, R. (1990). \emph{The Design of Experiments: Statistical Principles for Practical Applications}. Cambridge University Press. 
#'          \item  Kenward, M. G., & Roger, J. H. (1997). Small sample inference for fixed effects from restricted maximum likelihood. \emph{Biometrics},  \strong{53}, 983–997.
#'          \item Piepho, H., & Edmondson, R. (2018). A tutorial on the Statistical Analysis of Factorial Experiments with Qualitative and Quantitative treatment factor levels. 
#'                \emph{Journal of Agronomy and Crop Science.} (\url{https://onlinelibrary.wiley.com/doi/full/10.1111/jac.12267}).
#'          }
#' 
#' 
#' @importFrom broom tidy glance augment
#' @importFrom broom.mixed tidy glance augment
#' @importFrom dplyr filter group_by ungroup select summarise collapse
#' @importFrom emmeans emmeans contrast
#' @importFrom lmerTest lmer
#' @importFrom tidyr  spread
#' @import  ggfortify ggplot2 magrittr
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
#' library(tidyr)
#' 
#' options(contrasts = c('contr.treatment', 'contr.poly'))
#'
#' ##----fm5.1----
#' fm5.1 <- lm(log_yield ~ Replicate + density * rowspacing + 
#'               I(density^2) + I(rowspacing^2) + Density * Spacing
#'             , turnip)
#' fm5.1.ANOVA <- anova(fm5.1)
#' 
#' 
#' ##----fm5.1.ANOVA----
#' fm5.1.ANOVA
#' 
#' ##----fm5.2----
#' fm5.2 <- lm(log_yield ~ Replicate + log(density) * log(rowspacing) +
#'               I(log(density)^2) + I(log(rowspacing)^2) + 
#'               Density * Spacing, turnip)
#' fm5.2.ANOVA <- anova(fm5.2)
#' 
#' 
#' ##----fm5.2.ANOVA----
#' fm5.2.ANOVA
#' 
#' ##----fm5.3----
#' fm5.3 <- lm(log_yield ~ density * rowspacing + I(density^2) + 
#'               I(rowspacing^2) , turnip)
#' fm5.3.Coef <- broom::tidy(fm5.3)
#' turnip1 <- broom::augment(fm5.3, turnip)
#' 
#' ##----fm5.3.Coef----
#' fm5.3.Coef
#' 
#' ##----fm5.3.Plot1----
#' ggplot(data = turnip1, 
#'        mapping = aes(x = rowspacing, y = log_yield, 
#'                      color = factor(density), group = factor(density))) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y =.fitted), method = "loess")
#' 
#' ##----fm5.3.Plot2----
#' ggplot(data = turnip1, 
#'        mapping = aes(x = rowspacing, y = exp(log_yield), 
#'                      color = factor(density), group = factor(density))) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y =exp(.fitted)), method = "loess")
#' 
#' ##----fm5.3.Plot3----
#' ggplot(data = turnip1, 
#'        mapping = aes(x = density, y = log_yield, 
#'                      color = factor(rowspacing), group = factor(rowspacing))) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y =.fitted), method = "loess")
#' 
#' ##----fm5.3.Plot4----
#' ggplot(data = turnip1, 
#'        mapping = aes(x = density, y = exp(log_yield), 
#'                      color = factor(rowspacing), group = factor(rowspacing))) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y =exp(.fitted)), method = "loess")
#' 
#' 
#' ##----fm5.4----
#' fm5.4 <- lm(log_yield ~ log(density) * log(rowspacing) + 
#'               I(log(density)^2) + I(log(rowspacing)^2),
#'             turnip)
#' fm5.4.Coef <- broom::tidy(fm5.4)
#' turnip2 <- broom::augment(fm5.4, turnip)
#' 
#' ##----fm5.4.Plot1----
#' ggplot(data = turnip2, 
#'        mapping = aes(x = log(rowspacing), y = log_yield, 
#'                      color = factor(density), group = factor(density))) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y =.fitted), method = "loess")
#' 
#' ##----fm5.4.Plot2----
#' ggplot(data = turnip2, 
#'        mapping = aes(x = log(rowspacing), y = exp(log_yield), 
#'                      color = factor(density), group = factor(density))) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y =exp(.fitted)), method = "loess")
#' 
#' ##----fm5.4.Plot3----
#' ggplot(data = turnip2, 
#'        mapping = aes(x = log(density), y = log_yield, 
#'                      color = factor(rowspacing), group = factor(rowspacing))) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y =.fitted), method = "loess")
#' 
#' ##----fm5.4.Plot4----
#' ggplot(data = turnip2, 
#'        mapping = aes(x = log(density), y = exp(log_yield), 
#'                      color = factor(rowspacing), group = factor(rowspacing))) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y = exp(.fitted)), method = "loess")
#' 
#' 
#' ##----fm5.5----
#' fm5.5 <- lm(log_yield ~ density * rowspacing + 
#'               I(density^2) + I(rowspacing^2),
#'             turnip)
#' 
#' ##----fm5.5.Plot1----
#' ggplot2::autoplot(fm5.5)
#' 
#' ##----fm5.6----
#' fm5.6 <- lm(log_yield ~ log(density) * log(rowspacing) + 
#'               I(log(density)^2) + I(log(rowspacing)^2),
#'             turnip)
#' 
#' ##----fm5.6.Plot1----
#' ggplot2::autoplot(fm5.6)
#' 
NULL
