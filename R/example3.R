#' @name example3
#' @title  Example 3: Polynomial regression model with two quantitative level treatment factors
#' @description
#' Gomez & Gomez (1984, p. 401) report a two-factor nitrogen uptake greenhouse experiment on rice
#' involving duration of water stress (W) and level of nitrogen application (N) with four complete replicates of each treatment.
#' The experiment had four water-stress levels (0, 10, 20 and 40 days) applied as main-plot treatments and
#' four nitrogen rates (0, 90, 180 and 270 kg/ha) applied as sub-plot treatments. The four sub-plot treatments were randomized
#'  within main plots and the four main plot treatments were randomized within complete replicate blocks.
#'
#' @details
#' The first stage of the analysis is the calculation of polynomial powers of N and W using the poly() function.
#' The N rates are re-scaled by division by 100 while the W rates are re-scaled by division by 10.
#'
#' The second stage shows a Pearson residual plot of the untransformed N uptake data versus a Pearson residual plot of
#' the log transformed N uptake data. Comparison of the two plots shows that the untransformed residuals
#' increase as the fitted values increase whereas the log transformed N uptake residuals are approximately constant
#' over the full range of the fitted values. This shows that a log transformation of the N uptake data gives a
#' dependent variate with constant variance over the full range of fitted values which shows that a simple
#' unweighted analysis of variance is valid for the effects of the treatment factors.
#'
#' Sometimes the original scale of measurement is the proper scale of measurement for an analysis,
#' e.g. an analysis of actual measured crop yields, and then it might be appropriate to fit a weighted
#' analysis of variance in the original scale of measurement of the dependent variable (see Faraway 2002 Chapter 5).
#' However, the log transformation model assumes a proportional rather than an additive model
#' for treatment effects and, in this example, a proportional model for nitrogen uptake may well be a more natural
#' physiological model than a simple additive model.
#'
#' The next stage compares the fit of a first-order linear model (Table 9) versus a second-order quadratic
#' model (Table 10). The first-order model shows significant lack-of-fit and is not adequate for the
#' data. The second-order model is also not fully adequate for the data as there is a significant N lack of
#' fit term indicating a significant cubic effect. However, the magnitude of the cubic effect is
#' relatively small and it will be assumed here that a quadratic model is adequate for the data.
#'
#' The final stage fits regression coefficients for the quadratic response surface model on the
#' re-scaled water stress and re-scaled nitrogen rate treatments. The fitted coefficients are then used to
#' plot the fitted quadratic log uptake curves versus the nitrogen rate treatments and the
#' water stress treatments, as shown in Fig 4.
#'
#' Note that in this analysis all the polynomial models are built by adding individual polynomial effects in accordance with the requirements of
#' functional marginality.
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
#'          \item  Gomez, K. A., & Gomez, A. A. (1984). \emph{Statistical Procedures for Agricultural Research}.   John Wiley & Sons.
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
#' ##----greenrice----
#' greenrice <-
#'   greenrice %>%
#'   dplyr::mutate(
#'     loguptake   = log(uptake)
#'     , Nitrogen    = factor(N)
#'     , Water       = factor(W)
#'   )
#' 
#' 
#' ##----fm3.1----
#' fm3.1 <- lmer(uptake ~ Replicate + Nitrogen * Water +
#'                 (1|Replicate:Main), data = greenrice)
#' 
#' ##----fm3.1.Plot1----
#' fm3.1.Augment <- broom.mixed::augment(fm3.1)
#' 
#' ggplot(data = fm3.1.Augment, mapping = aes(x = .fitted, y = .resid)) +
#'   geom_point() +
#'   geom_hline(yintercept = 0) +
#'   labs(
#'     x = "Fitted"
#'     , y = "Residuals N uptake"
#'     , title = "Pearson residual plot for untransformed N uptake") +
#'   theme_bw() +
#'   theme(plot.title = element_text(hjust = 0.5))
#' 
#' ##----fm3.2----
#' fm3.2 <- lmer(loguptake ~ Replicate + Nitrogen * Water +
#'                 (1|Replicate:Main), data = greenrice)
#' 
#' ##----fm3.2.Plot1----
#' fm3.2.Augment <- broom.mixed::augment(fm3.2)
#' 
#' ggplot(data = fm3.2.Augment, mapping = aes(x = .fitted, y = .resid)) +
#'   geom_point() +
#'   geom_hline(yintercept = 0) +
#'   labs(
#'     x = "Fitted"
#'     , y = "Residuals log N uptake"
#'     , title = "Pearson residual plot for log transformed N uptake") +
#'   theme_bw() +
#'   theme(plot.title = element_text(hjust = 0.5))
#' 
#' 
#' ##----fm3.3----
#' fm3.3 <- lmer(loguptake ~ N + W + Nitrogen * Water +
#'                 (1|Replicate) + (1|Replicate:Main), data = greenrice)
#' fm3.3.ANOVA <- anova(fm3.3, ddf = "Kenward-Roger", type = 1)
#' fm3.3.Summary <- summary(fm3.3, ddf = "Kenward-Roger", type = 1)$coef
#' 
#' ##----fm3.3.ANOVA----
#' fm3.3.ANOVA
#' 
#' ##----fm3.3.Summary----
#' fm3.3.Summary
#' 
#' ##----fm3.4----
#' fm3.4 <- lmer(loguptake ~ N * W + I(N^2) + I(W^2) +
#'                 Nitrogen * Water + (1|Replicate) + (1|Replicate:Main), data = greenrice)
#' fm3.4.Coef <-summary(fm3.4, ddf = "Kenward-Roger", type = 1)$coef
#' greenrice2 <- broom.mixed::augment(fm3.4)
#' 
#' 
#' ##----fm3.4.Coef----
#' fm3.4.Coef
#' 
#' ##----fm3.4.Plot1----
#' ggplot(data = greenrice2, 
#'  mapping = aes(x = Water, y = loguptake, color = Nitrogen, 
#'           group = Nitrogen)) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y =.fitted), method = "loess")
#' 
#' ##----fm3.4.Plot2----
#' ggplot(data = greenrice2, 
#'   mapping = aes(x = Water, y = exp(loguptake), 
#'      color = Nitrogen, group = Nitrogen)) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y =.fitted), method = "loess")
#' 
#' ##----fm3.4.Plot3----
#' ggplot(data = greenrice2, 
#'    mapping = aes(x = Nitrogen, y = loguptake, 
#'       color = Water, group = Water)) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y =.fitted), method = "loess")
#' 
#' ##----fm3.4.Plot4----
#' ggplot(data = greenrice2, 
#'    mapping = aes(x = Nitrogen, y = exp(loguptake), 
#'      color = Water, group = Water)) +
#'   geom_point() +
#'   geom_smooth(mapping = aes(y =.fitted), method = "loess")
#' 
#' ##----fm3.5----
#' fm3.5 <- lmer(loguptake ~ N * W + I(N^2) + I(W^2) +
#'                 (1|Replicate) + (1|Replicate:Main), data = greenrice)
#' fm3.5.Coef <- summary(fm3.5, ddf = "Kenward-Roger", type = 1)$coef
#' 
#' ##----fm3.5.Coef----
#' fm3.5.Coef
#' 
NULL
