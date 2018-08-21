#' @title   Example 1: Split-plot design with one qualitative and one quantitative level factor
#' @name    example1
#' @aliases example1
#' @description Gomez & Gomez (1984, p. 143) report a rice experiment with three 
#'              management practices (minimum, optimum, intensive), five different amounts of
#'              nitrogen (N) fertilizer (0, 50, 80, 110, 140 kg/ha), and three varieties (V1, V2, V3).
#'              The experiment involved variety and management as qualitative treatment factors and 
#'              nitrogen fertilizer as a quantitative treatment factor. Overall, 
#'              there were 45 treatments with three replicates in complete replicate blocks. 
#'              The fertilizer treatments were applied to main plots, the management practices to 
#'              split-plots and the varieties to split-split-plots.
#'
#' @details 
#' Section 1
#' Section 1 examines treatment effects by fitting qualitative factorial models and 
#' the first analysis calculates a full analysis of variance (Table 1) for 
#' main plots (nitrogen), split-plots (management) and split-split-plots (variety). 
#' Each type of experimental unit (or "stratum") requires a separate error term in 
#' the fitted analysis.
#' 
#' The second analysis (Table 2) uses a REML mixed model analysis to find treatment means 
#' and SE's for each marginal treatment classification averaged over all the 
#' other treatment factors, together with estimates of pairwise contrasts of 
#' treatment means and the SE's of the pairwise treatment comparisons. 
#' This analysis fits the full set of nitrogen-by-variety interaction effects 
#' assuming additive management effects and the fit of the model is tested by 
#' a graphical plot of the model residuals. Residual plots provide an important 
#' check on model assumptions but many more options for model testing are 
#' available and further methods for diagnostic testing are examined in the 
#' subsequent examples.
#' 
#' The third analysis (Table 3) shows a mixed model analysis of the full factorial model 
#' fitted by REML using the lmer function of the lme4 package. Generally with 
#' mixed models, determination of the denominator degrees of freedom for 
#' Wald-type F- and t-statistics becomes an issue, and here we use the 
#' method proposed by Kenward & Roger (1997).
#' 
#' Section 2
#' Section 2 examines treatment effects by fitting polynomial models and 
#' the first step calculates a full set of four raw polynomials for the 
#' 5-levels of N using the poly() function. The N rates are re-scaled by 
#' division by 100 to improve numerical stability.
#' 
#' The second step fits a mixed model polynomial analysis of nitrogen effects 
#' assuming additive management effects (Table 7). In this analysis, 
#' most of the nitrogen treatment effect can be explained by linear and 
#' quadratic trend effects. but it is important to note that there is a 
#' non-negligible Variety x Cubic N interaction effect. This suggests 
#' that not all the varieties responded in a similar way to the N treatments 
#' and that some further analysis of the data may be required 
#' (see also the N plots of individual varieties and replicates in Fig 1).
#' 
#' The third step fits the required model for the actual fitted model coefficients 
#' (Table 8). When estimating model effects, only effects that are significant for 
#' the fitted model or that are marginal to those effects (functional marginality) 
#' should be included in the model therefore only linear and quadratic nitrogen effects 
#' are included in this model. The fitted model for the nitrogen effects fits the actual 
#' actual nitrogen levels used in the experiment therefore this model provides the 
#' required coefficients for the actual applied nitrogen levels.
#' 
#' 
#' Section 3
#' 
#' Section 3 provides checks on some of the assumptions underlying 
#' the blocks-by-treatments model.
#' 
#' The first analysis in this section shows a complete partition of 
#' the blocks-by-treatments interaction effects into factorial mean 
#' square terms where all the terms that contain a replicate:variety 
#' interaction effect are estimates of the split-split-plot error 
#' variance. If the blocks-by-treatments assumptions are valid, 
#' all the estimates of the split-split-plot error variance are 
#' expected to have the same error mean square. However, 
#' the Replicate:variety effect has a mean square of 1.54 on 4 
#' degrees of freedom whereas the Replicate:management:variety:nitrogen 
#' effect has a mean square of 0.26 on 32 degrees of freedom. 
#' The ratio of these mean squares is 5.92 with an F-probability of 
#' 0.00110 on 4 and 32 degrees of freedom, which means that the 
#' Replicate:variety interaction effect is significantly inflated 
#' relative to the Replicate:management:variety:nitrogen effect. 
#' This shows that the assumptions underlying the blocks-by-treatments 
#' analysis of the model are invalid with a high level of probability.
#' 
#' The 4 degrees of freedom in the Replicate:variety interaction 
#' effect are the differences between the three varieties differenced 
#' between the three replicate blocks. Fig S1 shows graphical plots 
#' of variety effects in each replicate block averaged over management 
#' effects, and there is clear evidence that the effects of Variety 1 
#' in blocks 1 and 2 were different from the effects of Variety 1 in 
#' block 3.
#' 
#' The second analysis in Section 3 shows a complete partition of the 
#' blocks-by-treatments interaction effects into factorial mean square 
#' terms ignoring Variety 1. This analysis shows a reasonably good fit 
#' to the assumed additive block which supports the hypothesis that 
#' the non-additivity of the block-and-treatment effects in the full 
#' unrestricted analysis is mainly due to Variety 1.
#' 
#' The final analysis in Section 3 shows an analysis of variance of 
#' the treatment effects ignoring Variety 1. In this analysis, 
#' the management:variety interaction effect becomes significant at 
#' the 0.00992 probability level compared with a non-significant 
#' management:variety interaction effect in the analysis of the 
#' full data set.
#' 
#' Such anomalies are not uncommon in the analysis of real data sets 
#' and it is the task of the statistician to identify anomalies as 
#' and when they occur. Factorial designs can be very powerful for 
#' practical research but, as demonstrated with this data set, 
#' the analysis of such designs is complex and anomalies can be 
#' easily missed. Unless an anomaly is due to an easily identified 
#' cause such as an incorrectly recorded data point, it is likely 
#' that the anomaly will need to be investigated by further 
#' discussion with the research workers. It is a mistake to suppose 
#' that data from a designed experiment can be analysed statistically 
#' in isolation from the research workers who conducted the experiment.
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
#' @import broom broom.mixed dplyr emmeans ggfortify ggplot2 lmerTest magrittr nlme pbkrtest
#'
#'
#' @examples
#' data(rice)
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
#' ##----fm1.1----
#' fm1.1 <- aov(yield ~ Replicate + nitrogen * management * variety +
#'                Error(Replicate/nitrogen/management), rice)
#'                
#' fm1.1.Summary <- broom::tidy(fm1.1)
#' fm1.1.Summary
#' ##----fm1.2----
#' fm1.2 <- lmer(yield ~ Replicate + nitrogen * management * variety +
#'                 (1|Replicate:Main) + (1|Replicate:Main:Sub), data = rice)
#' fm1.2.ANOVA <- anova(fm1.2, ddf = "Kenward-Roger", type = 1)
#' fm1.2.ANOVA
#' 
#' 
#' 
#' ##----fm1.3----
#' fm1.3 <- lmer(yield ~ Replicate + nitrogen + management + variety + nitrogen:variety +
#'                 (1|Replicate:Main) + (1|Replicate:Main:Sub), data = rice)
#' fm1.3.ANOVA <- anova(fm1.3, ddf = "Kenward-Roger", type = 1)
#' 
#' emmeans::emmeans(fm1.3, ~ nitrogen)
#' emmeans::emmeans(fm1.3, ~ variety)
#' emmeans::emmeans(fm1.3, ~ nitrogen * variety)
#' emmeans::contrast(
#'   emmeans::emmeans(fm1.3, ~ nitrogen|variety)
#'   , alpha = 0.05
#'   , method = "pairwise"
#' )
#' 
#' emmeans::contrast(
#'   emmeans::emmeans(fm1.3, ~ variety|nitrogen)
#'   , alpha = 0.05
#'   , method = "pairwise"
#' )
#' 
#' ##----fm1.3.Plot----
#' fm1.3.Augment <- broom.mixed::augment(fm1.3)
#' 
#' ggplot(data = fm1.3.Augment, mapping = aes(x = .fitted, y = .resid)) +
#'   geom_point() +
#'   geom_hline(yintercept = 0) +
#'   labs(
#'     x = "Fitted"
#'     , y = "Residuals"
#'     , title = "Full analysis with full nitrogen effects") +
#'   theme_bw() +
#'   theme(plot.title = element_text(hjust = 0.5))
#' 
#' 
#' ##----fm1.4----
#' fm1.4 <- lmer(yield ~ Replicate + nitrogen * management * variety + (1|Replicate:Main) +
#'                 (1|Replicate:Main:Sub), data = rice)
#' fm1.4.ANOVA <- anova(fm1.4, ddf = "Kenward-Roger", type = 1)
#' 
#' fm1.4.ANOVA
#' 
#' ##----fm1.5----
#' fm1.5 <- lmer(yield ~ Replicate + management + variety * (nrate + I(nrate^2) +
#'                                                             I(nrate^3) + I(nrate^4)) + 
#'                 (1|Replicate:Main) + (1|Replicate:Main:Sub), data = rice)
#' fm1.5.ANOVA <- anova(fm1.5, ddf = "Kenward-Roger", type = 1)
#' 
#' fm1.5.ANOVA
#' 
#' ##----fm1.6----
#' fm1.6 <- lmer(yield ~ Replicate + management + variety * nrate + I(nrate^2) +
#'                 (1|Replicate:Main) + (1|Replicate:Main:Sub), data = rice)
#' 
#' fm1.6.Coef <- summary(fm1.6, ddf = "Kenward-Roger")$coef
#' fm1.6.Coef
#' 
#' ##----fm1.6.Coefs----
#' # fm1.6.Coef[ ,1, drop = FALSE]
#' 
#' # Intercepts
#' fm1.6.Coef[1, 1] + sum(fm1.6.Coef[2:3, 1])/3 + sum(fm1.6.Coef[4:5, 1])/3
#' fm1.6.Coef[1, 1] + sum(fm1.6.Coef[2:3, 1])/3 + sum(fm1.6.Coef[4:5, 1])/3 + 
#'   fm1.6.Coef[6, 1]
#' fm1.6.Coef[1, 1] + sum(fm1.6.Coef[2:3, 1])/3 + sum(fm1.6.Coef[4:5, 1])/3 + 
#'   fm1.6.Coef[7, 1]
#' 
#' # Linear Slopes
#' fm1.6.Coef[8, 1]
#' fm1.6.Coef[8, 1] +  fm1.6.Coef[10, 1]
#' fm1.6.Coef[8, 1] +  fm1.6.Coef[11, 1]
#' 
#' # Quadratic Slopes
#' fm1.6.Coef[9, 1]
#' 
#' ##----fm1.7----
#' fm1.7 <- aov(yield ~ Replicate*management * variety * nitrogen, rice)
#' fm1.7.Summary <- broom::tidy(fm1.7)
#' 
#' fm1.7.Summary
#' 
#' ##----fm1.7.Rice----
#' Rice1 <-
#'   rice %>%
#'   dplyr::group_by(Replicate, nitrogen, variety) %>%
#'   dplyr::summarise(Yield = mean(yield, na.rm = TRUE))
#' 
#' Rice1
#' 
#' WideRice1 <-
#'   Rice1 %>%
#'   tidyr::spread(key = nitrogen, value = Yield) %>%
#'   dplyr::ungroup() %>%
#'   dplyr::select(-Replicate, -variety)
#' 
#' 
#' ##----fm1.7.Plot1----
#' ggplot(data = Rice1, mapping = aes(x = nitrogen, y = Yield,  group = Replicate)) +
#'   geom_line() +
#'   facet_grid(variety ~ Replicate, labeller = label_both) +
#'   labs(
#'     x = "Nitrogen"
#'     , y = "Yield"
#'     , title = "Fig S1. Variety response to nitrogen for individual replicate blocks"
#'   ) +
#'   theme_bw() +
#'   theme(plot.title = element_text(hjust = 0.5)) 
#' 
#' 
#' 
#' ##----fm1.8----
#' riceV2V3 <- 
#'   rice %>%
#'   dplyr::filter(variety != "V1") %>% 
#'   droplevels()
#' 
#' fm1.8 <- aov(yield ~ Replicate*management * variety * nitrogen, riceV2V3)
#' fm1.8.ANOVA <- broom::tidy(fm1.8)
#' 
#' fm1.8.ANOVA
#' 
#' ##----fm1.9----
#' fm1.9 <- aov(yield ~ Replicate + management * variety * nitrogen +
#'                Error(Replicate/Main/Sub), riceV2V3)
#' fm1.9.ANOVA <- broom::tidy(fm1.9)
#' fm1.9.ANOVA
#' 
NULL