#' @name example1
#' @title  Example 1: Split-plot design with one qualitative and one quantitative level factor
#' @description
#' Gomez & Gomez (1984, p. 143) report a rice experiment with three management practices
#' (minimum, optimum, intensive), five different amounts of nitrogen (N) fertilizer
#' (0, 50, 80, 110, 140 kg/ha), and three varieties (V1, V2, V3). The experiment
#' involved variety and management as qualitative treatment factors and nitrogen
#' fertilizer as a quantitative treatment factor. Overall, there were 45 treatments with
#' three replicates in complete replicate blocks. The fertilizer treatments were applied
#'  to main plots, the management practices to split-plots and the varieties to split-split-plots.
#'
#' @details
#'
#' Section 1
#'
#' Section 1 examines treatment effects by fitting qualitative factorial models and the first analysis
#' calculates a full analysis of variance (Table 1) for main plots (nitrogen),
#' split-plots (management) and split-split-plots (variety). Each type of experimental unit (or "stratum")
#' requires a separate error term in the fitted analysis.
#'
#' The second analysis (Table 2) uses a REML mixed model analysis to find treatment means and SE's for each marginal
#' treatment classification averaged over all the other treatment factors, together with estimates of
#' pairwise contrasts of treatment means and the SE's of the pairwise treatment comparisons. This analysis fits
#' the full set of nitrogen-by-variety interaction effects assuming additive management effects and the fit of the model
#' is tested by a graphical plot of the model residuals.
#' Residual plots provide an important check on model assumptions but many more options for model testing are
#' available and further methods for diagnostic testing are examined in the subsequent examples.
#'
#' The third analysis (Table 3) shows a mixed model analysis of the full factorial model fitted by REML using the
#' lmer function of the lme4 package. Generally with mixed models, determination of the denominator degrees of
#' freedom for Wald-type F- and t-statistics becomes an issue, and here we use the method proposed by
#' Kenward & Roger (1997).
#'
#' Section 2
#'
#' Section 2 examines treatment effects by fitting polynomial models and the first step calculates
#' a full set of four raw polynomials for the 5-levels of N using the poly() function.
#' The N rates are re-scaled by division by 100 to improve numerical stability.
#'
#' The second step fits a mixed model polynomial analysis of nitrogen effects assuming additive management effects (Table 7).
#' In this analysis, most of the nitrogen treatment effect can be explained by linear and quadratic trend effects.
#' but it is important to note that there is a non-negligible Variety x Cubic N interaction effect. This suggests
#' that not all the varieties responded in a similar way to the N treatments and that some further analysis of the
#' data may be required (see also the N plots of individual varieties and replicates in Fig 1).
#'
#' The third step fits the required model for the actual fitted model coefficients (Table 8).
#' When estimating model effects, only effects that are significant for the fitted model or that are marginal to
#' those effects (functional marginality) should be included in the model therefore only linear and quadratic nitrogen effects
#' are included in this model. The fitted model for the nitrogen effects fits the actual actual nitrogen levels used
#' in the experiment therefore this model provides the required coefficients for the actual applied nitrogen levels.
#'
#' Section 3
#'
#' Section 3 provides checks on some of the assumptions underlying the blocks-by-treatments model.
#'
#' The first analysis in this section shows a
#' complete partition of the blocks-by-treatments interaction effects into factorial mean square terms where
#' all the terms that contain a replicate:variety interaction effect are estimates of the split-split-plot error variance.
#' If the blocks-by-treatments assumptions are valid, all the estimates of the split-split-plot error variance
#' are expected to have
#' the same error mean square. However, the Replicate:variety effect has a mean square of 1.54 on 4 degrees
#' of freedom whereas the Replicate:management:variety:nitrogen effect has a mean square of 0.26 on 32 degrees of freedom.
#' The ratio of these mean squares is 5.92 with an F-probability of 0.00110 on 4 and 32 degrees of freedom, which
#' means that the Replicate:variety interaction effect is significantly inflated relative to the
#' Replicate:management:variety:nitrogen effect. This shows that the assumptions underlying the blocks-by-treatments
#' analysis of the model are invalid with a high level of probability.
#'
#' The 4 degrees of freedom in the Replicate:variety interaction effect are the differences between the three varieties
#' differenced between the three replicate blocks. Fig S1 shows graphical plots of
#' variety effects in each replicate block averaged over management effects, and there is clear evidence that the effects of
#' Variety 1 in blocks 1 and 2 were different from the effects of Variety 1 in block 3.
#'
#' The second analysis in Section 3 shows a complete partition of the blocks-by-treatments
#' interaction effects into factorial mean square terms ignoring Variety 1. This
#' analysis shows a reasonably good fit to the assumed additive block which
#' supports the hypothesis that the non-additivity of the block-and-treatment effects in the
#' full unrestricted analysis is mainly due to Variety 1.
#'
#' The final analysis in Section 3 shows an analysis of variance of the treatment effects ignoring Variety 1.
#' In this analysis, the management:variety interaction effect becomes significant at the 0.00992 probability level
#' compared with a non-significant management:variety interaction effect in the analysis of the full data set.
#'
#' Such anomalies are not uncommon in the analysis of real data sets and it is the task of
#' the statistician to identify anomalies as and when they occur. Factorial designs can be very powerful for practical
#' research but, as demonstrated with this data set, the analysis of such designs is complex and anomalies
#' can be easily missed.
#' Unless an anomaly is due to an easily identified cause such as an incorrectly recorded data point, it is likely
#' that the anomaly will need to be investigated by further discussion with the research workers.
#' It is a mistake to suppose that data from a designed experiment can be analysed statistically in isolation from
#' the research workers who conducted the experiment.
#'
#' \code{\link[agriTutorial]{agriTutorial}}: return to home page if you want to select a different example \cr
#'
#' @references
#' Gomez, K.A., & Gomez, A.A. (1984). Statistical procedures for agricultural
#' research, 2nd edn. New York: Wiley.
#'
#' Kenward, M.G., & Roger, J.H. (1997). Small sample inference for fixed effects from restricted maximum likelihood.
#' Biometrics, 53, 983–997.
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
#' ## Either type example("example1") to run ALL the examples succesively
#' ## or copy and paste examples sucessively, as required
#'
#' ## *************************************************************************************
#' ##                       Options and required packages
#' ## *************************************************************************************
#'
#' ## Packages lmerTest, emmeans and pbkrtest MUST be installed
#' require(lmerTest)
#' require(emmeans)
#' require(pbkrtest)
#' options(contrasts = c('contr.treatment', 'contr.poly'))
#'
#' ## *************************************************************************************
#' ##            Section 1: Qualitative analysis of factorial treatment effects
#' ## *************************************************************************************
#'
#' ## Table 1 Full analysis of rice data assuming qualitative nitrogen effects
#' rice.aov1 = aov(yield ~ Replicate + management * variety * nitrogen +
#' Error(Replicate/Main/Sub), rice)
#' summary(rice.aov1, ddf = "Kenward-Roger", type = 1)
#' \donttest{
#' ## Table 2 REML means and se's for additive management and qualitative nitrogen effects
#' rice.means = lmer(yield ~ Replicate + management + nitrogen * variety +
#'  (1|Replicate:Main) + (1|Replicate:Main:Sub), data = rice)
#' anova(rice.means, ddf = "Kenward-Roger", type = 1)
#' plot(rice.means, sub.caption = NA, ylab = "Residuals", xlab = "Fitted",
#'  main = "Full analysis with full nitrogen effects")
#' emmeans::emmeans(rice.means, ~ nitrogen)
#' emmeans::emmeans(rice.means, ~ variety)
#' emmeans::emmeans(rice.means, ~ nitrogen * variety)
#'
#' ## REML contrasts and sed's for additive management and qualitative nitrogen effects
#' n.v = emmeans::emmeans(rice.means, ~ nitrogen|variety)
#' emmeans::contrast(n.v, alpha = 0.05, method = "pairwise")
#' v.n = emmeans::emmeans(rice.means, ~ variety|nitrogen)
#' emmeans::contrast(v.n, alpha = 0.05, method = "pairwise")
#'
#' ## Table 3 Mixed model effects for rice data with significance tests
#' rice.lmer = lmer(yield ~ Replicate + nitrogen * management * variety + (1|Replicate:Main) +
#'  (1|Replicate:Main:Sub), data = rice)
#' anova(rice.lmer, ddf = "Kenward-Roger", type = 1)
#' }
#'
#' ## *************************************************************************************
#' ##            Section 2: Quantitative analysis of factorial treatment effects
#' ## *************************************************************************************
#'
#' ## adds raw N polynomials to data frame: note that the nrate is re-scaled
#' N = poly((rice$nrate/100), 4, raw = TRUE)
#' colnames(N) = c("Linear_N", "Quadratic_N", "Cubic_N", "Quartic_N")
#' rice = cbind(rice, N)
#' \donttest{
#' ## Table 7: Mixed model fitting raw polynomials for nitrogen effects
#' rice.fullN = lmer(yield ~ Replicate + management + variety * (Linear_N + Quadratic_N +
#'  Cubic_N + Quartic_N) + (1|Replicate:Main) + (1|Replicate:Main:Sub), data = rice)
#' anova(rice.fullN, ddf = "Kenward-Roger", type = 1)
#'
#' ## Table 8 Coefficients for separate linear and common quadratic N with additive management
#' rice.quadN = lmer(yield ~ Replicate + management + variety * Linear_N + Quadratic_N +
#'  (1|Replicate:Main) + (1|Replicate:Main:Sub), data = rice)
#' summary(rice.quadN, ddf = "Kenward-Roger")
#' }
#'
#' ## *************************************************************************************
#' ##                       Section 3: Model assumptions
#' ## *************************************************************************************
#'
#' ## Full analysis of variance of block and treatment effects showing large mean square error
#' ## due to variety-by-replicates interaction effects
#' rice.fullaov = aov(yield ~ Replicate*management * variety * nitrogen, rice)
#' summary(rice.fullaov, ddf = "Kenward-Roger", type = 1)
#'
#' ## Fig S1 Nitrogen response per variety per plot showing anomalous behaviour of Variety 1
#' ## in Blocks 1 and 2 compared with Block 3
#' Rice = aggregate(rice$yield, by = list(rice$Replicate, rice$nitrogen, rice$variety),
#'  FUN = mean, na.rm = TRUE)
#' colnames(Rice) = c("Reps", "Nlev", "Vars", "Yield")
#' wideRice = reshape(Rice, timevar = "Nlev", idvar = c("Vars", "Reps"), direction = "wide")
#' wideRice = wideRice[,-c(1, 2)]
#' N = c(0, 50, 80, 110, 140)
#' par(mfrow = c(3, 3), oma = c(0, 0, 2, 0))
#' for (i in 1:3) {
#'	for (j in 1:3) {
#'		plot(N, wideRice[(i - 1) * 3 + j, ], type = "l", ylab = "yield",
#'		main = paste("Variety",i,"Block",j), ylim = c(0, max(wideRice)))
#'		}
#'	}
#' title(main = "Fig S1. Variety response to nitrogen for individual replicate blocks", outer = TRUE)
#'
#' ## Subset of data excluding variety 1
#' riceV2V3=droplevels(rice[rice$variety != "V1",])
#'
#' ## Restricted analysis of variance of block and treatment effects excluding variety 1
#' ## compare variety-by-replicates interaction effects of full and restricted analysis
#' rice.fullaov = aov(yield ~ Replicate*management * variety * nitrogen, riceV2V3)
#' summary(rice.fullaov, ddf = "Kenward-Roger", type = 1)
#'
#' ## Restricted analysis assuming qualitative nitrogen effects excluding variety 1
#' rice.aov1 = aov(yield ~ Replicate + management * variety * nitrogen +
#' Error(Replicate/Main/Sub), riceV2V3)
#' summary(rice.aov1, ddf = "Kenward-Roger", type = 1)
#'
#' @importFrom lmerTest lmer
#' @importFrom pbkrtest PBmodcomp
#' @importFrom emmeans emmeans
#' @importFrom emmeans contrast
#'
#'
NULL
