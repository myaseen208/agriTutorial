#' @name example3
#' @title  Example 3: Polynomial regression model with two quantitative level treatment factors
#' @description
#' (Gomez & Gomez, 1984, p. 401) report a two-factor nitrogen uptake greenhouse experiment on rice
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
#' @references
#' Faraway J (2002) Practical Regression and Anova using R. https://cran.r-project.org/doc/contrib/Faraway-PRA.pdf
#'
#' Gomez, K.A., & Gomez, A.A. (1984). Statistical procedures for agricultural research, 2nd edn. New York: Wiley.
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
#' ## Either type example("example3") to run ALL the examples succesively
#' ## or copy and paste examples sucessively, as required
#'
#' ## *************************************************************************************
#' ##                  Options and required packages
#' ## *************************************************************************************
#'
#' require(lmerTest)
#' require(lattice)
#' require(pbkrtest)
#' options(contrasts = c('contr.treatment', 'contr.poly'))
#'
#' ## *************************************************************************************
#' ##            Section 1: Polynomial powers of N and W
#' ## *************************************************************************************
#'
# '##  N and W rate raw polynomials for rice data
#' greenrice$loguptake = log(greenrice$uptake)
#' greenrice$Nitrogen = factor(greenrice$N)
#' greenrice$Water = factor(greenrice$W)
#' PolW = poly((greenrice$W/10), degree = 2, raw = TRUE)
#' colnames(PolW) = c("Linear_W", "Quadratic_W")
#' PolN = poly((greenrice$N/100), degree = 2, raw = TRUE)
#' colnames(PolN) = c("Linear_N", "Quadratic_N")
#' greenrice = cbind(greenrice, PolW, PolN)
#'
#' ## residual plot of untransformed N uptake data
#' greenrice.uptake = lmer(uptake ~ Replicate + factor(N) * factor(W) +
#'  (1|Replicate:Main), data = greenrice)
#' plot(greenrice.uptake, main = "Pearson residual plot for untransformed N uptake",
#'  ylab = "Residuals N uptake")
#'
#' ## residual plot of log transformed N uptake data
#' greenrice.loguptake = lmer(loguptake ~ Replicate + factor(N) * factor(W) +
#'  (1|Replicate:Main), data = greenrice)
#' plot(greenrice.loguptake, main = "Pearson residual plot for log transformed N uptake",
#'  ylab = "Residuals log N uptake")
#' \donttest{
#' ## Table 9: first-order model of log uptake with Wald tests
#' greenrice.lmer1 = lmer(loguptake ~ Linear_N + Linear_W + Nitrogen * Water +
#'  (1|Replicate) + (1|Replicate:Main), data = greenrice)
#' anova(greenrice.lmer1, ddf = "Kenward-Roger", type = 1)
#'
#' ## Table 10: second-order model of log uptake with Wald tests
#' greenrice.lmer2 = lmer(loguptake ~ Linear_N * Linear_W + Quadratic_N + Quadratic_W +
#'  Nitrogen * Water + (1|Replicate) + (1|Replicate:Main), data = greenrice)
#' anova(greenrice.lmer2, ddf = "Kenward-Roger", type = 1)
#' }
#' ## *************************************************************************************
#' ##         Section 2 : Fitted regression models and quadratic log uptake curves
#' ## *************************************************************************************
#' \donttest{
#' ## Regression coefficients of quadratic response model of W and N
#' greenrice.lmer0 = lmer(loguptake ~ Linear_N * Linear_W + Quadratic_N +
#'  Quadratic_W + (1|Replicate) + (1|Replicate:Main), data = greenrice)
#' summary(greenrice.lmer0, ddf = "Kenward-Roger", type = 1)
#' }
#'
#' ## Fig 4a fitted quadratic loguptake curve versus water stress treatments
#' panel.plot = function(x, y) {
#' panel.xyplot(x, y) # shows observed points
#' Nitrogen = c(0, .90, 1.80, 2.70)[panel.number()]
#' panel.curve(-1.16 + 0.17603 * x - 0.11599 * x * x + 0.68 * Nitrogen -
#'  0.0938 * Nitrogen * Nitrogen - 0.09072 * x * Nitrogen,
#' from = 0, to = 4.0, type = "l", lwd = 2)
#' }
#' xyplot(loguptake ~ Linear_W|factor(Linear_N), data = greenrice,
#'  scales = list(x = list(at = c(0, 1, 2, 4), labels = c(0, 10, 20, 40))),
#'  main = "Fig 4a: logN uptake versus water stress",
#'  xlab = " Water stress (days)", ylab = "Log nitrogen uptake (g/pot)",
#'  strip = strip.custom(strip.names = TRUE,
#'  factor.levels = c("0", "90", "180", "270")),
#' panel = panel.plot)
#'
#' ## Fig 4b fitted quadratic quadratic loguptake curve versus nitrogen rate treatments
#' panel.plot = function(x, y) {
#' panel.xyplot(x, y) # shows observed points
#' Water = c(0, 1.0, 2.0, 4.0)[panel.number()]
#' panel.curve( -1.16 + 0.17603 * Water - 0.11599 * Water * Water +
#'  0.68 * x - 0.0938 * x * x - 0.09072 * Water * x ,
#' from = 0, to = 2.70, type = "l", lwd = 2)
#' }
#' xyplot(loguptake ~ Linear_N|factor(Linear_W), data = greenrice,
#'  scales = list(x = list(at = c(0, .9, 1.8, 2.7), labels = c(0, 90, 180, 270))),
#'  main = "Fig 4b: logN uptake versus nitrogen rate",
#'  xlab = "Nitrogen (kg/ha)", ylab = "Log nitrogen uptake (g/pot)",
#'  strip = strip.custom(strip.names = TRUE,
#'  factor.levels = c("0", "10", "20", "40")),
#' panel = panel.plot)
#'
#'
#'
#' ## Fig 4a backtransformed quadratic loguptake curve versus water stress treatments
#' panel.plot = function(x, y) {
#' panel.xyplot(x, y) # shows observed points
#' Nitrogen = c(0, .90, 1.80, 2.70)[panel.number()]
#' panel.curve( exp(-1.16 + 0.17603 * x - 0.11599 * x * x + 0.68 * Nitrogen -
#'  0.0938 * Nitrogen * Nitrogen - 0.09072 * x * Nitrogen),
#' from = 0, to = 4.0, type = "l", lwd = 2)
#' }
#'
#' xyplot(uptake ~ Linear_W|factor(Linear_N), data = greenrice,
#'  scales = list(x = list(at = c(0, 1, 2, 4), labels = c(0, 10, 20, 40))),
#'  main = "Fig 4a: Back transformed N uptake versus water stress",
#'  xlab = " Water stress (days)", ylab = "Nitrogen uptake (g/pot)",
#'  strip = strip.custom(strip.names = TRUE,
#'  factor.levels = c("0", "90", "180", "270")),
#' panel = panel.plot)
#'
#' ## Fig 4b back transformed quadratic loguptake curve versus nitrogen rate treatments
#' panel.plot = function(x, y) {
#' panel.xyplot(x, y) # shows observed points
#' Water = c(0, 1.0, 2.0, 4.0)[panel.number()]
#' panel.curve(exp(-1.16 + 0.17603 * Water - 0.11599 * Water * Water +
#'  0.68 * x - 0.0938 * x * x - 0.09072 * Water * x),
#' from = 0, to = 2.70, type = "l", lwd = 2)
#' }
#' xyplot(uptake ~ Linear_N|factor(Linear_W), data = greenrice,
#'  scales = list(x = list(at = c(0, .9, 1.8, 2.7), labels = c(0, 90, 180, 270))),
#'  main = "Fig 4b: Back transformed N uptake versus nitrogen rate",
#'  xlab = "Nitrogen (kg/ha)", ylab = "Nitrogen uptake (g/pot)",
#'  strip = strip.custom(strip.names = TRUE,
#'  factor.levels = c("0", "10", "20", "40")),
#' panel = panel.plot)
#'
#'
#'
#' @importFrom lattice xyplot
#'
NULL
