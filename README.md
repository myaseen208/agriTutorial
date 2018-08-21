# agriTutorial: Tutorial Analysis of Some Agricultural Experiments

## Introduction

The agriTutorial package provides R software for the analysis of five agricultural example data sets as discussed in the paper:
*A tutorial on the statistical analysis of factorial experiments with qualitative and quantitative treatment factor levels* by Piepho and Edmondson (2018).
 
The example code produces statistical analysis for the five agricultural data sets in Piepho & Edmondson (2018) and also produces additional graphical analysis.
The data for each example is provided as a data frame which loads automatically whenever the package is loaded and the code for each analysis is provided as a set of examples.

Printed output defaults to the device terminal window but can be diverted to a suitable text file by using a sink file command: see help(sink), if required. Similarly, graphical output defaults
to the device graphics window but can be diverted to a suitable graphics device if required:
see vignette for further details,

The example code demonstrates some basic methodology for the analysis of data from designed experiments but there are other methods available in R and it is straightforward to extend the example code by adding functionality from other packages. 

### Polynomials

The polynomials used in this tutorial are either raw polynomials or orthogonal polynomials.

A raw polynomial is a weighted sum of the powers and cross-products
of a set of treatment or nuisance effect vectors.

An orthogonal polynomial is a weighted sum of orthogonal combinations of the powers and cross-products
of a set of treatment or nuisance effect vectors.

Raw polynomials have a direct interpretation as a fitted polynomial model but can be numerically unstable whereas orthogonal polynomials are numerically stable but give coefficients which are linear combinations of the required polynomial model coefficients and are difficult to interpret.

Raw polynomials are the polynomials of choice for most analyses but sometimes orthogonal polynomials can be useful when, for example, fitting higher-degree polynomials in a long series of repeated measures (see example 4).

### Functional marginality

Polynomial expansions are based on a Taylor series expansion and normally must include all polynomial terms up to and including the maximum degree of the expansion.
This is the property of functional marginality and applies to any polynomial or response surface model including models with polynomial interaction effects (Nelder, 2000). In this tutorial,
all polynomial and response surface models will be assumed to conform with the requirements of functional marginality.

### Packages

The example code depends on a number of R packages each of which must be installed on the user machine before the example code can be properly executed. The required packages are lmerTest, emmeans, pbkrtest, lattice, nlme and ggplot2, all of which should install automatically. If, for any reason, packages need to be installed by hand, this can be done by using install.packages("PackageName").

NB. It is important to keep packages updated using the update.packages() command.

## Examples

* **example1** : split-plot design with one quantitative and one qualitative treatment factor\cr

* **example2** : block design with one qualitative treatment factor\cr 

* **example3** : response surface design with two quantitative treatment factors\cr

* **example4** : repeated measures design with one quantitative treatment factor\cr

* **example5** : block design with transformed quantitative treatment levels\cr


## References
1. Piepho, H. P, and Edmondson. R. N. (2018). A tutorial on the statistical analysis of factorial experiments with qualitative and quantitative treatment factor levels. Journal of Agronomy and Crop Science. DOI: 10.1111/jac.12267.


2. Nelder, J. A. (2000). Functional marginality and response-surface fitting. Journal of Applied Statistics, 26, 109-122.



## Installation
Use **CRAN** to install the package:

```{r}
install.packages("agriTutorial")
```

## License
This package is free and open source software, licensed under GPL.
