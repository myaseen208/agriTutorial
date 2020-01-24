
## `agriTutorial`: Tutorial Analysis of Some Agricultural Experiments

###### Version : [1.0.0](https://myaseen208.github.io/agriTutorial/); Copyright (C) 2019-2020:; License: [GPL-2|GPL-3](https://www.r-project.org/Licenses/)

##### *Rodney Edmondson*<sup>1</sup>, *Hans-Peter Piepho*<sup>2</sup> and *Muhammad Yaseen*<sup>3</sup>

1.  Rana House, Wellesbourne, UK (<rodney.edmondson@gmail.com>)
2.  Biostatistics Unit, Institute of Crop Science, University of
    Hohenheim, Stuttgart, Germany (<piepho@uni-hohenheim.de>)
3.  Department of Mathematics and Statistics, University of Agriculture
    Faisalabad, Pakistan (<myaseen208@gmail.com>).

-----

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-last-release/agriTutorial)](https://cran.r-project.org/package=agriTutorial)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/agriTutorial?color=green)](https://CRAN.R-project.org/package=agriTutorial)
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.3.3-orange.svg)](https://github.com/myaseen208/agriTutorial) -->

[![develVersion](https://img.shields.io/badge/devel%20version-1.0.0-orange.svg)](https://github.com/myaseen208/agriTutorial)

<!-- [![GitHub Download Count](https://github-basic-badges.herokuapp.com/downloads/myaseen208/agriTutorial/total.svg)] -->

[![Project Status:
WIP](http://www.repostatus.org/badges/latest/inactive.svg)](http://www.repostatus.org/#inactive)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--01--24-yellowgreen.svg)](https://github.com/myaseen208/agriTutorial)
[![Rdoc](http://www.rdocumentation.org/badges/version/agriTutorial)](http://www.rdocumentation.org/packages/agriTutorial)
[![Analytics](https://pro-pulsar-193905.appspot.com/UA-116716530-1/welcome-page)](https://github.com/myaseen208/google-analytics-beacon)

-----

## Description

Example software for the analysis of data from designed experiments,
especially agricultural crop experiments. The basics of the analysis of
designed experiments are discussed using real examples from agricultural
field trials. A range of statistical methods using a range of R
statistical packages are exemplified . The experimental data is made
available as separate data sets for each example and the R analysis code
is made available as example code. The example code can be readily
extended, as required.

## Installation

The package can be installed from CRAN as follows:

``` r
install.packages("agriTutorial", dependencies = TRUE)
```

The development version can be installed from github as follows:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("myaseen208/agriTutorial")
```

## Detailed tutorial

For a detailed tutorial (vignette) on how to used this package type:

``` r
browseVignettes(package = "agriTutorial")
```

The vignette for the latest version is also available
[online](https://myaseen208.github.io/agriTutorial/articles/IntroagriTutorial.pdf).

## What’s new

To know whats new in this version type:

``` r
news(package = "agriTutorial")
```

## Links

[CRAN page](https://cran.r-project.org/package=agriTutorial)

[Github page](https://github.com/myaseen208/agriTutorial)

[Documentation website](https://myaseen208.github.io/agriTutorial/)

## Citing `agriTutorial`

To cite the methods in the package use:

``` r
citation("agriTutorial")
```

``` 

To cite the R package 'agriTutorial' in publications use:

  Rodney Edmondson, Hans-Peter Piepho, and Muhammad Yaseen (2020).
  agriTutorial: Tutorial Analysis of Some Agricultural Experiments.R
  package version 1.0.0 ,
  https://myaseen208.github.io/agriTutorial/https://cran.r-project.org/package=agriTutorial.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {agriTutorial: Detecting Structural Change with Heteroskedasticity},
    author = {{Rodney Edmondson} and {Hans-Peter Piepho} and {Muhammad Yaseen}},
    year = {2020},
    note = {R package version 1.0.0},
    note = {https://myaseen208.github.io/agriTutorial/ },
    note = {https://cran.r-project.org/package=agriTutorial},
  }

This free and open-source software implements academic research by the
authors and co-workers. If you use it, please support the project by
citing the package.
```
