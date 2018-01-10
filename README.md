<!-- README.md is generated from README.Rmd. Please edit that file -->
treeio: Base classes and functions for phylogenetic tree input and output
=========================================================================

<img src="https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/treeio/treeio.png" height="200" align="right" />

[![releaseVersion](https://img.shields.io/badge/release%20version-1.2.1-green.svg?style=flat)](https://bioconductor.org/packages/treeio) [![develVersion](https://img.shields.io/badge/devel%20version-1.3.11-green.svg?style=flat)](https://github.com/guangchuangyu/treeio) [![Bioc](http://www.bioconductor.org/shields/years-in-bioc/treeio.svg)](https://www.bioconductor.org/packages/devel/bioc/html/treeio.html#since) [![](https://badges.ropensci.org/179_status.svg)](https://github.com/ropensci/onboarding/issues/179)

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![codecov](https://codecov.io/gh/GuangchuangYu/treeio/branch/master/graph/badge.svg)](https://codecov.io/gh/GuangchuangYu/treeio) [![Last-changedate](https://img.shields.io/badge/last%20change-2018--01--10-green.svg)](https://github.com/GuangchuangYu/treeio/commits/master) [![GitHub forks](https://img.shields.io/github/forks/GuangchuangYu/treeio.svg)](https://github.com/GuangchuangYu/treeio/network) [![GitHub stars](https://img.shields.io/github/stars/GuangchuangYu/treeio.svg)](https://github.com/GuangchuangYu/treeio/stargazers)

[![platform](http://www.bioconductor.org/shields/availability/devel/treeio.svg)](https://www.bioconductor.org/packages/devel/bioc/html/treeio.html#archives) [![Build Status](http://www.bioconductor.org/shields/build/devel/bioc/treeio.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/treeio/) [![Linux/Mac Travis Build Status](https://img.shields.io/travis/GuangchuangYu/treeio/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/GuangchuangYu/treeio) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/Guangchuangyu/treeio/master.svg?label=Windows)](https://ci.appveyor.com/project/GuangchuangYu/treeio) [![Twitter](https://img.shields.io/twitter/url/https/github.com/GuangchuangYu/treeio.svg?style=social)](https://twitter.com/intent/tweet?hashtags=treeio&url=http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12628/abstract&screen_name=guangchuangyu)

Base classes and functions for parsing and exporting phylogenetic trees. 'treeio' supports parsing analysis findings from commonly used software packages, allows linking exteranl data to phylogeny and merging tree data obtained from different sources. It also supports exporting phylogenetic tree with heterogeneous associated data to a single tree file.

Authors
-------

Guangchuang YU <https://guangchuangyu.github.io>

School of Public Health, The University of Hong Kong

Installation
------------

Get the released version from Bioconductor:

``` r
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
## biocLite("BiocUpgrade") ## you may need this
biocLite("treeio")
```

Or the development version from github:

``` r
setRepositories(ind=1:2)
## install.packages("devtools")
devtools::install_github("GuangchuangYu/treeio")
```

------------------------------------------------------------------------

For details, please visit our project website, <https://guangchuangyu.github.io/treeio>.

-   [Documentation](https://guangchuangyu.github.io/treeio/documentation/)
-   [Feedback](https://guangchuangyu.github.io/treeio/#feedback)

### Download stats

[![download](http://www.bioconductor.org/shields/downloads/treeio.svg)](https://bioconductor.org/packages/stats/bioc/treeio) [![total](https://img.shields.io/badge/downloads-5283/total-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/treeio) [![month](https://img.shields.io/badge/downloads-691/month-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/treeio)

<img src="docs/images/dlstats.png" width="890"/>
