<!-- README.md is generated from README.Rmd. Please edit that file -->
treeio: Base classes and functions for phylogenetic tree input and output
=========================================================================

<img src="https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/treeio/treeio.png" height="200" align="right" />

[![releaseVersion](https://img.shields.io/badge/release%20version-1.4.1-green.svg?style=flat)](https://bioconductor.org/packages/treeio) [![develVersion](https://img.shields.io/badge/devel%20version-1.5.2-green.svg?style=flat)](https://github.com/guangchuangyu/treeio) [![Bioc](http://www.bioconductor.org/shields/years-in-bioc/treeio.svg)](https://www.bioconductor.org/packages/devel/bioc/html/treeio.html#since) [![](https://badges.ropensci.org/179_status.svg)](https://github.com/ropensci/onboarding/issues/179)

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![codecov](https://codecov.io/gh/GuangchuangYu/treeio/branch/master/graph/badge.svg)](https://codecov.io/gh/GuangchuangYu/treeio) [![Last-changedate](https://img.shields.io/badge/last%20change-2018--07--19-green.svg)](https://github.com/GuangchuangYu/treeio/commits/master) [![GitHub forks](https://img.shields.io/github/forks/GuangchuangYu/treeio.svg)](https://github.com/GuangchuangYu/treeio/network) [![GitHub stars](https://img.shields.io/github/stars/GuangchuangYu/treeio.svg)](https://github.com/GuangchuangYu/treeio/stargazers)

[![platform](http://www.bioconductor.org/shields/availability/devel/treeio.svg)](https://www.bioconductor.org/packages/devel/bioc/html/treeio.html#archives) [![Build Status](http://www.bioconductor.org/shields/build/devel/bioc/treeio.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/treeio/) [![Linux Travis Build Status](https://img.shields.io/travis/GuangchuangYu/treeio/master.svg?label=Linux)](https://travis-ci.org/GuangchuangYu/treeio) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/Guangchuangyu/treeio/master.svg?label=Windows)](https://ci.appveyor.com/project/GuangchuangYu/treeio)

`treeio` is an R package to make it easier to import and store phylogenetic tree with associated data; and to link external data from different sources to phylogeny. It also supports exporting phylogenetic tree with heterogeneous associated data to a single tree file and can be served as a platform for merging tree with associated data and converting file formats.

See the `treeio` project website, <https://guangchuangyu.github.io/software/treeio>, and package vignettes for more details.

### Vignettes

-   [Importing trees with data](http://bioconductor.org/packages/devel/bioc/vignettes/treeio/inst/doc/Importer.html)
-   [Exporting trees with data](http://bioconductor.org/packages/devel/bioc/vignettes/treeio/inst/doc/Exporter.html)

[![Twitter](https://img.shields.io/twitter/url/http/shields.io.svg?style=social&logo=twitter)](https://twitter.com/intent/tweet?hashtags=treeio&url=http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12628/abstract&screen_name=guangchuangyu) [![saythanks](https://img.shields.io/badge/say-thanks-ff69b4.svg)](https://saythanks.io/to/GuangchuangYu) [![](https://img.shields.io/badge/follow%20me%20on-微信-green.svg?style=flat)](https://guangchuangyu.github.io/blog_images/biobabble.jpg) [![](https://img.shields.io/badge/打赏-支付宝/微信-green.svg?style=flat)](https://guangchuangyu.github.io/blog_images/pay_qrcode.png)

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
## install.packages("devtools")
devtools::install_github("GuangchuangYu/treeio")
```

Download stats
--------------

[![download](http://www.bioconductor.org/shields/downloads/treeio.svg)](https://bioconductor.org/packages/stats/bioc/treeio) [![total](https://img.shields.io/badge/downloads-12147/total-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/treeio) [![month](https://img.shields.io/badge/downloads-1148/month-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/treeio)

<img src="https://guangchuangyu.github.io/software/treeio/index_files/figure-html/unnamed-chunk-2-1.png" width="890"/>

Contributing
------------

We welcome any contributions! By participating in this project you agree to abide by the terms outlined in the [Contributor Code of Conduct](CONDUCT.md).

Package Affiliations
--------------------

The `treeio` package is a part of the Bioconductor and rOpenSci projects.

| [![bioconductor\_footer](http://bioconductor.org/images/logo_bioconductor.gif)](http://bioconductor.org) | [![ropensci\_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org) |
|:--------------------------------------------------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------:|
