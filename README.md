<!-- README.md is generated from README.Rmd. Please edit that file -->

# treeio: Base classes and functions for phylogenetic tree input and output <a href="https://yulab-smu.top/treedata-book/"><img src="man/figures/logo.png" align="right" height="139" /></a>

[![](https://badges.ropensci.org/179_status.svg)](https://github.com/ropensci/onboarding/issues/179)
[![Bioc](http://www.bioconductor.org/shields/years-in-bioc/treeio.svg)](https://www.bioconductor.org/packages/devel/bioc/html/treeio.html#since)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![platform](http://www.bioconductor.org/shields/availability/devel/treeio.svg)](https://www.bioconductor.org/packages/devel/bioc/html/treeio.html#archives)
[![codecov](https://codecov.io/gh/GuangchuangYu/treeio/branch/master/graph/badge.svg)](https://codecov.io/gh/GuangchuangYu/treeio)

[![](https://img.shields.io/badge/release%20version-1.20.2-green.svg)](https://www.bioconductor.org/packages/treeio)
[![](https://img.shields.io/badge/devel%20version-1.21.2.001-green.svg)](https://github.com/guangchuangyu/treeio)
[![Linux Travis Build
Status](https://img.shields.io/travis/GuangchuangYu/treeio/master.svg?label=Linux)](https://travis-ci.org/GuangchuangYu/treeio)
[![AppVeyor Build
Status](https://img.shields.io/appveyor/ci/Guangchuangyu/treeio/master.svg?label=Windows)](https://ci.appveyor.com/project/GuangchuangYu/treeio)

[![](https://img.shields.io/badge/download-284494/total-blue.svg)](https://bioconductor.org/packages/stats/bioc/treeio)
[![](https://img.shields.io/badge/download-12470/month-blue.svg)](https://bioconductor.org/packages/stats/bioc/treeio)
[![download](http://www.bioconductor.org/shields/downloads/release/treeio.svg)](https://bioconductor.org/packages/stats/bioc/treeio)

‘treeio’ is an R package to make it easier to import and store
phylogenetic tree with associated data; and to link external data from
different sources to phylogeny. It also supports exporting phylogenetic
tree with heterogeneous associated data to a single tree file and can be
served as a platform for merging tree with associated data and
converting file formats.

Visit <https://yulab-smu.top/treedata-book/> for details.

[![Twitter](https://img.shields.io/twitter/url/http/shields.io.svg?style=social&logo=twitter)](https://twitter.com/intent/tweet?hashtags=treeio&url=http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12628/abstract&screen_name=guangchuangyu)
[![saythanks](https://img.shields.io/badge/say-thanks-ff69b4.svg)](https://saythanks.io/to/GuangchuangYu)
[![](https://img.shields.io/badge/follow%20me%20on-WeChat-green.svg)](https://yulab-smu.top/images/biobabble.jpg)

## :writing_hand: Authors

Guangchuang YU

School of Basic Medical Sciences, Southern Medical University

<https://yulab-smu.top>

If you use [treeio](http://bioconductor.org/packages/treeio) in
published research, please cite:

-   LG Wang, TTY Lam, S Xu, Z Dai, L Zhou, T Feng, P Guo, CW Dunn, BR
    Jones, T Bradley, H Zhu, Y Guan, Y Jiang, **G Yu**<sup>\*</sup>.
    treeio: an R package for phylogenetic tree input and output with
    richly annotated and associated data. ***Molecular Biology and
    Evolution***. 2020, 37(2):599-603. doi:
    [10.1093/molbev/msz240](http://dx.doi.org/10.1093/molbev/msz240).

## :arrow_double_down: Installation

Get the released version from Bioconductor:

``` r
## try http:// if https:// URLs are not supported
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
## BiocManager::install("BiocUpgrade") ## you may need this
BiocManager::install("treeio")
```

Or the development version from github:

``` r
## install.packages("devtools")
devtools::install_github("YuLab-SMU/treeio")
```

## :sparkling_heart: Contributing

We welcome any contributions! By participating in this project you agree
to abide by the terms outlined in the [Contributor Code of
Conduct](CONDUCT.md).

## :houses: Package Affiliations

The `treeio` package is a part of the Bioconductor and rOpenSci
projects.

| [![bioconductor_footer](http://bioconductor.org/images/logo_bioconductor.gif)](http://bioconductor.org) | [![ropensci_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org) |
|:-------------------------------------------------------------------------------------------------------:|:----------------------------------------------------------------------------------------------:|
