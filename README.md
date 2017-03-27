<!-- README.md is generated from README.Rmd. Please edit that file -->
treeio: Base classes and functions for parsing and exporting phylogenetic tree
==============================================================================

<img src="https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/treeio/treeio.png" height="200" align="right" />

[![](https://img.shields.io/badge/release%20version-0.99.10-green.svg?style=flat)](https://bioconductor.org/packages/treeio) [![](https://img.shields.io/badge/devel%20version-0.99.11-green.svg?style=flat)](https://github.com/guangchuangyu/treeio) [![Bioc](http://www.bioconductor.org/shields/years-in-bioc/treeio.svg)](https://www.bioconductor.org/packages/devel/bioc/html/treeio.html#since) [![](https://img.shields.io/badge/download-80/total-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/treeio) [![](https://img.shields.io/badge/download-35/month-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/treeio)

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![codecov](https://codecov.io/gh/GuangchuangYu/treeio/branch/master/graph/badge.svg)](https://codecov.io/gh/GuangchuangYu/treeio) [![Last-changedate](https://img.shields.io/badge/last%20change-2017--03--21-green.svg)](https://github.com/GuangchuangYu/treeio/commits/master) [![GitHub forks](https://img.shields.io/github/forks/GuangchuangYu/treeio.svg)](https://github.com/GuangchuangYu/treeio/network) [![GitHub stars](https://img.shields.io/github/stars/GuangchuangYu/treeio.svg)](https://github.com/GuangchuangYu/treeio/stargazers)

[![platform](http://www.bioconductor.org/shields/availability/devel/treeio.svg)](https://www.bioconductor.org/packages/devel/bioc/html/treeio.html#archives) [![Build Status](http://www.bioconductor.org/shields/build/devel/bioc/treeio.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/treeio/) [![Linux/Mac Travis Build Status](https://img.shields.io/travis/GuangchuangYu/treeio/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/GuangchuangYu/treeio) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/Guangchuangyu/treeio/master.svg?label=Windows)](https://ci.appveyor.com/project/GuangchuangYu/treeio) [![Twitter](https://img.shields.io/twitter/url/https/github.com/GuangchuangYu/treeio.svg?style=social)](https://twitter.com/intent/tweet?hashtags=treeio&url=http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12628/abstract&screen_name=guangchuangyu)

------------------------------------------------------------------------

`treeio` was splited from `ggtree` package, please cite the following article when using `treeio`:

**G Yu**, DK Smith, H Zhu, Y Guan, TTY Lam<sup>\*</sup>. ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data. ***Methods in Ecology and Evolution***. 2017, 8(1):28-36.

[![](https://img.shields.io/badge/doi-10.1111/2041--210X.12628-green.svg?style=flat)](http://dx.doi.org/10.1111/2041-210X.12628) [![citation](https://img.shields.io/badge/cited%20by-11-green.svg?style=flat)](https://scholar.google.com.hk/scholar?oi=bibs&hl=en&cites=7268358477862164627) [![](https://img.shields.io/badge/Altmetric-348-green.svg?style=flat)](https://www.altmetric.com/details/10533079)

------------------------------------------------------------------------

For details, please visit our project website, <https://guangchuangyu.github.io/treeio>.

-   [Documentation](https://guangchuangyu.github.io/treeio/documentation/)
-   [FAQ](https://guangchuangyu.github.io/treeio/faq/)
-   [Featured Articles](https://guangchuangyu.github.io/treeio/featuredArticles/)
-   [Feedback](https://guangchuangyu.github.io/treeio/#feedback)

### Citation

[![citation](https://img.shields.io/badge/cited%20by-11-green.svg?style=flat)](https://scholar.google.com.hk/scholar?oi=bibs&hl=en&cites=7268358477862164627)

       +-------------+-------------+------------+-------------++
    10 +                                                      *+
       |                                                       |
     8 +                                                       +
       |                                                       |
       |                                                       |
     6 +                                                       +
       |                                                       |
     4 +                                                       +
       |                                                       |
       |                                                       |
     2 +                                                       +
       |                           *                           |
       +-------------+-------------+------------+-------------++
     2015         2015.5         2016        2016.5         2017

### Download stats

[![download](http://www.bioconductor.org/shields/downloads/treeio.svg)](https://bioconductor.org/packages/stats/bioc/treeio) [![](https://img.shields.io/badge/download-80/total-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/treeio) [![](https://img.shields.io/badge/download-35/month-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/treeio)
