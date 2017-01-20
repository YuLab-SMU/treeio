<!-- AddToAny BEGIN -->
<div class="a2a_kit a2a_kit_size_32 a2a_default_style">
<a class="a2a_dd" href="//www.addtoany.com/share"></a>
<a class="a2a_button_facebook"></a>
<a class="a2a_button_twitter"></a>
<a class="a2a_button_google_plus"></a>
<a class="a2a_button_pinterest"></a>
<a class="a2a_button_reddit"></a>
<a class="a2a_button_sina_weibo"></a>
<a class="a2a_button_wechat"></a>
<a class="a2a_button_douban"></a>
</div>
<script async src="//static.addtoany.com/menu/page.js"></script>
<!-- AddToAny END -->


<link rel="stylesheet" href="https://guangchuangyu.github.io/css/font-awesome.min.css">

[![releaseVersion](https://img.shields.io/badge/release%20version-0.99.9-blue.svg?style=flat)](https://bioconductor.org/packages/treeio)
[![develVersion](https://img.shields.io/badge/devel%20version-0.99.9-blue.svg?style=flat)](https://github.com/GuangchuangYu/treeio)
[![total](https://img.shields.io/badge/downloads-40/total-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/treeio)
[![month](https://img.shields.io/badge/downloads-13/month-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/treeio)



The `treeio` package defines classes and functions for parsing and
exporting phylogenetic trees.
*treeio* is released within the [Bioconductor](https://bioconductor.org/packages/treeio/) project and the source code is hosted on <a href="https://github.com/GuangchuangYu/treeio"><i class="fa fa-github fa-lg"></i> GitHub</a>.


## <i class="fa fa-user"></i> Authors

Guangchuang Yu and Tommy Tsan-Yuk Lam, School of Public Health, The University of Hong Kong.

## <i class="fa fa-book"></i> Citation

`treeio` was splitted from `ggtree` package, please cite the following article when using `treeio`:

[![doi](https://img.shields.io/badge/doi-10.1111/2041--210X.12628-blue.svg?style=flat)](http://dx.doi.org/10.1111/2041-210X.12628)
[![citation](https://img.shields.io/badge/cited%20by-1-blue.svg?style=flat)](https://scholar.google.com.hk/scholar?oi=bibs&hl=en&cites=7268358477862164627)
[![Altmetric](https://img.shields.io/badge/Altmetric-299-blue.svg?style=flat)](https://www.altmetric.com/details/10533079)

__G Yu__, DK Smith, H Zhu, Y Guan, TTY Lam<sup>\*</sup>. ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data. __*Methods in Ecology and Evolution*__. 2017, 8(1):28-36.

<!--
## <i class="fa fa-pencil"></i> Featured Articles

![](https://guangchuangyu.github.io/featured_img/ggtree/2015_peiyu_1-s2.0-S1567134815300721-gr1.jpg)

<i class="fa fa-hand-o-right"></i> Find out more on <i class="fa fa-pencil"></i> [Featured Articles](https://guangchuangyu.github.io/ggtree/featuredArticles/).

-->

## <i class="fa fa-download"></i> Installation

Install `treeio` is easy, follow the guide on the [Bioconductor page](https://bioconductor.org/packages/treeio/):

```r
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
## biocLite("BiocUpgrade") ## you may need this
biocLite("treeio")
```

<!-- If you have problems when installing some of the dependent packages, please refer to the [ggtree-installation](https://github.com/GuangchuangYu/ggtree/wiki/ggtree-installation) wiki page.
-->

## <i class="fa fa-cogs"></i> Overview

#### <i class="fa fa-angle-double-right"></i> Supported tree format:


+ Newick
+ NEXUS
+ New Hampshire eXtended format (NHX)
+ Jplace
+ Phylip

#### <i class="fa fa-angle-double-right"></i> Supported software output:

+ BEAST
+ EPA
+ HYPHY
+ MrBayes
+ PHYLODOG
+ phyloT
+ pplacer
+ r8s
+ RAxML
+ RevBayes

#### <i class="fa fa-angle-double-right"></i> Tree manipulation

+ `merge_tree` for merging evolutionary evidences inferred from
  different phylogenetic analyses

<i class="fa fa-hand-o-right"></i> Find out details and examples on <i class="fa fa-book"></i> [Documentation](https://guangchuangyu.github.io/treeio/documentation/).




## <i class="fa fa-code-fork"></i> Projects that depend on _treeio_


#### <i class="fa fa-angle-double-right"></i> Bioconductor packages
+ [LINC](https://www.bioconductor.org/packages/LINC): co-expression of lincRNAs and protein-coding genes
+ [philr](https://www.bioconductor.org/packages/philr): Phylogenetic partitioning based ILR transform for metagenomics data


## <i class="fa fa-comments"></i> Feedback

<ul class="fa-ul">
	<li><i class="fa-li fa fa-hand-o-right"></i> Please make sure you <a href="https://guangchuangyu.github.io/2016/07/how-to-bug-author/">follow the guide</a> before posting any issue/question</li>
	<li><i class="fa-li fa fa-bug"></i> For bugs or feature requests, please post to <i class="fa fa-github-alt"></i> <a href="https://github.com/GuangchuangYu/treeio/issues">github issue</a></li>
	<li><i class="fa-li fa fa-question"></i>  For user questions, please post to <i class="fa fa-google"></i> <a href="https://groups.google.com/forum/#!forum/bioc-treeio">google group</a></li>
	<li><i class="fa-li fa fa-support"></i> We are also following every post tagged with <strong>treeio</strong> on <a href="https://support.bioconductor.org">Bioconductor support site</a> and <a href="https://www.biostars.org">Biostars</a></li>
	<li><i class="fa-li fa fa-commenting"></i> Join the group chat on <a href="https://twitter.com/hashtag/treeio"><i class="fa fa-twitter fa-lg"></i></a> and <a href="http://huati.weibo.com/k/treeio"><i class="fa fa-weibo fa-lg"></i></a></li>
</ul>


