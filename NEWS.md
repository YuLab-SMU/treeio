# treeio 1.17.2

+ allow additional parameter to pass to `drop.tip` methods (2021-06-23, Wed, @xiangpin, #62)
+ `as.phylo` and `as.treedata` for `data.frame` (2021-06-12, Sat)
+ `as.ultrametric` method to force a tree to be ultrametric (2021-06-09, Wed)
+ introduce `force.ultrametric` parameter in `read.mcmctree` 

# treeio 1.17.1

+ `read.mcmctree` for PAML MCMCTree result (2021-06-04, Fri)

# treeio 1.16.0

+ Bioconductor 3.13 release

# treeio 1.15.6

+ optimized `read.nhx` for large tree file (2021-03-12, Fri)
- <https://github.com/YuLab-SMU/treeio/pull/51>

# treeio 1.15.5

+ `read.beast.newick` and `write.beast.newick` for importing and exporting newick text with metadata in BEAST style (2021-03-11, Thu)
  - <https://github.com/YuLab-SMU/treeio/pull/50>

# treeio 1.15.4

+ support parsing tree qza file from qiime2 (2020-03-01, Mon)
  - <https://github.com/YuLab-SMU/treeio/pull/46/files>

# treeio 1.15.3

+ support parsing phyloxml (2021-02-04, Thu)
  - <https://github.com/YuLab-SMU/treeio/pull/44>

# treeio 1.15.2

+ bug fixed of parsing nhx, now compatible with missing nhx tag (2020-11-19, Thu)
  - <https://github.com/YuLab-SMU/treeio/pull/40>

# treeio 1.15.1

+ remove magrittr::`%<>%` as it throw error of 'Error: could not find function "%>%<-"' (2020-11-19, Thu) 

# treeio 1.14.0

+ Bioconductor 3.12 release (2020-10-28, Wed)

# treeio 1.13.1

+ `as_tibble` for `pvclust` (2020-06-22, Mon)
+ `as.phylo` and `as.treedata` methods for `pvclust` object (2020-06-21, Sun)

# treeio 1.12.0

+ Bioconductor 3.11 release

# treeio 1.11.3

+ change according to dplyr (v=1.0.0) (2020-04-09, Thu)
  - remove mutate_, rename_, select_ and group_by_
+ remove data_frame for it was deprecated in tibble (v=3.0.0)

# treeio 1.11.2

+ update citation (2020-02-18, Tue)
+ phyloxml parser `read.phyloxml` (2019-12-05, Thu)
  
# treeio 1.11.1

+ support jplace version 1 (2019-11-25, Mon)
  - <https://github.com/YuLab-SMU/treeio/pull/25>
+ `offspring` return `integer(0)` instead of throw error if input `.node` is a tip (2019-11-21, Thu)

# treeio 1.10.0

+ Bioconductor 3.10 release

# treeio 1.9.3

+ add citation information (2019-10-05, Sta)
+ rename `phyPML` to `as.treedata.pml` (2019-10-01, Tue)
+ `as.phylo` method for `igraph` (only work with tree graph) (2019-09-28, Sat)

# treeio 1.9.2

+ `nodeid` and `nodelab` methods for converting between node number and labels (2019-08-09, Fri)
+ `parent`, 'ancestor`, `child`, `offspring` and `rootnode` methods for `treedata` (2019-08-07, Wed)
+ `read.mega_tabular` to parse MEGA Tabular output (2019-07-16, Tue)
+ `read.mega` to parse MEGA NEXUS (actually BEAST compatible)

# treeio 1.9.1

+ `rename_taxa` now use 1st column as key and 2nd column as value by default (2019-05-28, Tue)
+ enable `tree_subset` to specify `group_name` and enable to incorporate `root.edge` by setting `root_edge = TRUE` (2019-05-27, Mon)
+ `full_join` method for phylo object (2019-05-22, Wed)
+ redefined root method to wrape `ape::root.phylo` for compatibility (2019-05-20, Mon)
  - <https://github.com/GuangchuangYu/treeio/issues/18>
  
# treeio 1.8.0

+ Bioconductor 3.9 release

# treeio 1.7.4

+ update test according to the change of default RNG in the comming R-3.6 (2019-04-02, Tue)

# treeio 1.7.3

+ `rescale_tree` from `ggtree` (2019-01-11, Fri)

# treeio 1.7.2

+ `MRCA` methods for `phylo` and `treedata`  (2019-01-10, Thu)
+ mv vignettes to [treedata-book](https://yulab-smu.github.io/treedata-book/)
+ `root` method from `ggtree::reroot` (2018-12-28, Fri)
  - rename to `root` for importing `ape::root` generic

# treeio 1.7.1

+ compatible with `tibble` v=2.0.0 (2018-11-29, Thu)

# treeio 1.6.0

+ Bioconductor 3.8 release

# treeio 1.5.3

+ `read.jplace` compatible with output of [TIPars](https://github.com/id-bioinfo/TIPars) (2018-08-07, Tue)

# treeio 1.5.2

+ bug fixed of `as.phylo.ggtree` and `as.treedata.ggtree` (2018-07-19, Thu)
+ fixed R check for `tree_subset` by using `rlang::quo` and import `utils::head`
  and `utils::tail` (2018-05-24, Thu)
+ `tree_subset` methods contributed by [@tbradley1013](https://github.com/tbradley1013)
+ `drop.tip` works with `tree@extraInfo` (2018-05-23, Wed)
  - <https://github.com/GuangchuangYu/tidytree/pull/6#issuecomment-390259901>

# treeio 1.5.1

+ bug fixed of `groupOTU.treedata` (2018-05-23, Wed)
  - <https://github.com/GuangchuangYu/treeio/issues/7>

# treeio 1.4.0

+ Bioconductor 3.7 release

# treeio 1.3.15

+ Supports convert edge list (matrix, data.frame or tibble) to `phylo` and `treedata` object, now
  `ggtree` can be used to visualize all tree-like graph. (2018-04-23, Mon)

# treeio 1.3.14

+ rename_taxa (2018-04-19, Thu)
  - <https://guangchuangyu.github.io/2018/04/rename-phylogeny-tip-labels-in-treeio/>
+ read.astral (2018-04-17, Tue)
+ read.iqtree

# treeio 1.3.13

+ mv project website to <https://guangchuangyu.github.io/software/treeio>
+ update for rOpenSci acceptance
  - <https://github.com/ropensci/onboarding/issues/179#issuecomment-372127781>


# treeio 1.3.12

+ read.beast now compatible with taxa label contains ', " and space (2018-02-27,
  Wed)
+ update according to rOpenSci comments (2018-02-26, Mon)
  - <https://github.com/ropensci/onboarding/issues/179#issuecomment-365144565>
  - <https://github.com/ropensci/onboarding/issues/179#issuecomment-366800716>

# treeio 1.3.11

+ deprecate read.phyloT as read.tree in ape v5 now supports phyloT newick text
  <2018-01-11, Thu>
+ fixed goodpractice check <2018-01-10, Wed>
    - <https://github.com/ropensci/onboarding/issues/179#event-1416196637>
    - avoid using = for assignment
    - avoid code line > 80 characters
    - avoid sapply, instead using vapply and lapply
    - avoid using 1:length, 1:nrow and 1:ncol, use `seq_len` and `seq_along`
    - more unit tests

# treeio 1.3.10

* added 'Parsing jtree format' session in Importer vignette <2017-12-20, Wed>
* added 'Exporting tree data to JSON format' in Exporter vignette
* `read.jtree` and `write.jtree` functions
* added 'Combining tree with external data' and 'Merging tree data from
  different sources' sessions in Exporter vignette
* added 'Combining tree data' and 'Manipulating tree data using tidytree' sessions in Importer vignette
* full_join method for treedata object and added 'Linking external data to phylogeny' session in Importer vignette <2017-12-15, Fri>

# treeio 1.3.9

* move treedata class, show, get.fields methods to tidytree <2017-12-14, Thu>
* Exporter.Rmd vignette <2017-12-13, Wed>

# treeio 1.3.8

* mv treeio.Rmd vignette to Importer.Rmd and update the contents <2017-12-13, Wed>
* write.beast for treedata object <2017-12-12, Tue>
* add "connect" parameter in groupOTU <2017-12-12, Tue>
   + <https://groups.google.com/forum/#!msg/bioc-ggtree/Q4LnwoTf1DM/yEe95OFfCwAJ>

# treeio 1.3.7

* export groupClade.phylo method <2017-12-11, Mon>

# treeio 1.3.6

* re-defined groupOTU and groupClade generic using S3 <2017-12-11, Mon>

# treeio 1.3.5

* parent, ancestor, child, offspring, rootnode and sibling generic and method for phylo <2017-12-11, Mon>
* update mask and merge_tree function according to the treedata object <2017-12-11, Mon>

# treeio 1.3.4

* support tbl_tree object defined in tidytree <2017-12-08, Fri>

# treeio 1.3.3

* read.codeml output treedata, remove codeml class and clean up code <2017-12-07, Thu>

# treeio 1.3.2

* read.codeml_mlc output treedata object and remove codeml_mlc class <2017-12-06, Wed>
* read.paml_rst output treedata and remove paml_rst class <2017-12-06, Wed>
* read.phylip.tree and read.phylip.seq
* read.phylip output treedata object and phylip class definition was removed
* read.hyphy output treedata object; hyphy class definition was removed
* remove r8s class, read.r8s now output multiPhylo object
* jplace class inherits treedata <2017-12-05, Tue>
* using treedata object to store beast and mrbayes tree
* export read.mrbayes

# treeio 1.3.1

* compatible to parse beast output that only contains HPD range <2017-11-01, Wed>
   + https://groups.google.com/forum/?utm_medium=email&utm_source=footer#!msg/bioc-ggtree/RF2Ly52U_gc/jEP97nNPAwAJ

# treeio 1.2.0

* BioC 3.6 release <2017-11-01, Wed>

# treeio 1.1.2

* new project site using blogdown <2017-09-28, Thu>

# treeio 1.1.1

* parse mlc file without dNdS <2017-08-31, Thu>
   + https://groups.google.com/forum/?utm_medium=email&utm_source=footer#!topic/bioc-ggtree/hTRj-uldgAg
* better implementation of merge_tree <2017-08-31, Thu>

# treeio 0.99.11

* bug fixed in get.fields method for paml_rst <2017-03-20, Mon>
* fixed raxml2nwk for using treedata as output of read.raxml <2017-03-17, Fri>
* taxa_rename function <2017-03-15, Wed>
* phyPML method moved from ggtree <2017-03-06, Mon>

# treeio 0.99.10

* remove raxml class, now read.raxml output treedata object <2017-02-28, Tue>
* bug fixed of read.beast <2017-02-27, Mon>

# treeio 0.99.9

* read.newick for parsing node.label as support values <2017-01-03, Tue>
* read.beast support MrBayes output <2016-12-30, Fri>
* export as.phylo.ggtree <2016-12-30, Fri>

# treeio 0.99.8

* as.treedata.ggtree <2016-12-30, Fri>
* as.treedata.phylo4 & as.treedata.phylo4d <2016-12-28, Wed>

# treeio 0.99.7

* groupOTU, groupClade, gzoom methods from ggtree <2016-12-21, Wed>

# treeio 0.99.6

* add unit test of NHX (move from ggtree) <2016-12-14, Wed>

# treeio 0.99.3

* fixed BiocCheck by adding examples <2016-12-07, Wed>

# treeio 0.99.1

* fixed link in DESCRIPTION <2016-12-06, Tue>

# treeio 0.99.0

* add vignette <2016-12-06, Tue>
* move parser functions from ggtree <2016-12-06, Tue>

# treeio 0.0.1

* `read.nhx` from ggtree <2016-12-06, Tue>
* `as.phylo.treedata` to access `phylo` from `treedata` object <2016-12-06, Tue>
* `as.treedata.phylo` to convert `phylo` to `treedata` object <2016-12-06, Tue>
* `treedata` class definition <2016-12-06, Tue>
