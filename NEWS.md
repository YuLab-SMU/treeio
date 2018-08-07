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

* read.nhx from ggtree <2016-12-06, Tue>
* as.phylo.treedata to access phylo from treedata object <2016-12-06, Tue>
* as.treedata.phylo to convert phylo to tree data object <2016-12-06, Tue>
* treedata class definition <2016-12-06, Tue>
