#' @title read.treeqza
#' @param treeqza the qiime2 output file contained tree file.
#' @param node.label parse node label as 'label' or 'support' value.
#' @param ... additional parameter, passed to 'read.tree'.
#' @return phylo tree object or treedata object when node.label was parsed 'support'.
#' @export 
#' @examples
#' qzafile1 <- system.file("extdata/qiime2treeqza", "fasttree-tree.qza", package="treeio")
#' qzafile2 <- system.file("extdata/qiime2treeqza", "iqt-tree.qza", package="treeio")
#' qzafile3 <- system.file("extdata/qiime2treeqza", "raxml-cat-tree.qza", package="treeio")
#' tr1 <- read.treeqza(qzafile1)
#' tr1
#' tr2 <- read.treeqza(qzafile2)
#' tr2
#' tr3 <- read.treeqza(qzafile3)
#' tr3
#' # parse node label as 'support' value.
#' qzafile4 <- system.file("extdata/qiime2treeqza", "raxml-cat-bootstrap-tree.qza", package="treeio")
#' tr4 <- read.treeqza(qzafile4, node.label="support")
#' tr4
read.treeqza <- function(treeqza, node.label = "label", ...){
    tmpdir <- tempdir()
    unzipfiles <- utils::unzip(treeqza, exdir=tmpdir)
    metadafile <- unzipfiles[grep("metadata.yaml", unzipfiles)[1]]
    metaflag <- yaml::read_yaml(metadafile)
    formatflag <- metaflag$format
    if (formatflag!="NewickDirectoryFormat"){
        stop("Please check whether treeqza file contained tree file!")
    }else{
        datafile <- unzipfiles[grep("data/tree.nwk", unzipfiles)]
        x <- read.newick(datafile, node.label=node.label, ...)
        return (x)
    }
}
