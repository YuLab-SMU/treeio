#' @title read.treeqza
#' @param treeqza the qiime2 output file contained tree file.
#' @return phylo tree class
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
read.treeqza <- function(treeqza){
    tmpdir <- tempdir()
    unzipfiles <- utils::unzip(treeqza, exdir=tmpdir)
    metadafile <- unzipfiles[grep("metadata.yaml", unzipfiles)[1]]
    metaflag <- yaml::read_yaml(metadafile)
    formatflag <- metaflag$format
    if (formatflag!="NewickDirectoryFormat"){
        stop("Please check whether treeqza file contained tree file!")
    }else{
        datafile <- unzipfiles[grep("data/tree.nwk", unzipfiles)]
        x <- read.tree(datafile)
        return (x)
    }
}
