##' read MCMCTree output Tree
##'
##' @title read.mcmctree
##' @param file the output tree file of MCMCTree
##' @param force.ultrametric logical whether convert the tree 
##' to be ultrametric, if it is not ultrametric, default is FALSE.
##' When the tree is ultrametric, branch times will be calculated 
##' automatically. 
##' @return treedata object
##' @details The node age is not reported by MCMCTree, it is derived from the
##' branch lengths and stored in the `reltime` column. The credibility interval
##' of a node age, `[&95%={lower, upper}]` in the file, is stored in the
##' `reltime_0.95_CI` column and can be plotted with
##' `geom_range(range='reltime_0.95_CI', center='reltime')`.
##' @export
##' @examples
##' file <- system.file("extdata/MCMCTree", "mcmctree_output.tree", package="treeio")
##' tr <- read.mcmctree(file)
##' tr
read.mcmctree <- function(file, force.ultrametric = FALSE){
    text <- readLines(file)
    ind <- grep("^.*tree.*=.*", text, ignore.case=TRUE)
    text[ind] <- gsub("^.*TREE", "TREE", text[ind], ignore.case=TRUE)
    text <- paste(text, collapse="\n")
    newfile <- tempfile()
    writeLines(text, newfile)
    obj <- read.beast(file=newfile)
    if(inherits(obj, "treedata")){
        obj@file <- filename(file)
        obj <- add.branch.time.mcmctree(
                                        obj=obj, 
                                        force.ultrametric = force.ultrametric, 
                                        )
    }else{
        for (i in seq_len(length(obj))){
            obj[[i]]@file <- filename(file)
            obj[[i]] <- add.branch.time.mcmctree(
                                        obj=obj[[i]], 
                                        force.ultrametric = force.ultrametric
                                        )
        }
    }
    return(obj)
}

#' @importFrom ape branching.times
#' @importFrom ape is.ultrametric
add.branch.time.mcmctree <- function(obj, force.ultrametric=FALSE, ...){
    obj <- rename.mcmctree.ci(obj)
    flag_ultrametric <- is.ultrametric(obj@phylo, option=2) || is.ultrametric(obj@phylo)
    if (force.ultrametric && flag_ultrametric){
        message("This tree is not ultrametric, and you has set force.ultrametric to TRUE, so the tree will be converted to ultrametric automatically!")
        obj <- as.ultrametric(obj)
    }
    if (flag_ultrametric){
        xx <- branching.times(obj@phylo)
        ntip <- Ntip(obj)
        N <- Nnode(obj, internal.only = FALSE)
        xx <- data.frame(node=as.character((ntip+1):N), reltime=as.vector(xx))
        obj@data <- full_join(obj@data, xx, by="node")
    }
    return(obj)
}

## MCMCTree reports the credibility interval of a node age as
## [&95%={lower, upper}] and read.beast() stores it in a column named '0.95',
## which says neither what it is nor what it belongs to. Naming it after the
## node time ('reltime', added above) makes it usable, e.g. with
## geom_range(range='reltime_0.95_CI', center='reltime'), #13
rename.mcmctree.ci <- function(obj) {
    i <- which(colnames(obj@data) == "0.95")
    if (length(i) == 1) {
        colnames(obj@data)[i] <- "reltime_0.95_CI"
    }
    return(obj)
}
