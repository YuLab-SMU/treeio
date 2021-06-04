##' read MCMCTree output Tree
##'
##' @title read.mcmctree
##' @param file the output tree file of MCMCTree
##' @param as.ultrametric logical whether convert the tree 
##' to be ultrametric, if it is not ultrametric, default is FALSE.
##' When the tree is ultrametric, branch times will be calculated 
##' automatically. 
##' @param option integer it should be one of 1 or 2, default is 2. 
##' see also the 'option' of 'is.ultrametric' of 'ape'.
##' @param ... additional arguments, see also the arguments of 
##' 'is.ultrametric' of 'ape'.
##' @return treedata object
##' @export
##' @examples
##' file <- system.file("extdata/MCMCTree", "mcmctree_output.tree", package="treeio")
##' tr <- read.mcmctree(file, option=2)
##' tr
read.mcmctree <- function(file, as.ultrametric = FALSE, option=2, ...){
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
                                        as.ultrametric = as.ultrametric, 
                                        option=option, 
                                        ...)
    }else{
        for (i in seq_len(length(obj))){
            obj[[i]]@file <- filename(file)
            obj[[i]] <- add.branch.time.mcmctree(
                                        obj=obj[[i]], 
                                        as.ultrametric = as.ultrametric, 
                                        option=option, 
                                        ...)
        }
    }
    return(obj)
}

#' @importFrom ape branching.times
#' @importFrom ape is.ultrametric
add.branch.time.mcmctree <- function(obj, as.ultrametric=FALSE, ...){
    if (as.ultrametric && !is.ultrametric(obj@phylo, ...)){
        obj <- as.ultrametric(obj)
    }
    if (is.ultrametric(obj@phylo, ...)){
        xx <- branching.times(obj@phylo)
        ntip <- Ntip(obj)
        N <- Nnode(obj, internal.only = FALSE)
        xx <- data.frame(node=as.character((ntip+1):N), reltime=as.vector(xx))
        obj@data <- full_join(obj@data, xx, by="node") 
    }
    return(obj)
}
