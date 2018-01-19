##' convert phylo to treedata
##'
##'
##' converting phylo object to treedata object
##' @title as.treedata
##' @rdname as.treedata
##' @param tree input tree, a \code{phylo} object
##' @param boot optional, can be bootstrap value from ape::boot.phylo
##' @param ... additional parameters
##' @importFrom methods new
##' @importFrom tidytree as.treedata
##' @method as.treedata phylo
##' @export
##' @author guangchuang yu
as.treedata.phylo <- function(tree, boot=NULL, ...) {
    ## boot is output from boot.phylo
    res <- new("treedata",
               phylo = tree
               )

    if (!is.null(boot)) {
        res@data <- data_frame(node=nodeIds(tree), bootstrap=boot)
    }
    return(res)
}

##' @method as.treedata phylo4
##' @export
as.treedata.phylo4 <- function(tree, ...) {
    new("treedata",
        phylo = as.phylo(tree))
}

##' @method as.treedata phylo4d
##' @export
as.treedata.phylo4d <- function(tree, ...) {
    d <- as_data_frame(tree@data)
    d$node <- as.numeric(rownames(tree@data))

    new("treedata",
        phylo = as.phylo.phylo4(tree),
        data = d
        )
}


##' @method as.treedata ggtree
##' @export
as.treedata.ggtree <- function(tree, ...) {
    as.treedata(tree$data, ...)
}
