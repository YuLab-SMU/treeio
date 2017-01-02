##' convert phylo to treedata
##'
##'
##' converting phylo object to treedata object
##' @rdname as.treedata
##' @param boot optional, can be bootstrap value from ape::boot.phylo
##' @importFrom methods new
##' @method as.treedata phylo
##' @export
##' @author guangchuang yu
as.treedata.phylo <- function(tree, boot=NULL, ...) {
    ## boot is output from boot.phylo
    res <- new("treedata",
               phylo = tree
               )

    if (!is.null(boot)) {
        res@data = data.frame(node=nodeIds(tree), bootstrap=boot)
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
    new("treedata",
        phylo = as.phylo.phylo4(tree),
        data = data.frame(node=rownames(tree@data), tree@data)
        )
}

##' @method as.treedata ggtree
##' @export
as.treedata.ggtree <- function(tree, ...) {
    cn <- colnames(tree$data)
    idx <- cn[!cn %in% c("parent", "branch.length", "label", "isTip", "x", "y", "branch", "angle")]
    res <- new("treedata",
               phylo = as.phylo(tree))
    if (length(idx))
        res@data <- tree$data[, idx]
    return(res)
}
