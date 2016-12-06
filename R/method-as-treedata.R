##' convert phylo to treedata
##'
##'
##' converting phylo object to treedata object
##' @rdname as.treedata
##' @param boot optional, can be bootstrap value from ape::boot.phylo
##' @importFrom methods new
##' @export
##' @author guangchuang yu
as.treedata.phylo <- function(tree, boot=NULL, ...) {
    ## boot is output from boot.phylo
    res <- new("treedata",
               phylo = tree
               )

    if (!is.null(boot)) {
        res@data = data.frame(node=1:Nnode(tree, internal.only=FALSE), bootstrap=boot)
    }
    return(res)
}
