##' @importFrom ape as.phylo
##' @method as.phylo treedata
##' @export
as.phylo.treedata <- function(x, ...) {
    return(x@phylo)
}

##' access phylo slot
##'
##'
##' @title get.tree
##' @param x tree object
##' @param ... additional parameters
##' @return phylo object
##' @export
##' @author guangchuang yu
get.tree <- function(x, ...) {
    as.phylo(x, ...)
}
