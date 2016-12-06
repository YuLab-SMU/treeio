##' @importFrom ape as.phylo
##' @method as.phylo treedata
##' @export
as.phylo.treedata <- function(x, ...) {
    return(x@phylo)
}
