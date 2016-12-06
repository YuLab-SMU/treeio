##' @title as.treedata
##' @param tree tree object
##' @param ... additional parameter
##' @return treedata object
##' @rdname as.treedata
##' @export
as.treedata <- function(tree, ...) {
    UseMethod("as.treedata")
}

