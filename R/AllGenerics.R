
##' access placement information
##'
##'
##' @title get.placements
##' @param tree tree object
##' @param ... additional parameters
##' @return placement tibble
##' @rdname get-placements
##' @export
get.placements <- function(tree, ...) {
    UseMethod("get.placements")
}


##' access tree text (newick text) from tree object
##'
##'
##' @docType methods
##' @name get.treetext
##' @rdname get.treetext-methods
##' @title get.treetext method
##' @param object treedata object
##' @param ... additional parameter
##' @return phylo object
##' @importFrom methods setGeneric
##' @export
setGeneric(
    name = "get.treetext",
    def = function(object, ...)
        standardGeneric("get.treetext")
)
