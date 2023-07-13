
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

##' whether the node is a tip
##'
##'
##' @title isTip
##' @param .data phylo, treedata or tbl_tree object
##' @param .node node number
##' @param ... additional parameters
##' @return logical value
##' @export
##' @author Guangchuang Yu
isTip <- function(.data, .node, ...) {
    UseMethod("isTip")
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



##' @docType methods
##' @name drop.tip
##' @rdname drop.tip-methods
##' @title drop.tip method
##' @param object A treedata or phylo object
##' @param tip a vector of mode numeric or character specifying the tips to delete
##' @param ... additional parameters
##' @return updated object
##' @export
setGeneric (
    name = "drop.tip",
    def = function( object, tip, ... )
        standardGeneric("drop.tip")
)

##' @rdname drop.tip-methods
##' @export
setGeneric(
    name = 'keep.tip',
    def = function(object, tip, ...)
        standardGeneric('keep.tip')
)
