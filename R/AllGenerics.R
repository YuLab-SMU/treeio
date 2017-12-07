##' @title as.treedata
##' @param tree tree object
##' @param ... additional parameters
##' @return treedata object
##' @rdname as.treedata
##' @export
as.treedata <- function(tree, ...) {
    UseMethod("as.treedata")
}

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
##' @export
setGeneric("get.treetext", function(object, ...) standardGeneric("get.treetext"))


##' @docType methods
##' @name get.fields
##' @rdname get.fields-methods
##' @title get.fields method
##' @param object one of \code{jplace}, \code{beast}, \code{hyphy}, \code{codeml}, \code{codeml_mlc}, \code{paml_rst} object
##' @param ... additional parameter
##' @return available annotation variables
##' @export
setGeneric("get.fields", function(object, ...) standardGeneric("get.fields"))


##' @docType methods
##' @name drop.tip
##' @rdname drop.tip-methods
##' @title drop.tip method
##' @param object An nhx or phylo object
##' @param tip a vector of mode numeric or character specifying the tips to delete
##' @param ... additional parameters
##' @return updated object
##' @export
setGeneric (
	name = "drop.tip",
	def = function( object, tip, ... ) {
      standardGeneric("drop.tip")
  }
)


##' @docType methods
##' @name groupOTU
##' @rdname groupOTU-methods
##' @title groupOTU method
##' @param object supported objects, including phylo, paml_rst,
##'               codeml_mlc, codeml, jplace, beast, hyphy
##' @param focus a vector of tip (label or number) or a list of tips.
##' @param group_name name of the group, 'group' by default
##' @param ... additional parameter
##' @return group index
##' @export
setGeneric("groupOTU", function(object, focus, group_name="group", ...) standardGeneric("groupOTU"))

##' @docType methods
##' @name groupClade
##' @rdname groupClade-methods
##' @title groupClade method
##' @param object supported objects, including phylo, paml_rst,
##'               codeml_mlc, codeml, jplace, beast, hyphy
##' @param node a internal node or a vector of internal nodes
##' @param group_name name of the group, 'group' by default
##' @param ... additional parameter
##' @return group index
##' @export
setGeneric("groupClade", function(object, node, group_name="group", ...) standardGeneric("groupClade"))

