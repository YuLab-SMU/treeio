##' @title as.treedata
##' @param tree tree object
##' @param ... additional parameter
##' @return treedata object
##' @rdname as.treedata
##' @export
as.treedata <- function(tree, ...) {
    UseMethod("as.treedata")
}

##' @docType methods
##' @name get.tree
##' @rdname get.tree-methods
##' @title get.tree method
##' @param object one of \code{phylo}, \code{jplace}, \code{nhx}, \code{phangorn}, \code{beast}, \code{hyphy}, \code{codeml}, \code{codeml_mlc}, \code{paml_rst} object
##' @param ... additional parameter
##' @return phylo object
##' @export
setGeneric("get.tree", function(object, ...) standardGeneric("get.tree"))

##' @docType methods
##' @name get.treetext
##' @rdname get.treetext-methods
##' @title get.treetext method
##' @param object one of \code{phylo}, \code{jplace}, \code{beast}, \code{hyphy}, \code{codeml}, \code{codeml_mlc}, \code{paml_rst} object
##' @param ... additional parameter
##' @return phylo object
##' @export
setGeneric("get.treetext", function(object, ...) standardGeneric("get.treetext"))


##' @docType methods
##' @name get.treeinfo
##' @rdname get.treeinfo-methods
##' @title get.treeinfo method
##' @param object jplace object
##' @param layout layout
##' @param ladderize ladderize, logical
##' @param right logical, parameter for ladderize
##' @param ... additional parameter
##' @return data.frame
##' @export
setGeneric("get.treeinfo", function(object, layout="phylogram", ladderize=TRUE, right=FALSE, ...) standardGeneric("get.treeinfo"))


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
##' @name get.placements
##' @rdname get.placements-methods
##' @title get.placements method
##' @param object jplace object
##' @param by get best hit or others
##' @param ... additional parameter
##' @return data.frame
##' @export
setGeneric("get.placements", function(object, by, ...) standardGeneric("get.placements"))

##' @docType methods
##' @name get.subs
##' @rdname get.subs-methods
##' @title get.subs method
##' @param object paml_rst object
##' @param type one of 'marginal_subs', 'marginal_AA_subs',
##'                     'joint_subs' or 'joint_AA_subs'.
##' @param ... additional parameter
##' @return data.frame
##' @export
setGeneric("get.subs", function(object, type, ...) standardGeneric("get.subs"))


##' @docType methods
##' @name get.tipseq
##' @rdname get.tipseq-methods
##' @title get.tipseq method
##' @param object one of paml_rst or codeml object
##' @param ... additional parameter
##' @return character
##' @export
setGeneric("get.tipseq", function(object, ...) standardGeneric("get.tipseq"))

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
	def = function( object, tip, ... )
		{ standardGeneric("drop.tip") }
)
