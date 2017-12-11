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
##' @param object \code{treedata} object
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

##' grouping OTUs
##'
##'
##' @title groupOTU
##' @param .data tree object (phylo, treedata, tbl_tree, ggtree etc.)
##' @param .node selected nodes
##' @param ... additional parameter
##' @return updated tree with group information or group index
##' @author guangchuang yu
##' @export
groupOTU <- function(.data, .node, ...) {
    UseMethod("groupOTU")
}

##' grouping clades
##'
##'
##' @title groupClade
##' @inheritParams groupOTU
##' @return updated tree with group information or group index
##' @author Guangchuang Yu
##' @export
groupClade <- function(.data, .node, ...) {
    UseMethod("groupClade")
}

##' access child data
##'
##'
##' @title child
##' @rdname child
##' @param .data phylo or tbl_tree object
##' @param ... additional parameters
##' @return child data
##' @export
##' @author guangchuang yu
child <- function(.data, ...) {
    UseMethod("child")
}

##' access offspring data
##'
##'
##' @title offspring
##' @rdname offspring
##' @inheritParams child
##' @return offspring data
##' @export
##' @author guangchuang yu
offspring <- function(.data, ...) {
    UseMethod("offspring")
}


##' access parent data
##'
##'
##' @title parent
##' @rdname parent
##' @inheritParams child
##' @return parent data
##' @export
##' @author guangchuang yu
parent <- function(.data, ...) {
    UseMethod("parent")
}


##' access ancestor data
##'
##'
##' @title ancestor
##' @rdname ancestor
##' @inheritParams child
##' @return ancestor data
##' @export
##' @author guangchuang yu
ancestor <- function(.data, ...) {
    UseMethod("ancestor")
}

##' access most recent common ancestor data
##'
##'
##' @title mrca
##' @rdname mrca
##' @inheritParams child
##' @return mrca data
##' @export
##' @author guangchuang yu
mrca <- function(.data, ...) {
    UseMethod("mrca")
}


##' access root node data
##'
##'
##' @title rootnode
##' @rdname rootnode
##' @inheritParams child
##' @return root node data
##' @export
##' @author guangchuang yu
rootnode <- function(.data, ...) {
    UseMethod("rootnode")
}

##' access sibling data
##'
##'
##' @title sibling
##' @rdname sibling
##' @inheritParams child
##' @return sibling
##' @export
##' @author guangchuang yu
sibling <- function(.data, ...) {
    UseMethod("sibling")
}

