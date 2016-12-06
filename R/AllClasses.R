##' @importFrom methods setOldClass
setOldClass("phylo")
setOldClass("ggtree")

##' Class "treedata"
##' This class stores phylogenetic tree with associated data
##'
##'
##' @name treedata-class
##' @docType class
##' @slot phylo phylo object for tree structure
##' @slot treetext newick tree string
##' @slot data associated data
##' @slot extraInfo extra information, reserve for merge_tree
##' @slot file tree file
##' @importFrom methods setClass
##' @exportClass treedata
##' @author guangchuang yu \url{https://guangchuangyu.github.io}
##' @keywords classes
setClass("treedata",
         representation = representation(
             phylo = "phylo",
             treetext = "character",
             data = "data.frame",
             extraInfo = "data.frame",
             file = "character"
         )
         )

