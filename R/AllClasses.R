##' Class "jplace"
##' This class stores phylogenetic placements
##'
##'
##' @name jplace-class
##' @docType class
##' @slot phylo phylo object for tree structure
##' @slot treetext newick tree string
##' @slot data associated data
##' @slot extraInfo extra information, reserve for merge_tree
##' @slot file tree file
##' @slot placements reserve for jplace file to store placement information
##' @slot info extra information, e.g. metadata, software version etc.
##' @importClassesFrom tidytree treedata
##' @exportClass jplace
##' @author Guangchuang Yu \url{https://guangchuangyu.github.io}
##' @keywords classes
setClass("jplace",
         representation = representation(
             placements = "tbl_df"
         ),
         prototype = prototype(
             placements = tibble::tibble()
         ),
         contains = "treedata"
         )


##' @importFrom tidytree treedata
##' @export
tidytree::treedata

