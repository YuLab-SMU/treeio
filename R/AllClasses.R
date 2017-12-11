##' @importFrom methods setOldClass
setOldClass("phylo")
## setOldClass("multiPhylo")
setOldClass("DNAbin")
setOldClass("ggtree")

## @importFrom methods setClassUnion
## setClassUnion("phyloOrmultiPhylo", c("phylo", "multiPhylo"))

##' Class "treedata"
##' This class stores phylogenetic tree with associated data
##'
##'
##' @name treedata-class
##' @aliases treedata-class
##'   show,treedata-method
##' @docType class
##' @slot file tree file
##' @slot treetext newick tree string
##' @slot phylo phylo object for tree structure
##' @slot translation tip number to name translation in nexus file
##' @slot data associated data
##' @slot extraInfo extra information, reserve for merge_tree
##' @slot tip_seq tip sequences
##' @slot anc_seq ancestral sequences
##' @slot seq_type sequence type, one of NT or AA
##' @slot tipseq_file tip sequence file
##' @slot ancseq_file ancestral sequence file
##' @slot info extra information, e.g. metadata, software version etc.
##' @importFrom methods setClass
##' @importFrom methods representation
##' @importFrom ape as.DNAbin
##' @exportClass treedata
##' @author guangchuang yu \url{https://guangchuangyu.github.io}
##' @keywords classes
setClass("treedata",
         representation = representation(
             file        = "character",
             treetext    = "character",
             phylo       = "phylo",
             translation = "matrix",
             data        = "tbl_df",
             extraInfo   = "tbl_df",
             tip_seq     = "DNAbin",
             anc_seq     = "DNAbin",
             seq_type    = "character",
             tipseq_file = "character",
             ancseq_file = "character",
             info        = "list"
         ),
         prototype = prototype(
             data      = data_frame(),
             extraInfo = data_frame(),
             anc_seq = as.DNAbin(character(0)),
             tip_seq = as.DNAbin(character(0))
         )
         )


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
##' @slot translation tip number to name translation in nexus file
##' @slot placements reserve for jplace file to store placement information
##' @slot info extra information, e.g. metadata, software version etc.
##' @exportClass jplace
##' @author guangchuang yu \url{https://guangchuangyu.github.io}
##' @keywords classes
setClass("jplace",
         representation = representation(
             placements = "tbl_df"
         ),
         prototype = prototype(
             placements = data_frame()
         ),
         contains = "treedata"
         )




