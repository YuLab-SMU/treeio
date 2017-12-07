##' get associated data stored in treedata object
##'
##'
##' @title get_tree_data
##' @param tree_object a \code{treedata} object
##' @return tbl_df
##' @export
##' @author guangchuang yu
get_tree_data <- function(tree_object) {
    tree_anno <- tree_object@data
    extraInfo <- tree_object@extraInfo

    if (nrow(tree_anno) == 0) {
        return(extraInfo)
    }
    if (nrow(extraInfo) == 0) {
        return(tree_anno)
    }
    full_join(tree_anno, extraInfo, by = "node")
}

