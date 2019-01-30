##' @method isTip tbl_tree
##' @rdname isTip
##' @export
isTip.tbl_tree <- function(.data, .node, ...) {
    tips <- .data$node[!.data$node %in% .data$parent]
    return(.node %in% tips)
}

##' @method isTip phylo
##' @rdname isTip
##' @export
isTip.phylo <- function(.data, .node, ...) {
    isTip.tbl_tree(as_tibble(.data), .node, ...)
}

##' @method isTip treedata
##' @rdname isTip
##' @export
isTip.treedata <- isTip.phylo
