##' @importFrom tidytree MRCA
##' @importFrom tidytree as_tibble
##' @method MRCA phylo
##' @export
MRCA.phylo <- function(.data, .node1, .node2, ...) {
    MRCA(as_tibble(.data), .node1, .node2, ...)[["node"]]
}

##' @method MRCA treedata
##' @export
MRCA.treedata <- function(.data, .node1, .node2, ...) {
    MRCA.phylo(as.phylo(.data), .node1, .node2, ...)
}
