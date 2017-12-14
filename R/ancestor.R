##' @importFrom tidytree parent
##' @method parent phylo
##' @export
parent.phylo <- function(.data, .node, ...) {
    if ( .node == rootnode(.data) )
        return(0)
    edge <- .data[["edge"]]
    parent <- edge[,1]
    child <- edge[,2]
    res <- parent[child == .node]
    if (length(res) == 0) {
        stop("cannot found parent node...")
    }
    if (length(res) > 1) {
        stop("multiple parent found...")
    }
    return(res)
}

##' @importFrom tidytree ancestor
##' @method ancestor phylo
##' @export
ancestor.phylo <- function(.data, .node, ...) {
    root <- rootnode(.data)
    if (.node == root) {
        return(NA)
    }
    p <- parent(.data, .node)
    res <- p
    while(p != root) {
        p <- parent(.data, p)
        res <- c(res, p)
    }
    return(res)
}

##' @importFrom tidytree rootnode
##' @method rootnode phylo
##' @export
rootnode.phylo <- function(.data, ...) {
    edge <- .data[["edge"]]
    ## 1st col is parent,
    ## 2nd col is child,
    if (!is.null(attr(.data, "order")) && attr(.data, "order") == "postorder")
        return(edge[nrow(edge), 1])

    parent <- unique(edge[,1])
    child <- unique(edge[,2])
    ## the node that has no parent should be the root
    root <- parent[ ! parent %in% child ]
    if (length(root) > 1) {
        stop("multiple roots founded...")
    }
    return(root)
}
