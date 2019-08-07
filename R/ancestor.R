##' @importFrom tidytree parent
##' @method parent phylo
##' @export
parent.phylo <- function(.data, .node, ...) {
    vapply(.node, function(nn) {
        if ( nn == rootnode(.data) )
            return(0)
        edge <- .data[["edge"]]
        parent <- edge[,1]
        child <- edge[,2]
        res <- parent[child == nn]
        if (length(res) == 0) {
            stop("cannot found parent node...")
        }
        if (length(res) > 1) {
            stop("multiple parent found...")
        }
        return(res)
    }, numeric(1))
}

##' @method parent treedata
##' @export
parent.treedata <- function(.data,  .node,  ...) {
    parent.phylo(as.phylo(.data), .node, ...)
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

##' @method ancestor treedata
##' @export
ancestor.treedata <- function(.data, .node, ...) {
    ancestor.phylo(as.phylo(.data), .node, ...)
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
        stop("multiple roots found...")
    }
    return(root)
}

##' @method rootnode phylo
##' @export
rootnode.treedata <- function(.data, ...) {
    rootnode.phylo(as.phylo(.data), ...)
}
