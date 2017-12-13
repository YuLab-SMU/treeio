##' @importFrom ape as.phylo
##' @export
ape::as.phylo


##' @method as.phylo treedata
##' @export
as.phylo.treedata <- function(x, ...) {
    return(x@phylo)
}


##' @method as.phylo phylo4
##' @export
as.phylo.phylo4 <- function(x, ...) {
    edge <- x@edge
    edge <- edge[edge[,1] != 0, ]
    edge.length <- x@edge.length
    edge.length <- edge.length[!is.na(edge.length)]
    tip.id <- sort(setdiff(edge[,2], edge[,1]))
    tip.label <- x@label[tip.id]
    phylo <- list(edge = edge,
                  edge.length = edge.length,
                  tip.label = tip.label)

    node.id <- sort(unique(edge[,1]))
    node.id <- node.id[node.id != 0]
    node.label <- x@label[node.id]
    if (!all(is.na(node.label))) {
        phylo$node.label <- node.label
    }
    phylo$Nnode <- length(node.id)
    class(phylo) <- "phylo"
    return(phylo)
}


##' @method as.phylo tbl_tree
##' @export
## original contributed by Bradley Jones and modified by Guangchuang Yu
as.phylo.tbl_tree <- function(x, ...) {
    valid.tbl_tree(x)

    edge <- x[, c("parent", "node")]
    i <- which(edge[,1] != 0 & edge[,1] != edge[,2])
    edge <- edge[i, ]
    if (is.null(x[["branch.length"]])) {
        edge.length <- NULL
    } else {
        edge.length <- x$branch.length[i]
    }

    x %<>% mutate_(isTip = ~(! node %in% parent))
    tip.label <- as.character(x$label[x$isTip])

    phylo <- list(edge = as.matrix(edge),
                  edge.length = edge.length,
                  tip.label = tip.label)

    node.label <- as.character(x$label[!x$isTip])
    if (!all(is.na(node.label))) {
        phylo$node.label <- node.label
    }
    phylo$Nnode <- sum(!x[, "isTip"])
    class(phylo) <- "phylo"
    return(phylo)
}

valid.tbl_tree <- function(object, cols = c("parent", "node", "label")) {
    cc <- cols[!cols %in% colnames(object)]
    if (length(cc) > 0) {
        msg <- paste0("invalid tbl_tree object.\n  missing column:\n    ", paste(cc, collapse=","), ".")
    }
}


##' @method as.phylo ggtree
##' @export
as.phylo.ggtree <- function(x, ...) {
    as.phylo(as_data_frame(x$data))
}


##' access phylo slot
##'
##'
##' @title get.tree
##' @param x tree object
##' @param ... additional parameters
##' @return phylo object
##' @export
##' @author guangchuang yu
get.tree <- function(x, ...) {
    as.phylo(x, ...)
}

