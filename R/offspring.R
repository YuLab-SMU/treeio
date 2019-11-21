##' @importFrom tidytree child
##' @method child phylo
##' @export
child.phylo <- function(.data, .node, ...) {
    edge <- .data[["edge"]]
    res <- edge[edge[,1] == .node, 2]
    ## if (length(res) == 0) {
    ##     ## is a tip
    ##     return(NA)
    ## }
    return(res)
}

##' @method child treedata
##' @export
child.treedata <- function(.data, .node, ...) {
    child.phylo(as.phylo(.data), .node, ...)
}

##' @importFrom tidytree offspring
##' @method offspring phylo
##' @export
offspring.phylo <- function(.data, .node, tiponly = FALSE, self_include = FALSE, ...) {
    if (self_include) {
        sp <- .node
    } else {
        sp <- child(.data, .node)
    }

    sp <- sp[sp != 0]
    if (length(sp) == 0) {
        return(sp)
        ## stop("input node is a tip...")
    }
    i <- 1
    while (i <= length(sp)) {
        sp <- c(sp, child(.data, sp[i]))
        sp <- sp[sp != 0]
        i <- i + 1
    }
    if (tiponly) {
        return(sp[sp <= Ntip(.data)])
    }
    return(sp)
}


##' @method offspring treedata
##' @export
offspring.treedata <- function(.data, .node, tiponly = FALSE, self_include = FALSE, ...) {
    offspring.phylo(as.phylo(.data), .node,
                    tiponly = tiponly, self_include = self_include, ...)
}
