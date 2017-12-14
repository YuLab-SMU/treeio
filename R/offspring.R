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

##' @importFrom tidytree offspring
##' @method offspring phylo
##' @export
offspring.phylo <- function(.data, .node, ...) {
    sp <- child(.data, .node)
    sp <- sp[sp != 0]
    if (length(sp) == 0) {
        stop("input node is a tip...")
    }
    i <- 1
    while (i <= length(sp)) {
        sp <- c(sp, child(.data, sp[i]))
        sp <- sp[sp != 0]
        i <- i + 1
    }
    return(sp)
}
