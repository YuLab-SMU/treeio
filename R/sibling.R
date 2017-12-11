##' @method sibling phylo
##' @export
sibling.phylo <- function(.data, .node, ...) {
    root <- rootnode(.data)
    if (.node == root) {
        return(NA)
    }

    pp <- parent(.data, .node)
    cc <- child(.data, pp)
    sib <- cc[cc != .node]
    return(sib)
}
