##' @method nodeid phylo
##' @export
nodeid.phylo <- function(tree, label) {
    ## nodeid(as_tibble(tree), label)
    lab <- c(tree$tip.label, tree$node.label)
    match(label, lab)
}

##' @method nodeid treedata
##' @export
nodeid.treedata <- function(tree, label) {
    nodeid(as.phylo(tree), label)
}

##' @method nodelab phylo
##' @export
nodelab.phylo <- function(tree, id) {
    nodelab(as_tibble(tree), id)
}

##' @method nodelab treedata
##' @export
nodelab.treedata <- function(tree, id) {
    nodelab(as.phylo(tree), id)
}
