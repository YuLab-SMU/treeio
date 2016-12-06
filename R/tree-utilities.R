##' number of tips
##'
##'
##' @title Ntip
##' @param tree tree object
##' @return number of tips
##' @export
##' @author guangchuang yu
Ntip <- function(tree) {
    phylo <- as.phylo(tree)
    length(phylo$tip.label)
}

##' number of nodes
##'
##'
##' @title Nnode
##' @param tree tree object
##' @param internal.only whether only count internal nodes
##' @return number of nodes
##' @export
##' @author guangchuang yu
Nnode <- function(tree, internal.only=TRUE) {
    phylo <- as.phylo(tree)
    if (internal.only)
        return(phylo$Nnode)

    Ntip(phylo) + phylo$Nnode
}

