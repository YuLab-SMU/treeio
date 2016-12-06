Ntip <- function(tree) {
    phylo <- as.phylo(tree)
    length(phylo$tip.label)
}

Nnode <- function(tree, internal.only=TRUE) {
    phylo <- as.phylo(tree)
    if (internal.only)
        return(phylo$Nnode)

    Ntip(phylo) + phylo$Nnode
}

