##' @importFrom ape read.tree
##' @export
ape::read.tree

##' @importFrom ape read.nexus
##' @export
ape::read.nexus


##' @importFrom ape rtree
##' @export
ape::rtree

##' @importFrom ape write.tree
##' @export
ape::write.tree

##' @importFrom ape write.nexus
##' @export
ape::write.nexus

##' @importFrom ape Nnode
##' @export
ape::Nnode

##' @importFrom ape Ntip
##' @export
ape::Ntip

##' @importFrom ape is.rooted
##' @export
ape::is.rooted

##' @importFrom ape root
##' @export
ape::root

##' @method Ntip treedata
##' @importFrom ape Ntip
##' @export
Ntip.treedata <- function(phy) {
    Ntip(as.phylo(phy))
}

##' number of nodes
##'
##'
##' @title Nnode
##' @param phy treedata object
##' @param internal.only whether only count internal nodes
##' @param ... additional parameters
##' @return number of nodes
##' @method Nnode treedata
##' @export
##' @examples
##' Nnode(rtree(30))
##' @author guangchuang yu
Nnode.treedata <- function(phy, internal.only=TRUE, ...) {
    Nnode(as.phylo(phy), internal.only = internal.only, ...)
}

##' @method is.rooted treedata
##' @importFrom ape is.rooted
##' @export
is.rooted.treedata <- function(phy) {
    is.rooted(as.phylo(phy))
}
