##' rename taxa
##'
##'
##' @title taxa_rename
##' @param tree tree object
##' @param name a two column data.frame contains original name in 1st column and new name in 2nd column
##' @return updated tree object with new taxa name
##' @export
##' @author guangchuang yu
taxa_rename <- function(tree, name) {
    ## name is a two column data.frame
    ## 1st column contains original taxa name
    ## 2nd column contains new taxa name
    phylo <- as.phylo(tree)
    taxa <- phylo$tip.label
    i <- match(name[,1], taxa)
    if (anyNA(i))
        stop("taxa name not match, please check your input...")
    taxa[i] <- name[,2]
    phylo$tip.label <- taxa
    if (inherits(tree, "phylo"))
        return(phylo)
    tree@phylo <- phylo
    return(tree)
}


##' number of tips
##'
##'
##' @title Ntip
##' @param tree tree object
##' @return number of tips
##' @export
##' @author guangchuang yu
##' @examples
##' Ntip(rtree(30))
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
##' @examples
##' Nnode(rtree(30))
##' @author guangchuang yu
Nnode <- function(tree, internal.only=TRUE) {
    phylo <- as.phylo(tree)
    if (internal.only)
        return(phylo$Nnode)

    Ntip(phylo) + phylo$Nnode
}


tipIds <- function(tree) {
    1:Ntip(tree)
}

nodeIds <- function(tree, internal.only=TRUE) {
    if (internal.only) {
        return(Ntip(tree) + 1:Nnode(tree, internal.only))
    }
    1:Nnode(tree, internal.only)
}


##' calculate total number of nodes
##'
##'
##' @title getNodeNum
##' @param tree tree object
##' @return number
##' @export
##' @examples
##' getNodeNum(rtree(30))
##' @author Guangchuang Yu
getNodeNum <- function(tree) {
    Nnode(tree, internal.only=FALSE)
}

##' @rdname getNodeNum
##' @export
##' @examples
##' Nnode2(rtree(30))
Nnode2 <- getNodeNum

getParent <- function(tr, node) {
    if ( node == getRoot(tr) )
        return(0)
    edge <- tr[["edge"]]
    parent <- edge[,1]
    child <- edge[,2]
    res <- parent[child == node]
    if (length(res) == 0) {
        stop("cannot found parent node...")
    }
    if (length(res) > 1) {
        stop("multiple parent found...")
    }
    return(res)
}

getChild <- function(tr, node) {
    edge <- tr[["edge"]]
    res <- edge[edge[,1] == node, 2]
    ## if (length(res) == 0) {
    ##     ## is a tip
    ##     return(NA)
    ## }
    return(res)
}

getSibling <- function(tr, node) {
    root <- getRoot(tr)
    if (node == root) {
        return(NA)
    }

    parent <- getParent(tr, node)
    child <- getChild(tr, parent)
    sib <- child[child != node]
    return(sib)
}


getAncestor <- function(tr, node) {
    root <- getRoot(tr)
    if (node == root) {
        return(NA)
    }
    parent <- getParent(tr, node)
    res <- parent
    while(parent != root) {
        parent <- getParent(tr, parent)
        res <- c(res, parent)
    }
    return(res)
}

##' get the root number
##'
##'
##' @title getRoot
##' @param tr phylo object
##' @return root number
##' @export
##' @examples
##' getRoot(rtree(10))
##' @author Guangchuang Yu
getRoot <- function(tr) {
    edge <- tr[["edge"]]
    ## 1st col is parent,
    ## 2nd col is child,
    if (!is.null(attr(tr, "order")) && attr(tr, "order") == "postorder")
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

##' test whether input object is produced by ggtree function
##'
##'
##' @title is.ggtree
##' @param x object
##' @return TRUE or FALSE
##' @export
##' @examples
##' library(ggtree)
##' p <- ggtree(rtree(30))
##' is.ggtree(p)
##' @author guangchuang yu
is.ggtree <- function(x) inherits(x, 'ggtree')

getNodeName <- function(tr) {
    if (is.null(tr$node.label)) {
        n <- Ntip(tr)
        nl <- 1:(Nnode2(tr)-n) + n
        nl <- as.character(nl)
    }
    else {
        nl <- tr$node.label
    }
    nodeName <- c(tr$tip.label, nl)
    return(nodeName)
}

