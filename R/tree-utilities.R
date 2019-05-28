

##' label branch for PAML to infer selection pressure using branch model
##'
##'
##' @title label_branch_paml
##' @param tree phylo object
##' @param node node number
##' @param label label of branch, e.g. #1
##' @return updated phylo object
##' @export
##' @author guangchuang yu
label_branch_paml <- function(tree, node, label) {
    sp_id <- offspring(tree, node)
    tip_id <- sp_id[sp_id <= Ntip(tree)]
    node_id <- c(node, sp_id[sp_id > Ntip(tree)])
    tree$tip.label[tip_id] <- paste(tree$tip.label[tip_id], label)
    if (is.null(tree$node.label)) {
        tree$node.label <- rep("", Nnode(tree))
    }
    node_index <- node_id - Ntip(tree)
    tree$node.label[node_index] <- paste(tree$node.label[node_index], label)
    return(tree)
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


##' test whether input object is produced by ggtree function
##'
##'
##' @title is.ggtree
##' @param x object
##' @return TRUE or FALSE
##' @export
##' @author guangchuang yu
is.ggtree <- function(x) inherits(x, 'ggtree')

getNodeName <- function(tr) {
    if (is.null(tr$node.label)) {
        n <- Ntip(tr)
        nl <- 1:Nnode(tr) + n
        nl <- as.character(nl)
    }
    else {
        nl <- tr$node.label
    }
    nodeName <- c(tr$tip.label, nl)
    return(nodeName)
}

