

##' label branch for PAML to infer selection pressure using branch model
##'
##'
##' @title label_branch_paml
##' @param tree phylo object
##' @param node node number
##' @param label label of branch, e.g. #1
##' @return updated phylo object
##' @export
##' @author Guangchuang Yu
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
##' @author Guangchuang Yu
is.ggtree <- function(x) {
    if (inherits(x, 'ggtree')) return(TRUE)

    if (!inherits(x, 'gg')) return(FALSE)

    ## to compatible with user using `ggplot(tree) + geom_tree()`

    tree_layer <- vapply(x$layers,
                         function(y) {
                             any(grepl("StatTree", class(y$stat)))
                         },
                         logical(1)
                         )
    return(any(tree_layer))
}



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


.extract_annotda.treedata <- getFromNamespace('.extract_annotda.treedata', 'tidytree')

.internal_nest <- getFromNamespace('.internal_nest', 'tidytree')

.update.td.join <- getFromNamespace('.update.td.join', 'tidytree')

