##' merge two tree object
##'
##'
##' @title merge_tree
##' @param obj1 tree object 1
##' @param obj2 tree object 2
##' @return tree object
##' @importFrom magrittr %<>%
##' @importFrom ggplot2 fortify
##' @export
##' @author Guangchuang Yu
merge_tree <- function(obj1, obj2) {
    ##
    ## INFO:
    ## ape::all.equal.phylo can be used to test equal phylo topology.
    ##

    if (has.slot(obj1, "extraInfo") == FALSE) {
        stop("input tree object is not supported...")
    }

    if ((is.tree(obj1) & is.tree(obj2)) == FALSE) {
        stop("input should be tree objects...")
    }

    tr1 <- get.tree(obj1)
    tr2 <- get.tree(obj2)

    if (getNodeNum(tr1) != getNodeNum(tr2)) {
        stop("number of nodes not equals...")
    }

    if (Ntip(tr1) != Ntip(tr2)) {
        stop("number of tips not equals...")
    }

    if (all(tr1$tip.label %in% tr2$tip.label) == FALSE) {
        stop("tip names not match...")
    }

    idx <- match(tr2$tip.label, tr1$tip.label)

    node_map <- list()
    node_map$from %<>% c(1:Ntip(tr2))
    node_map$to %<>% c(idx)
    node1 <- node_map$to
    node2 <- node_map$from

    while(length(node1) > 0) {
        p1 = sapply(node1, getParent, tr = tr1)
        p2 = sapply(node2, getParent, tr = tr2)
        if (!all(duplicated(p1) == duplicated(p2))) {
            stop("tree structure not identical...")
        }
        node1 <- unique(p1[p1!=0])
        node2 <- unique(p2[p2!=0])
        node_map$from <- unique(c(node_map$from, node2))
        node_map$to   <- unique(c(node_map$to,   node1))
    }


    node_map.df <- do.call("cbind", node_map)
    node_map.df <- unique(node_map.df)
    node_map.df <- node_map.df[node_map.df[,1] != 0,]
    i <- order(node_map.df[,1], decreasing = FALSE)
    node_map.df <- node_map.df[i,]

    info2 <- fortify(obj2)
    info2$node <- node_map.df[info2$node, 2]
    info2$parent <- node_map.df[info2$parent, 2]

    cn <- colnames(info2)
    i <- match(c("x", "y", "isTip", "label", "branch", "branch.length", "angle"), cn)
    i <- i[!is.na(i)]
    info2 <- info2[, -i]

    extraInfo <- obj1@extraInfo
    if (nrow(extraInfo) == 0) {
        obj1@extraInfo <- info2
    } else {
        info <- merge(extraInfo, info2, by.x =c("node", "parent"), by.y = c("node", "parent"))
        obj1@extraInfo <- info
    }

    return(obj1)
}
