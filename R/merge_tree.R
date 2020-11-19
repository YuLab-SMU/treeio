##' merge two tree object
##'
##'
##' @title merge_tree
##' @param obj1 tree object 1
##' @param obj2 tree object 2
##' @return tree object
##' @importFrom dplyr full_join
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

    if (nrow(obj2@data) == 0 && nrow(obj2@extraInfo) == 0)
        return(obj1)


    tr1 <- as.phylo(obj1)
    tr2 <- as.phylo(obj2)

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
    ## node_map$from %<>% c(1:Ntip(tr2))
    ## node_map$to %<>% c(idx)
    node_map$from <- c(node_map$from, 1:Ntip(tr2))
    node_map$to <- c(node_map$to, idx)

    node1 <- node_map$to
    node2 <- node_map$from

    while(length(node1) > 0) {
        ## p1 <- vapply(node1, function(n) parent(tr1, n), numeric(1))
        ## p2 <- vapply(node2, function(n) parent(tr2, n), numeric(1))
        p1 <- parent(tr1, node1)
        p2 <- parent(tr2, node2)
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

    if (nrow(obj2@data) == 0) {
        info2 <- obj2@extraInfo
    } else if (nrow(obj2@extraInfo) == 0) {
        info2 <- obj2@data
    } else {
        info2 <- full_join(obj2@data, obj2@extraInfo, by = "node")
    }

    info2$node <- node_map.df[match(info2$node, node_map.df[,1]),2]

    extraInfo <- obj1@extraInfo
    if (nrow(extraInfo) == 0) {
        obj1@extraInfo <- info2
    } else {
        obj1@extraInfo <- full_join(extraInfo, info2, by = "node")
    }
    obj1@extraInfo$node <- as.integer(obj1@extraInfo$node)

    obj1@file <- c(obj1@file, obj2@file)
    obj1@file <- obj1@file[obj1@file != ""]
    return(obj1)
}
