reroot_node_mapping <- function(tree, tree2) {
    root <- rootnode(tree)

    node_map <- data.frame(from=1:getNodeNum(tree), to=NA, visited=FALSE)
    node_map[1:Ntip(tree), 2] <- match(tree$tip.label, tree2$tip.label)
    node_map[1:Ntip(tree), 3] <- TRUE

    node_map[root, 2] <- root
    node_map[root, 3] <- TRUE

    node <- rev(tree$edge[,2])
    for (k in node) {
        ##ip <- getParent(tree, k)
        ip <- parent(tree, k)
        if (node_map[ip, "visited"])
            next

        ## cc <- getChild(tree, ip)
        cc <- child(tree, ip)
        node2 <- node_map[cc,2]
        if (anyNA(node2)) {
            node <- c(node, k)
            next
        }

        ## to <- unique(sapply(node2, getParent, tr=tree2))
        to <- unique(sapply(node2, parent, .data=tree2))
        to <- to[! to %in% node_map[,2]]
        node_map[ip, 2] <- to
        node_map[ip, 3] <- TRUE
    }
    node_map <- node_map[, -3]
    return(node_map)
}


##' re-root a tree
##'
##'
##' @title reroot
##' @rdname reroot-method
##' @param object tree object
##' @param node node to reroot
##' @param ... additional parameters
##' @return rerooted tree
##' @author Guangchuang Yu
##' @export
reroot <- function(object, node, ...) UseMethod("reroot")

##' @rdname reroot-method
##' @method reroot phylo
##' @export
reroot.phylo <- function(object, node, ...) {
    pos <- 0.5* object$edge.length[which(object$edge[,2] == node)]

    ## @importFrom phytools reroot
    phytools <- "phytools"
    require(phytools, character.only = TRUE, quietly = TRUE)

    phytools_reroot <- eval(parse(text="phytools::reroot"))

    tree <- phytools_reroot(object, node, pos)
    attr(tree, "reroot") <- TRUE
    node_map <- reroot_node_mapping(object, tree)
    attr(tree, "node_map") <- node_map
    return(tree)
}


##' @rdname reroot-method
##' @method reroot treedata
##' @export
reroot.treedata <- function(object, node, ...) {
                                        # warning message
    message("The use of this method may cause some node data to become incorrect (e.g. bootstrap values).")

    newobject <- object

                                        # ensure nodes/tips have a label to properly map @anc_seq/@tip_seq
    tree <- object@phylo
    if (is.null(tree$tip.label)) {
        tree$tip.label <- as.character(1:Ntip(tree))
    }
    if (is.null(tree$node.label)) {
        tree$node.label <- as.character((1:tree$Nnode) + Ntip(tree))
    }

                                        # reroot tree
    tree <- reroot(tree, node, ...)
    newobject@phylo <- tree

                                        # update node numbers in data
    n.tips <- Ntip(tree)
    node_map<- attr(tree, "node_map")

    update_data <- function(data, node_map) {
        newdata <- data
        newdata[match(node_map$from, data$node), 'node'] <- node_map$to

                                        # clear root data
        root <- newdata$node == (n.tips + 1)
        newdata[root,] <- NA
        newdata[root,'node'] <- n.tips + 1

        return(newdata)
    }

    if (nrow(newobject@data) > 0) {
        newobject@data <- update_data(object@data, node_map)
    }

    if (nrow(object@extraInfo) > 0) {
        newobject@extraInfo <- update_data(object@extraInfo, node_map)
    }

    return(newobject)
}
