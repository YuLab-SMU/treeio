old_new_node_mapping <- function(oldtree, newtree){
    treelab1 <- oldtree %>% 
                as_tibble() %>%
                dplyr::select(c("node", "label"))
    treelab2 <- newtree %>% 
                as_tibble() %>%
                dplyr::select(c("node", "label"))
    node_map <- dplyr::inner_join(treelab1, treelab2, by="label") %>%
                dplyr::select(c("node.x", "node.y")) %>%
                dplyr::rename(c(old="node.x", new="node.y"))
    return(node_map)
}

# ##' re-root a tree
# ##'
# ##'
# ##' @title root
# ##' @rdname root-method
# ##' @param phy tree object
# ##' @param outgroup a vector of mode numeric or character specifying the new outgroup
# ##' @param node node to reroot
# ##' @param edgelabel a logical value specifying whether to treat node labels as 
# ##' edge labels and thus eventually switching them so that they are associated 
# ##' with the correct edges.
# ##' @param ... additional parameters passed to ape::root.phylo
# ##' @return rerooted tree
# ##' @importFrom ape root
# ##' @method root phylo
# ##' @export
# ##' @author Guangchuang Yu
# 
# root.phylo <- function(phy, outgroup, node = NULL, edgelabel = TRUE, ...){
#     tree <- ape::root.phylo(phy, outgroup = outgroup, node = node,
#                             edgelabel = edgelabel, ...)
# 
#     attr(tree, "reroot") <- TRUE
#     node_map <- reroot_node_mapping(phy, tree)
#     attr(tree, "node_map") <- node_map
#     return(tree)
# }


##' re-root a tree
##'
##' @title root
##' @rdname root-method
##' @param phy tree object
##' @param outgroup a vector of mode numeric or character specifying the new outgroup
##' @param node node to reroot
##' @param edgelabel a logical value specifying whether to treat node labels as
##' edge labels and thus eventually switching them so that they are associated
##' with the correct edges.
##' @param ... additional parameters passed to ape::root.phylo
##' @return rerooted treedata
##' @method root treedata
##' @export

root.treedata <- function(phy, outgroup, node = NULL, edgelabel = TRUE, ...){
    if (!edgelabel){
    ## warning message
        message("The use of this method may cause some node data to become incorrect (e.g. bootstrap values) if 'edgelabel' is FALSE.")
    }
    #object <- phy
    # generate node old label and new label map table.
    res <- build_new_labels(tree=phy)
    tree <- res$tree
    node2oldnewlab <- res$node2old_new_lab
    # reroot tree
    re_tree <- root(tree, outgroup = outgroup, node = node,
                 edgelabel = edgelabel, ...)
    
    node_map <- old_new_node_mapping(tree, re_tree)
    n.tips <- Ntip(re_tree)
    
    # replace new label with old label
    phy@phylo <- build_new_tree(tree=re_tree, node2old_new_lab=node2oldnewlab)

    # update data or extraInfo function
    update_data <- function(data, node_map) {
        cn <- colnames(data)
        cn <- cn[cn != "node"]
        data <- dplyr::inner_join(data, node_map, by=c("node"="old")) %>%
                dplyr::select(c("new", cn)) %>% 
                dplyr::rename(node=.data$new)

        # clear root data
        root <- data$node == (n.tips + 1)
        data[root,] <- NA
        data[root,'node'] <- n.tips + 1
        return(data)
    }
    if (nrow(phy@data) > 0) {
        phy@data <- update_data(phy@data, node_map)
    }    
    if (nrow(phy@extraInfo) > 0){
        phy@extraInfo <- update_data(phy@extraInfo, node_map)
    }

    return(phy)
}


## reroot_node_mapping <- function(tree, tree2) {
##     root <- rootnode(tree)
## 
## 
##     node_map <- data.frame(from=1:getNodeNum(tree), to=NA, visited=FALSE)
##     node_map[1:Ntip(tree), 2] <- match(tree$tip.label, tree2$tip.label)
##     node_map[1:Ntip(tree), 3] <- TRUE
## 
##     node_map[root, 2] <- root
##     node_map[root, 3] <- TRUE
## 
##     node <- rev(tree$edge[,2])
##     for (k in node) {
##         ##ip <- getParent(tree, k)
##         ip <- parent(tree, k)
##         if (node_map[ip, "visited"])
##             next
## 
##         ## cc <- getChild(tree, ip)
##         cc <- child(tree, ip)
##         node2 <- node_map[cc,2]
##         if (anyNA(node2)) {
##             node <- c(node, k)
##             next
##         }
## 
##         ## to <- unique(sapply(node2, getParent, tr=tree2))
##         to <- unique(sapply(node2, parent, .data=tree2))
##         to <- to[! to %in% node_map[,2]]
##         node_map[ip, 2] <- to
##         node_map[ip, 3] <- TRUE
##     }
##     node_map <- node_map[, -3]
##     return(node_map)
## }
## 
## 
## ##' re-root a tree
## ##'
## ##' 
## ##' @title root
## ##' @rdname root-method
## ##' @param phy tree object
## ##' @param outgroup a vector of mode numeric or character specifying the new outgroup
## ##' @param node node to reroot
## ##' @param resolve.root a logical specifying whether to resolve the new root as a bifurcating node
## ##' @param ... additional parameters passed to ape::root.phylo
## ##' @return rerooted tree
## ##' @importFrom ape root
## ##' @method root phylo
## ##' @export
## ##' @author Guangchuang Yu
## root.phylo <- function(phy, outgroup, node = NULL, resolve.root = TRUE, ...) {
##     ## pos <- 0.5* object$edge.length[which(object$edge[,2] == node)]
##     
##     ## @importFrom phytools reroot
##     ## phytools <- "phytools"
##     ## require(phytools, character.only = TRUE, quietly = TRUE)
##     
##     ## phytools_reroot <- eval(parse(text="phytools::reroot"))
##     
##     ## tree <- phytools_reroot(object, node, pos)
## 
##     tree <- ape::root.phylo(phy, outgroup = outgroup,
##                             node = node, resolve.root = resolve.root, ...)
## 
##     #if (Nnode(tree) != Nnode(phy)) {
##     #    return(tree)
##     #}
## 
##     attr(tree, "reroot") <- TRUE
##     node_map <- reroot_node_mapping(phy, tree)
##     attr(tree, "node_map") <- node_map
##     return(tree)
## }
## 
## 
## ##' @rdname root-method
## ##' @method root treedata
## ##' @export
## root.treedata <- function(phy, outgroup, node = NULL, resolve.root = TRUE, ...) {
##     ## warning message
##     message("The use of this method may cause some node data to become incorrect (e.g. bootstrap values).")
## 
##     object <- phy
##     newobject <- object
## 
##     ## ensure nodes/tips have a label to properly map @anc_seq/@tip_seq
##     tree <- object@phylo
##     if (is.null(tree$tip.label)) {
##         tree$tip.label <- as.character(1:Ntip(tree))
##     }
##     if (is.null(tree$node.label)) {
##         #tree$node.label <- as.character((1:tree$Nnode) + Ntip(tree))
##         tree$node.label <- paste0("node",1:Nnode(tree))
##     }
##     
##     ## reroot tree
##     tree <- root(tree, outgroup = outgroup, node = node,
##                  resolve.root = resolve.root, ...)
##     newobject@phylo <- tree
## 
##     ## update node numbers in data
##     n.tips <- Ntip(tree)
##     node_map<- attr(tree, "node_map")
##     if (is.null(node_map)) {
##         message("fail to assign associated data to rooted tree, only return tree structure (a phylo object)")
##         if (!resolve.root) {
##             message("maybe you can try again with `resolve.root = TRUE`")
##         }
##         return(tree)
##     }
## 
##     update_data <- function(data, node_map) {
##         newdata <- data
##         
##         indx <- match(node_map$from, data$node)
##         indx <- indx[!is.na(indx)]
##         
##         indy <- match(data$node, node_map$from)
##         indy <- indy[!is.na(indy)]
##         
##         newdata[indx, "node"] <- node_map[indy, "to"]
##         
##         # newdata[match(node_map$from, data$node), 'node'] <- node_map$to
## 
##         # clear root data
##         root <- newdata$node == (n.tips + 1)
##         newdata[root,] <- NA
##         newdata[root,'node'] <- n.tips + 1
##         
##         return(newdata)
##     }
##     
##     if (nrow(newobject@data) > 0) {
##         newobject@data <- update_data(object@data, node_map)
##     }
##     
##     if (nrow(object@extraInfo) > 0) {
##         newobject@extraInfo <- update_data(object@extraInfo, node_map)
##     }
##     
##     return(newobject)
## }
