##' convert phylo to treedata
##'
##'
##' converting phylo object to treedata object
##' @title as.treedata
##' @rdname as.treedata
##' @param tree input tree, a \code{phylo} object
##' @param boot optional, can be bootstrap value from ape::boot.phylo
##' @param ... additional parameters
##' @importFrom methods new
##' @importFrom tidytree as.treedata
##' @method as.treedata phylo
##' @export
##' @author Guangchuang Yu
as.treedata.phylo <- function(tree, boot=NULL, ...) {
    ## boot is output from boot.phylo
    res <- new("treedata",
               phylo = tree
               )

    if (!is.null(boot)) {
        res@data <- tibble(node=nodeIds(tree), bootstrap=boot)
    }
    return(res)
}

##' @method as.treedata phylo4
##' @export
as.treedata.phylo4 <- function(tree, ...) {
    new("treedata",
        phylo = as.phylo(tree))
}

##' @method as.treedata phylo4d
##' @export
as.treedata.phylo4d <- function(tree, ...) {
    d <- as_tibble(tree@data)
    d$node <- as.numeric(rownames(tree@data))

    new("treedata",
        phylo = as.phylo.phylo4(tree),
        data = d
        )
}


##' @method as.treedata ggtree
##' @export
as.treedata.ggtree <- function(tree, ...) {
    d <- as_tibble(tree$data)
    class(d) <- c("tbl_tree", "tbl_df", "tbl", "data.frame")
    as.treedata(d, ...)
}


##' @method as.treedata tbl_df
##' @export
as.treedata.tbl_df <- function(tree, ...) {
    edgelist <- as_tibble(tree)
    edge <- check_edgelist(edgelist)
    indx <- attr(edge, "indx")
    if (!is.null(indx)){
        edgelist <- edgelist[indx,]
        attr(edge, "indx") <- NULL
    }
    phylo <- as.phylo.tbl_df(edgelist, ...)

    res <- new("treedata",
               phylo = phylo)

    if (ncol(edgelist) >= 3) {
        d <- edgelist[,-c(1,2)]
        length_var <- attr(phylo, "length_var")

        if (!is.null(length_var)) {
            d <- d[, names(d) != length_var]
            if (ncol(d) == 0) return(res)
        }

        lab <- c(phylo$tip.label, phylo$node.label)

        #edge <- check_edgelist(edgelist)
        children <- edge[,2]

        d$node <- match(children, lab)
        res@data <- d
    }

    return(res)
}



##' @method as.treedata data.frame
##' @export
as.treedata.data.frame <- as.treedata.tbl_df


##' @method as.treedata matrix
##' @export
as.treedata.matrix <- as.treedata.tbl_df


##' @method as.treedata pvclust
##' @export
## contributed by Konstantinos Geles and modified by Guangchuang Yu
as.treedata.pvclust <- function(tree, ...) {
    phylo <- as.phylo.hclust_node(tree$hclust, ...)

    ## tranforming the pvclust bootstraps values to tibble with key column:"label"
    tree_boots <- (round(tree$edges[, c("si","au", "bp")],2)*100) %>% 
        as_tibble() %>%
        mutate(label = paste0(seq_len(Nnode(phylo)),"_edge"))

    tidytree::left_join(phylo, tree_boots, by = 'label')
}


## reference: https://stackoverflow.com/questions/22749634/how-to-append-bootstrapped-values-of-clusters-tree-nodes-in-newick-format-in
as.phylo.hclust_node <- function(x, hang = NULL){
    N <- dim(x$merge)[1]
    edge <- matrix(0L, 2 * N, 2)
    edge.length <- numeric(2 * N)
    node <- integer(N)
    node[N] <- N + 2L
    cur.nod <- N + 3L
    j <- 1L
    for (i in N:1) {
        edge[j:(j + 1), 1] <- node[i]
        for (l in 1:2) {
            k <- j + l - 1L
            y <- x$merge[i, l]
            if (y > 0) {
                edge[k, 2] <- node[y] <- cur.nod
                cur.nod <- cur.nod + 1L
                edge.length[k] <- x$height[i] - x$height[y]
            } else {
                edge[k, 2] <- -y
                edge.length[k] <- x$height[i]
            }
        }
        j <- j + 2L
    }
    if (is.null(x$labels)) 
        x$labels <- as.character(1:(N + 1))  
    node.lab <- order(node) # here we keep the order for the edges
    obj <- list(edge = edge, edge.length = edge.length/2, 
                tip.label = x$labels, 
                Nnode = N, 
                node.label = paste(node.lab,"_edge",sep = "")) # export it to the final object 
    class(obj) <- "phylo"
    obj <- stats::reorder(obj)
    if (!is.null(hang) && hang > 0){
        tip2parent <- edge[match(seq_len(N+1), edge[,2]), 1]
        tip.edge.len <- hang * max(x$height) - x$height[match(tip2parent, node)]
        obj$edge.length <- obj$edge.length * 2
        attr(obj, 'tip.edge.len') <- tip.edge.len
    }else if (hang < 0){
        obj$edge.length <- obj$edge.length * 2
    }
    return(obj)
}
