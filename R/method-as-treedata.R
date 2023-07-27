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
as.treedata.tbl_df <- function(tree, branch.length, label, ...) {
    edgelist <- as_tibble(data.frame(tree))
    branch.length <- rlang::enquo(branch.length)
    label <- rlang::enquo(label)
    if (nrow(unique(edgelist[, 1])) > nrow(unique(edgelist[,2]))){
        edgelist %<>% dplyr::select(rev(seq_len(2)), setdiff(seq_len(ncol(edgelist)), c(1,2)))
    }    
    edgelist$`.NAME` <- paste0('name', seq_len(nrow(edgelist)))
    phylo <- as.phylo.tbl_df(edgelist, branch.length=!!branch.length, label=".NAME", ...)

    res <- new("treedata",
               phylo = phylo)

    if (ncol(edgelist) >= 4) {
        d <- edgelist[,-c(1,2)]
        #length_var <- attr(phylo, "length_var")

        if (!rlang::quo_is_missing(branch.length)) {
            d <- d[, names(d) != rlang::as_name(branch.length)]
            if (ncol(d) == 1 && colnames(d)=='.NAME'){
                res@phylo$tip.label <- edgelist[match(res@phylo$tip.label,edgelist$`.NAME`),2,drop=TRUE]
                res@phylo$node.label <- edgelist[match(res@phylo$node.label,edgelist$`.NAME`),2,drop=TRUE]
                return(res)
            }
        }
        if (!rlang::quo_is_missing(label)){
            d <- d[,names(d) != rlang::as_name(label)]
            if (ncol(d)== 1 && colnames(d)=='.NAME'){
                res@phylo$tip.label <- edgelist[match(res@phylo$tip.label,edgelist$`.NAME`),rlang::as_name(label),drop=TRUE]
                res@phylo$node.label <- edgelist[match(res@phylo$node.label,edgelist$`.NAME`),rlang::as_name(label),drop=TRUE]
                return(res)
            }
            res <- dplyr::left_join(res, d, by = c('label' = '.NAME'))
            res@phylo$tip.label <- edgelist[match(res@phylo$tip.label,edgelist$`.NAME`),rlang::as_name(label),drop=TRUE]
            nodelab <- edgelist[match(res@phylo$node.label,edgelist$`.NAME`),rlang::as_name(label),drop=TRUE]
            if (all(is.na(nodelab))){
                res@phylo$node.label <- NULL
            }else{
                res@phylo$node.label <- nodelab
            }
        }else{
            res <- dplyr::left_join(res, d, by=c('label'='.NAME'))
            res@phylo$tip.label <- edgelist[match(res@phylo$tip.label,edgelist$`.NAME`),2,drop=TRUE]
            res@phylo$node.label <- edgelist[match(res@phylo$node.label,edgelist$`.NAME`),2,drop=TRUE]
        }
        return(res)
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
