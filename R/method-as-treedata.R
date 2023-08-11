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
    phylo <- as.phylo.hclust2(tree$hclust, ...)

    ## tranforming the pvclust bootstraps values to tibble with key column:"label"
    tree_boots <- (round(tree$edges[, c("si","au", "bp")],2)*100) %>% 
        as_tibble() %>%
        mutate(label = paste0(seq_len(Nnode(phylo)),"_edge"))

    tidytree::left_join(phylo, tree_boots, by = 'label')
}


## reference: https://stackoverflow.com/questions/22749634/how-to-append-bootstrapped-values-of-clusters-tree-nodes-in-newick-format-in
as.phylo.hclust2 <- function(x, hang = NULL){
    h <- x
    tr <- ape::as.phylo(x)
    ev <- edge2vec(tr)
    nodes <- integer(length(h$height))
    for (i in seq_along(nodes)) {
        j <- h$merge[i, ]
        if (any(j < 0)) {
            j2 <- j[j < 0][1]
            nodes[i] <- ev[abs(j2)]
        }
        else {
            nodes[i] <- ev[nodes[j[1]]]
        }
    }
    tip2parent <- tr$edge[match(seq_len(Ntip(tr)), tr$edge[,
        2]), 1]
    if (hang > 0) {
        tip.edge.len <- hang * max(h$height) - h$height[match(tip2parent,
            nodes)]
        attr(tr, "tip.edge.len") <- tip.edge.len
    }
    tr$edge.length <- tr$edge.length * 2
    return(tr)
}

edge2vec <- function(tr){
    parent <- tr$edge[, 1]
    child <- tr$edge[, 2]
    pvec <- integer(max(tr$edge))
    pvec[child] <- parent
    return(pvec)
}
