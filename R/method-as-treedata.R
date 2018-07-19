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
##' @author guangchuang yu
as.treedata.phylo <- function(tree, boot=NULL, ...) {
    ## boot is output from boot.phylo
    res <- new("treedata",
               phylo = tree
               )

    if (!is.null(boot)) {
        res@data <- data_frame(node=nodeIds(tree), bootstrap=boot)
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
    d <- as_data_frame(tree@data)
    d$node <- as.numeric(rownames(tree@data))

    new("treedata",
        phylo = as.phylo.phylo4(tree),
        data = d
        )
}


##' @method as.treedata ggtree
##' @export
as.treedata.ggtree <- function(tree, ...) {
    d <- as_data_frame(tree$data)
    class(d) <- c("tbl_tree", "tbl_df", "tbl", "data.frame")
    as.treedata(d, ...)
}


##' @method as.treedata tbl_df
##' @export
as.treedata.tbl_df <- function(tree, ...) {
    edgelist <- as.tibble(tree)

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

        edge <- check_edgelist(edgelist)
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
