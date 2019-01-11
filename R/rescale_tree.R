
##' @importFrom tidytree get_tree_data
set_branch_length <- function(tree_object, branch.length) {
    if (branch.length == "branch.length") {
        return(tree_object)
    } else if (branch.length == "none") {
        tree_object@phylo$edge.length <- NULL
        return(tree_object)
    }

    if (is(tree_object, "phylo")) {
        return(tree_object)
    }

    tree_anno <- get_tree_data(tree_object)
    tree_anno$node <- as.integer(tree_anno$node)

    phylo <- as.phylo(tree_object)

    cn <- colnames(tree_anno)
    cn <- cn[!cn %in% c('node', 'parent')]

    length <- match.arg(branch.length, cn)

    if (all(is.na(as.numeric(tree_anno[[length]])))) {
        stop("branch.length should be numerical attributes...")
    }

    edge <- phylo$edge
    colnames(edge) <- c("parent", "node")
    edge <- as_tibble(edge)

    dd <- full_join(edge, tree_anno, by = "node")

    dd <- dd[match(edge[['node']], dd[['node']]),]
    len <- unlist(dd[[length]])
    len <- as.numeric(len)
    len[is.na(len)] <- 0

    phylo$edge.length <- len

    tree_object@phylo <- phylo
    return(tree_object)
}


##' rescale branch length of tree object
##'
##' 
##' @title rescale_tree
##' @param tree_object tree object
##' @param branch.length numerical features (e.g. dN/dS)
##' @return update tree object
##' @export
##' @author Guangchuang Yu
rescale_tree <- function(tree_object, branch.length) {
    tree_object <- set_branch_length(tree_object, branch.length)
    return(tree_object)
}

