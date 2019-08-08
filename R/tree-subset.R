#' Subset tree objects by related nodes
#'
#' This function allows for a tree object to be subset by specifying a
#' node and returns all related nodes within a selected number of
#' levels
#'
#' @param tree a tree object of class phylo
#' @param node either a tip label or a node number for the given
#' tree that will be the focus of the subsetted tree
#' @param levels_back a number specifying how many nodes back from
#' the selected node the subsetted tree should include
#' @param group_node whether add grouping information of selected node
#' @param group_name group name (default 'group') for storing grouping information if group_node = TRUE
#' @param root_edge If TRUE (by default), set root.edge to path length of orginal root to the root of subset tree
#'
#' @details This function will take a tree and a specified node from
#' that tree and subset the tree showing all relatives back to a specified
#' number of nodes. This function allows for a combination of
#' \code{ancestor} and \code{offspring} to return a subsetted
#' tree that is of class phylo. This allows for easy graphing of the tree
#' with \code{ggtree}
#'
#' @examples
#' \dontrun{
#'   nwk <- system.file("extdata", "sample.nwk", package="treeio")
#'   tree <- read.tree(nwk)
#'
#'   sub_tree <- tree_subset(tree, node = "A", levels_back = 3)
#'   ggtree(sub_tree) + geom_tiplab() + geom_nodelab()
#' }
#'
#' @rdname tree_subset
#' @export
tree_subset <- function(tree, node, levels_back = 5, group_node = TRUE,
                        group_name = "group", root_edge = TRUE){
  UseMethod("tree_subset")
}


#' @method tree_subset phylo
#' @rdname tree_subset
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#'   nwk <- system.file("extdata", "sample.nwk", package="treeio")
#'   tree <- read.tree(nwk)
#'
#'   sub_tree <- tree_subset(tree, node = "A", levels_back = 3)
#'   ggtree(sub_tree) + geom_tiplab() + geom_nodelab()
#' }
#'
#' @importFrom utils tail
#' @importFrom utils head
#' @importFrom rlang quo .data
#' @export
tree_subset.phylo <- function(tree, node, levels_back = 5, group_node = TRUE,
                              group_name = "group", root_edge = TRUE){
 
    x <- tree_subset_internal(tree = tree, node = node, levels_back = levels_back, root_edge = root_edge)


    ## This drops all of the tips that are not included in group_nodes
    subtree <- drop.tip(tree, tree$tip.label[-x$subset_nodes], rooted = TRUE)

    if (group_node) subtree <- groupOTU.phylo(subtree, .node = x$group_labels, group_name = group_name)

    subtree$root.edge <- x$root.edge

    return(subtree)
}

##' @importFrom dplyr filter
##' @importFrom dplyr pull
##' @importFrom dplyr bind_rows
tree_subset_internal <- function(tree, node, levels_back = 5, root_edge = TRUE) {

    ## error catching to ensure the tree input is of class phylo
    ## if (class(tree) %in% c("phylo", "treedata")) {
    ##   tree_df <- tidytree::as_tibble(tree)
    ## } else {
    ##   stop("tree must be of class 'phylo'")
    ## }

    ## error catching to ensure the levels_back input is numeric
    ## or can be converted to numeric
    if (!is.numeric(levels_back)) {
        levels_back <- as.numeric(levels_back)
        if (is.na(levels_back)) stop("'levels_back' must be of class numeric")
    }

    tree_df <- tidytree::as_tibble(tree)

    selected_node <- node

    is_tip <- tree_df %>%
      dplyr::mutate(isTip = !.data$node %in% .data$parent) %>%
      dplyr::filter(.data$node == selected_node | .data$label == selected_node) %>%
      dplyr::pull(.data$isTip)

    if (is_tip & levels_back == 0){
      stop("The selected node (", selected_node, ") is a tip. 'levels_back' must be > 0",
           call. = FALSE)
    }

    if (is_tip) {
      group_labels <- tree_df %>%
        dplyr::filter(.data$node == selected_node | .data$label == selected_node) %>%
        dplyr::pull(.data$label)
    } else {
      group_labels <- tree_df %>%
        tidytree::offspring(selected_node) %>%
        dplyr::filter(!.data$node %in% .data$parent) %>%
        dplyr::pull(.data$label)
    }

    ## This pipeline returns the tip labels of all nodes related to
    ## the specified node
    ##
    ## The tail/head combo isolates the base node of the subsetted tree
    ## as the output from ancestor lists the closest parent nodes of a
    ## given node from the bototm up.
    ##
    ## It then finds all of the offspring of that parent node. From there
    ## it filters to include only tip and then pulls the labels.

    if (levels_back == 0) {
        new_root_node <- selected_node
    } else {
        new_root_node <- tidytree::ancestor(tree_df, selected_node) %>%
            tail(levels_back) %>%
            head(1) %>%
            dplyr::pull(.data$node)
    }

    subset_labels <- tidytree::offspring(tree_df, new_root_node) %>%
        dplyr::filter(!.data$node %in% .data$parent) %>%
        dplyr::pull(.data$label)

    ## This finds the nodes associated with the labels pulled
    subset_nodes <- which(tree$tip.label %in% subset_labels)

    root.edge <- NULL
    if (is.null(tree$edge.length)) {
        root_edge <- FALSE
        ## if not branch length info, no need to determine root.edge
    }

    if (root_edge) {
        root.edge <- ancestor(tree_df, new_root_node) %>%
            bind_rows(dplyr::filter(tree_df, node == new_root_node)) %>%
            pull(.data$branch.length) %>%
            sum(na.rm = TRUE)
        if (root.edge == 0)
            root.edge <- NULL
    }

    return(list(
        subset_nodes = subset_nodes,
        new_root_node = new_root_node,
        group_labels = group_labels,
        root.edge = root.edge
    ))
}

#' @method tree_subset treedata
#' @rdname tree_subset
#' @importFrom magrittr %>%
#'
#' @export
tree_subset.treedata <- function(tree, node, levels_back = 5, group_node = TRUE,
                                 group_name = "group", root_edge = TRUE){

    x <- tree_subset_internal(tree = tree@phylo, node = node, levels_back = levels_back)

    subtree <- drop.tip(tree, tree@phylo$tip.label[-x$subset_nodes], rooted = TRUE)

    if (group_node) subtree <- groupOTU(subtree, .node = x$group_labels, group_name = group_name)

    subtree@phylo$root.edge <- x$root.edge

    return(subtree)
}


