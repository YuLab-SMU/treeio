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
#'
#' @details This function will take a tree and a specified node from
#' that tree and subset the tree showing all relatives back to a specified
#' number of nodes. This function allows for a combination of
#' \link{\code{ancestor}} and \link{\code{offspring}} to return a subsetted
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
tree_subset <- function(tree, node, levels_back = 5, group_node = TRUE){
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
#' @export
tree_subset.phylo <- function(tree, node, levels_back = 5, group_node = TRUE){
  # error catching to ensure the tree input is of class phylo
  # if (class(tree) %in% c("phylo", "treedata")) {
  #   tree_df <- tidytree::as_data_frame(tree)
  # } else {
  #   stop("tree must be of class 'phylo'")
  # }

  # error catching to ensure the levels_back input is numeric
  # or can be converted to numeric
  if (!is.numeric(levels_back)) {
    levels_back <- as.numeric(levels_back)
    if (is.na(levels_back)) stop("'levels_back' must be of class numeric")
  }

  tree_df <- tidytree::as_data_frame(tree)

  # This pipeline returns the tip labels of all nodes related to
  # the specified node
  #
  # The tail/head combo isolates the base node of the subsetted tree
  # as the output from ancestor lists the closest parent nodes of a
  # given node from the bototm up.
  #
  # It then finds all of the offspring of that parent node. From there
  # it filters to include only tip and then pulls the labels.
  subset_labels <- tidytree::ancestor(tree_df, node) %>%
    tail(levels_back) %>%
    head(1) %>%
    dplyr::pull(node) %>%
    tidytree::offspring(tree_df, .) %>%
    dplyr::filter(!node %in% parent) %>%
    dplyr::pull(label)

  # This finds the nodes associated with the labels pulled
  subset_nodes <- which(tree$tip.label %in% subset_labels)


  # This drops all of the tips that are not included in group_nodes
  subtree <- treeio:::drop.tip(tree, tree$tip.label[-subset_nodes], rooted = TRUE)

  if (group_node) subtree <- treeio:::groupOTU.phylo(subtree, .node = node)


  return(subtree)
}


#' @method tree_subset treedata
#' @rdname tree_subset
#' @importFrom magrittr %>%
#'
#' @export
tree_subset.treedata <- function(tree, node, levels_back = 5, group_node = TRUE){
  # error catching to ensure the levels_back input is numeric
  # or can be converted to numeric
  if (!is.numeric(levels_back)) {
    levels_back <- as.numeric(levels_back)
    if (is.na(levels_back)) stop("'levels_back' must be of class numeric")
  }

  tree_df <- tidytree::as_data_frame(tree)


  subset_labels <- tidytree::ancestor(tree_df, node) %>%
    tail(levels_back) %>%
    head(1) %>%
    dplyr::pull(node) %>%
    tidytree::offspring(tree_df, .) %>%
    dplyr::filter(!node %in% parent) %>%
    dplyr::pull(label)

  subset_nodes <- which(tree@phylo$tip.label %in% subset_labels)

  subtree <- treeio:::drop.tip(tree, tree@phylo$tip.label[-subset_nodes], rooted = TRUE)

  if (group_node) subtree@phylo <- groupOTU(subtree@phylo, node)

  return(subtree)

}


