##' read newick tree
##'
##'
##' @title read.newick
##' @param file newick file
##' @param node.label parse node label as 'label' or 'support' value
##' @param ... additional parameter, passed to 'read.tree'
##' @return phylo or treedata object
##' @export
##' @author guangchuang yu
read.newick <- function(file, node.label = "label", ...) {
    node.label <- match.arg(node.label, c("support", "label"))
    tree <- read.tree(file, ...)
    if (node.label == "label")
        return(tree)

    df <- data.frame(node = nodeIds(tree), support = as.numeric(tree$node.label))
    tree$node.label <- NULL
    new("treedata",
        phylo = tree,
        data = df)
}


