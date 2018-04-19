##' rename tip label of phylogenetic tree
##'
##'
##' @title rename_taxa
##' @param tree tree object, either treedata or phylo
##' @param data data frame
##' @param key column in data that match tip label
##' @param value column in data for rename tip label
##' @importFrom rlang enexpr
##' @importFrom rlang quo_name
##' @export
##' @return tree object
##' @author Guangchuang Yu
rename_taxa <- function(tree, data, key, value) {
    phylo <- as.phylo(tree)
    key_var <- quo_name(enexpr(key))
    value_var <- quo_name(enexpr(value))

    idx <- match(phylo$tip.label, as.character(data[[key_var]]))
    phylo$tip.label <- as.character(data[[value_var]][idx])

    if (!is(tree, "phylo")) {
        tree@phylo <- phylo
    } else {
        return(phylo)
    }
    return(tree)
}
