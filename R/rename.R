##' rename tip label of phylogenetic tree
##'
##'
##' @title rename_taxa
##' @param tree tree object, either treedata or phylo
##' @param data data frame
##' @param key column in data that match tip label (use 1st column by default)
##' @param value column in data for rename tip label (use 2nd column by default)
##' @importFrom rlang enexpr
##' @importFrom rlang quo_name
##' @export
##' @return tree object
##' @examples
##' tree <- rtree(3)
##' d <- data.frame(old = paste0('t', 1:3), new = LETTERS[1:3])
##' rename_taxa(tree, d)
##' rename_taxa(tree, d, old, new)
##' @author Guangchuang Yu
rename_taxa <- function(tree, data, key = 1, value = 2) {
    phylo <- as.phylo(tree)

    key_var <- enexpr(key)
    value_var <- enexpr(value)

    if (!is.numeric(key_var))
        key_var <- quo_name(key_var)
    if (!is.numeric(value_var))
        value_var <- quo_name(value_var)

    idx <- match(phylo$tip.label, as.character(data[[key_var]]))
    if (anyNA(idx)) {
        stop("taxa name not match, please check your input...")
    }
    phylo$tip.label <- as.character(data[[value_var]][idx])

    if (!is(tree, "phylo")) {
        tree@phylo <- phylo
    } else {
        return(phylo)
    }
    return(tree)
}

