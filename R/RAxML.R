##' parse RAxML bootstrapping analysis output
##'
##'
##' @title read.raxml
##' @param file RAxML bootstrapping analysis output
##' @return treedata object
##' @export
##' @examples
##' raxml_file <- system.file("extdata/RAxML", "RAxML_bipartitionsBranchLabels.H3", package="treeio")
##' read.raxml(raxml_file)
##' @author Guangchuang Yu
read.raxml <- function(file) {
    tree.text <- readLines(file, warn=FALSE)
    tree_text <- gsub('(:[0-9\\.eE+\\-]+)\\[(\\d+)\\]', '\\@\\2\\1', tree.text)
    phylo <- read.tree(text=tree_text)
    if(any(grepl('@', phylo$node.label))) {
        bootstrap <- suppressWarnings(
            as.numeric(gsub("[^@]*@(\\d+)", "\\1", phylo$node.label)) 
                   )
        phylo$node.label <- gsub("@\\d+", "", phylo$node.label)
    }

    if (all(phylo$node.label == "")) {
        phylo$node.label <- NULL
    }

    bootstrap <- tibble(node = Ntip(phylo) + 1:phylo$Nnode,
                        bootstrap = bootstrap)

    new("treedata",
        file = filename(file),
        treetext = tree.text,
        phylo = phylo,
        data = bootstrap
        )
}
