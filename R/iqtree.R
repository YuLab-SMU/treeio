##' parse IQ-TREE output
##'
##'
##' @title read.iqtree
##' @param file IQ-TREE Newick text
##' @return treedata object
##' @export
##' @author Guangchuang Yu
read.iqtree <- function(file) {
    treetext <- readLines(file, warn = FALSE)
    phylo <- read.tree(text = treetext)
    nlabel <- phylo$node.label
    sh <- sub("/.*", "", nlabel) %>% as.numeric
    uf <- sub(".*/", "", nlabel) %>% as.numeric

    d <- tibble(node = Ntip(phylo) + 1:phylo$Nnode,
                SH_aLRT = sh,
                UFboot = uf)

    new("treedata",
        file = filename(file),
        treetext = treetext,
        phylo = phylo,
        data = d
        )
}
