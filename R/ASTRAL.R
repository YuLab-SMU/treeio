##' parse ASTRAL output newick text
##'
##'
##' @title read.astral
##' @param file ASTRAL Newick file
##' @return treedata object
##' @export
##' @author Guangchuang Yu
##' @examples
##' tt <- paste0(
##'   "((species1,(species2,species3)'[pp1=0.75;pp2=0.24;pp3=0.01]':",
##'   "1.2003685744180805)'[pp1=0.98;pp2=0.02;pp3=0]':0.9679599282730038,",
##'   "((species4,species5)'[pp1=0.88;pp2=0.11;pp3=0.01]':1.2454851536484994))"
##' )
##' read.astral(textConnection(tt))
read.astral <- function(file) {
    treetext <- readLines(file, warn=FALSE)
    treetext <- gsub(";", "$", treetext)  %>%
        gsub("'\\[", "@", .)  %>%
        gsub("]'", "@", .)

    if (substring(treetext, nchar(treetext), nchar(treetext)) != ';')
        treetext <- paste0(treetext, ";")

    phylo <- read.tree(text = treetext)
    stats <- phylo$node.label %>%
        gsub("^@|@$", "", .) %>%
        gsub("\\$", ":", .) %>%
        get_nhx_feature %>%
        as_tibble

    stats$node <- Ntip(phylo) + 1:phylo$Nnode

    phylo$node.label <- NULL

    new("treedata",
        file = filename(file),
        phylo = phylo,
        data = stats
        )
}
