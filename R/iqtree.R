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
    phylo <- read.tree(text = find_newick(treetext))
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

## a newick tree is enclosed in a pair of parentheses and is terminated by a
## semicolon. The report file of IQ-TREE (*.iqtree) is not a newick file, and
## handing it over to read.tree() returned a meaningless tree or even crashed
## the R session, #98
find_newick <- function(treetext) {
    is_newick <- function(x) {
        x <- trimws(x)
        if (!grepl("^\\(", x) || !grepl(";$", x))
            return(FALSE)
        nopen <- lengths(regmatches(x, gregexpr("(", x, fixed = TRUE)))
        nclose <- lengths(regmatches(x, gregexpr(")", x, fixed = TRUE)))
        return(nopen == nclose && nopen > 0)
    }

    res <- treetext[vapply(treetext, is_newick, logical(1))]
    if (length(res) > 0)
        return(res)

    ## the newick string may be wrapped over multiple lines
    txt <- paste(treetext, collapse = "")
    if (is_newick(txt))
        return(txt)

    stop("cannot find a Newick tree in the input file. ",
         "Please note that 'read.iqtree' parses the tree file ",
         "(e.g. '*.treefile') and not the report file ('*.iqtree') ",
         "of an IQ-TREE run.")
}
