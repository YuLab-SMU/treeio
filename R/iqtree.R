##' parse IQ-TREE output
##'
##'
##' @title read.iqtree
##' @param file IQ-TREE Newick text
##' @return treedata object
##' @details `read.iqtree()` parses the tree file of an IQ-TREE run (e.g.
##' `*.treefile`, the report file `*.iqtree` is not a tree file) and splits the
##' node labels into the SH-aLRT and the UFBoot support values. The two values
##' are expected to be separated by a `/`, which is what IQ-TREE writes when
##' both were requested. If the branch support was assessed with a single
##' method (e.g. the standard non-parametric bootstrap or SH-aLRT alone), the
##' node label holds one value only and there is no way to tell which method
##' produced it; the value is then stored in both columns and a message is
##' emitted. Use `read.newick()` to import such a tree without guessing.
##' @export
##' @author Guangchuang Yu
read.iqtree <- function(file) {
    treetext <- readLines(file, warn = FALSE)
    phylo <- read.tree(text = find_newick(treetext))
    nlabel <- phylo$node.label
    if (is.null(nlabel)) {
        ## a tree without any branch support, e.g. the output of a plain ML
        ## search, used to fail with a confusing tibble length error
        sh <- uf <- rep(NA_real_, phylo$Nnode)
    } else {
        if (!any(grepl("/", nlabel, fixed = TRUE))) {
            ## the branch support was assessed with a single method and
            ## the values cannot be attributed, #114
            message("the node labels do not contain '/', the support values ",
                    "cannot be split into SH-aLRT and UFBoot and the same ",
                    "values are stored in both columns; use 'read.newick()' ",
                    "to import the tree as it is.")
        }
        sh <- sub("/.*", "", nlabel) %>% as.numeric
        uf <- sub(".*/", "", nlabel) %>% as.numeric
    }

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
is_newick <- function(x) {
    x <- trimws(x)
    if (!grepl("^\\(", x) || !grepl(";$", x))
        return(FALSE)
    nopen <- lengths(regmatches(x, gregexpr("(", x, fixed = TRUE)))
    nclose <- lengths(regmatches(x, gregexpr(")", x, fixed = TRUE)))
    return(nopen == nclose && nopen > 0)
}

find_newick <- function(treetext, hint = NULL) {
    res <- treetext[vapply(treetext, is_newick, logical(1))]
    if (length(res) > 0)
        return(res)

    ## the newick string may be wrapped over multiple lines
    txt <- paste(treetext, collapse = "")
    if (is_newick(txt))
        return(txt)

    if (is.null(hint)) {
        hint <- paste0("Please note that 'read.iqtree' parses the tree file ",
                       "(e.g. '*.treefile') and not the report file ",
                       "('*.iqtree') of an IQ-TREE run.")
    }
    stop("cannot find a Newick tree in the input file. ", hint)
}
