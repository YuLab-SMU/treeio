##' parse output from phyloT
##'
##'
##' @title read.phyloT
##' @param file newick tree file
##' @param ... additional parameters to read.tree
##' @return phylo object
##' @references \url{http://phylot.biobyte.de/}
##' @export
##' @author guangchuang yu
read.phyloT <- function(file, ...) {
    x <- readLines(file)
    x <- paste0(gsub("\\s+", "", x), collapse="")
    x <- sub("^\\(", "", x) %>% sub("\\);", ";", .)
    res <- tryCatch(read.tree(text=x, ...), error=function(e) NULL)
    return(res)

    ## it seems not ape::read.tree is capable to handle phyloT output
    ## since it supports singleton.

    ## if (is.null(res)) {
    ## msg <- paste("`read.phyloT` only supports newick format with setting of",
    ##              "`Internal nodes` to `collapsed`, and `Polytomy` to `No`.",
    ##              "\nURL: http://phylot.biobyte.de/")
    ## stop(msg)
    ## }
}

