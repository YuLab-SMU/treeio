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
    ## now ape::read.tree is capable to handle phyloT output
    ## since it supports singleton.

    stop("read.phyloT was deprecated, ",
         "please use read.tree")

    ## x <- readLines(file)
    ## x <- paste0(gsub("\\s+", "", x), collapse="")
    ## x <- sub("^\\(", "", x) %>% sub("\\);", ";", .)
    ## res <- tryCatch(read.tree(text=x, ...), error=function(e) NULL)

    ## if (is.null(res)) {
    ## msg <- paste("`read.phyloT` only supports newick format with setting of",
    ##              "`Internal nodes` to `collapsed`, and `Polytomy` to `No`.",
    ##              "\nURL: http://phylot.biobyte.de/")
    ## stop(msg)
    ## }

    ## return(res)
}

##' generate jplace file
##'
##'
##' @title write.jplace
##' @param nwk tree in newick format
##' @param data annotation data
##' @param outfile jplace output file
##' @return jplace file
##' @export
##' @author ygc
write.jplace <- function(nwk, data, outfile) {
    stop("write.jplace is deprecated, please use write.jtree")
}

