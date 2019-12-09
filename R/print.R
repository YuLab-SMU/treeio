##' print information of a list of treedata objects
##'
##'
##' @title print
##' @param x a list of treedata objects
##' @param ... no used
##' @return message
##' @method print treedataList
##' @export
print.treedataList <- function(x, ...) {
    msg <- paste(length(x), "phylogenetic trees")
    cat(msg, "\n")
}


