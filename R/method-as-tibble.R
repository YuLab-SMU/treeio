##' @method as_tibble pvclust
##' @export
as_tibble.pvclust <- function(x, ...) {
    tidytree::as_tibble(as.treedata(x, ...))
}

