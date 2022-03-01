##' @importFrom tidytree groupClade
##' @importFrom ape extract.clade
##' @method groupClade phylo
##' @export
groupClade.phylo <- function(.data, .node, group_name = "group", ...) {
    if (length(.node) == 1) {
        clade <- extract.clade(.data, .node)
        tips <- clade$tip.label
    } else {
        tips <- lapply(.node, function(x) {
            clade <- extract.clade(.data, x)
            clade$tip.label
        })
    }

    groupOTU(.data, tips, group_name)
}

##' @method groupClade treedata
##' @export
groupClade.treedata <- function(.data, .node, group_name = "group", ...) {
    .data@phylo <- groupClade(as.phylo(.data), .node, group_name, ...)
    .data
}




