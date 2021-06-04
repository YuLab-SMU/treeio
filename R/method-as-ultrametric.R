#' @title as.ultrametric
#' @param tree tree object
#' @param ... additional parameters
#' @return treedata or phylo object
#' @export 
as.ultrametric <- function(tree, ...){
    UseMethod("as.ultrametric")
}

#' @method as.ultrametric phylo
#' @export
## reference 
## https://github.com/PuttickMacroevolution/MCMCtreeR/blob/master/R/readMCMCTree.R
as.ultrametric.phylo <- function(tree, ...){
    outer <- tree$edge[, 2]
    inner <- tree$edge[, 1]
    ntip <- Ntip(tree)
    totalPath <- c()
    tipindx <- which(outer <= ntip)
    for (i in tipindx) {
        start <- i
        end <- inner[start]
        edgeTimes <- tree$edge.length[start]
        while (end != inner[1]) {
            start <- which(outer == end)
            end <- inner[start]
            edgeTimes <- c(edgeTimes, tree$edge.length[start])
        }
        totalPath <- c(totalPath, sum(edgeTimes))
    }
    addLength <- max(totalPath) - totalPath
    tree$edge.length[tipindx] <- tree$edge.length[tipindx] + addLength
    return (tree)
}

#' @method as.ultrametric treedata
#' @export
as.ultrametric.treedata <- function(tree, ...){
    tree@phylo <- as.ultrametric(tree=tree@phylo,...)
    return (tree)
}

#' @method as.ultrametric tbl_tree
as.ultrametric.tbl_tree <- function(tree, ...){
    tree <- as.treedata(tree)
    tree <- as.ultrametric(tree)
    return(tree)
}
