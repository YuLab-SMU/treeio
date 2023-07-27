##' @name spt
##' @rdname spt-methods
##' @title spt method
##' @param x a igraph object
##' @param from a specific node of network.
##' @param to other nodes of the network, length of it must 
##' be larger than 2.
##' @param ... additional parameters
##' @return phylo object
##' @export
spt <- function(x, from, to, ...){
    UseMethod('spt')
}


#' @method spt igraph
#' @export
spt.igraph <- function(x, from, to, ...){
    .internal.spt(x, from, to, ...)
}

.internal.spt <- function(x, from, to, ...){
    edge <- igraph::get.edgelist(x)
    res <- igraph::shortest_paths(x, from, to, output = 'epath', ...)
    ind <- res$epath |>
        lapply(as.numeric) |>
        unlist() |>
        unique()
    edge <- edge[ind,] |> unique()
    if (nrow(edge) <=1){
        cli::cli_abort("the shortest path tree can not be found!")
    }
    if (any(duplicated(edge[,2]))){
        edge <- .adjust.tree.network.edge(edge)
    }
    as.phylo(edge)
}
