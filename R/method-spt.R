##' @name spt
##' @rdname spt-methods
##' @title spt method
##' @param x a igraph object
##' @param from a specific node of network.
##' @param to other nodes of the network, length of it must 
##' be larger than 2.
##' @param weights a numeric vector giving edge weights or a character.
##' If this is \code{NULL} and the graph has a \code{weight} edge attribute, 
##' then the attribute is used. If this is \code{NA} then no weights 
##' are used even if the graph has a \code{weight} attribute. If this is a
##' character, the graph has the edge attribute which is numeric, then it 
##' will be used, default is \code{NULL}.
##' @param ... additional parameters
##' @return phylo object
##' @export
##' @examples
##' library(igraph)
##' set.seed(123)
##' g <- igraph::sample_gnp(100, .1) %>% 
##'      set_edge_attr(name='weight', value=abs(rnorm(E(.),3)))
##' tr1 <- spt(g, from = 6, to=V(g), weights = 'weight')
##' tr1
##' tr2 <- spt(g, from = 6, to = V(g), weights = NA)
##' tr2
spt <- function(x, from, to, weights = NULL, ...){
    UseMethod('spt')
}


#' @method spt igraph
#' @export
spt.igraph <- function(x, from, to, weights = NULL, ...){
    .internal.spt(x, from, to, weights, ...)
}

##' find the hierarchical cluster analysis among the nodes of graph
##' based on the length of all the shortest paths in the graph.
##' @param x a igraph object
##' @param graph.mst logical whether obtain the minimum spanning tree first
##' then find.hclust, default is FALSE.
##' @param weights a numeric vector giving edge weights or a character.
##' If this is \code{NULL} and the graph has a \code{weight} edge attribute,
##' then the attribute is used. If this is \code{NA} then no weights
##' are used even if the graph has a \code{weight} attribute. If this is a
##' character, the graph has the edge attribute which is numeric, then it
##' will be used, default is \code{NULL}.
##' @param hclust.method the agglomeration method to be used, This should be (an
##' unambiguous abbreviation of) one of \code{"ward.D"}, \code{"ward.D2"},
##' \code{"single"}, \code{"complete"}, \code{"average"} (= UPGMA), \code{"mcquitty"}
##' (= WPGMA), \code{"median"} (= WPGMC) or \code{"centroid"} (= UPGMC). 
##' @param ... additional parameters
##' @return hclust object
##' @export
##' @examples
##' library(igraph)
##' set.seed(123)
##' g <- igraph::sample_gnp(100, .1) %>%
##'      set_edge_attr(name='weight', value=abs(rnorm(E(.),3)))
##' tr1 <- find.hclust(g, weights = NA)
##' tr2 <- find.hclust(g)
##' tr3 <- find.hclust(g, graph.mst = TRUE)
find.hclust <- function(x, graph.mst = FALSE, weights = NULL, hclust.method = 'average', ...){
    UseMethod('find.hclust')
}

##' @method find.hclust igraph
##' @importFrom stats as.dist hclust
##' @importFrom rlang check_installed
##' @export
find.hclust.igraph <- function(
  x,
  graph.mst = FALSE,
  weights = NULL,
  hclust.method = 'average',
  ...
  ){
    check_installed('igraph', 'for `find.hclust()`.')
    edge.attr.list <- igraph::edge_attr(x)
    wg.nm <- 'weight'
    if (is.numeric(weights) && length(weights) == igraph::ecount(x)){
        x <- igraph::set_edge_attr(x, name='weight', value = weights)
        weights <- NULL
    }else if (is.character(weights) && weights %in% names(edge.attr.list) && is.numeric(edge.attr.list[[weights]])){
        x <- igraph::set_edge_attr(x, name = 'weight', value = edge.attr.list[[weights]])
        weights <- NULL
    }else if (is.null(weights)){
        if (!('weight' %in% colnames(edge.attr.list) && is.numeric(edge.attr.list[['weight']]))){
            wg.nm <- NULL
        }
    }else{
        wg.nm <- NULL
        weights <- NA
    }

    if (graph.mst){
       x <- igraph::mst(x, weights = weights)
       if (!is.null(wg.nm)){
           weights <- igraph::edge_attr(x)[[wg.nm]]
       }
    }

    d <- igraph::distances(x, weights = weights, ...)

    hc <- hclust(as.dist(d), method = hclust.method)
    return(hc)
}


.internal.spt <- function(x, from, to, weights = NULL, ...){
    check_installed('igraph', 'for `spt()`.')
    edge <- igraph::as_data_frame(x)
    flag.weight <- FALSE
    if (is.numeric(weights) && length(weights)==nrow(edge)){
        flag.weight <- TRUE
        weights <- weights
    }else if (is.character(weights) && weights %in% colnames(edge) && is.numeric(edge[[weights]])){
        flag.weight <- TRUE
        weights <- edge[[weights]]
    }else if (is.null(weights) && 'weight' %in% colnames(edge)){
        flag.weight <- TRUE
        weights <- edge[['weight']]
    }
    res <- igraph::shortest_paths(x, from, to, output = 'epath', weights = weights, ...)
    ind <- res$epath |>
        lapply(as.numeric) |>
        unlist() |>
        unique()
    edge <- edge[ind,c(1,2)] |> as.matrix() 
    keep.ind <- !duplicated(edge)
    if (flag.weight){
        weights <- weights[ind]
        weights <- weights[keep.ind]
    }
    edge <- edge[keep.ind,]
    if (nrow(edge) <=1){
        cli::cli_abort("the shortest path tree can not be found!")
    }
    if (any(duplicated(edge[,2]))){
        edge <- .adjust.tree.network.edge(edge)
    }
    if (flag.weight){
        edge <- cbind(edge, branch.length = weights)
        tr <- as.phylo(edge, branch.length = 'branch.length')
    }else{
        tr <- as.phylo(edge)
    }
    return(tr)
}
