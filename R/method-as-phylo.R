##' @importFrom ape as.phylo
##' @export
ape::as.phylo


##' @method as.phylo tbl_df
##' @importFrom tibble as_tibble
##' @importFrom dplyr mutate_if
##' @export
as.phylo.tbl_df <- function(x, length, ...) {
    x <- as_tibble(x) %>% mutate_if(is.factor, as.character)

    edge.length <- NULL
    length_var <- quo_name(enexpr(length))

    if (length_var != "") {
        edge.length <- as.numeric(x[[length_var]])
    } else {
        length_var <- NULL
    }

    edge <- check_edgelist(x)
    indx <- attr(edge, "indx")
    if (!is.null(indx) && !is.null(edge.length)){
        edge.length <- edge.length[indx]
        attr(edge, "indx") <- NULL
    }
    phylo <- read.tree(text = .write.tree4(edge,
                                           id_as_label=TRUE,
                                           edge.length = edge.length),
                       ...)

    attr(phylo, "length_var") <- length_var
    return(phylo)
}

##' @method as.phylo data.frame
##' @export
as.phylo.data.frame <- as.phylo.tbl_df

##' @method as.phylo matrix
##' @export
as.phylo.matrix <- as.phylo.tbl_df


##' @method as.phylo phylo4
##' @export
as.phylo.phylo4 <- function(x, ...) {
    edge <- x@edge
    edge.filter <- edge[,1] != 0
    edge <- edge[edge.filter, ]
    edge.length <- x@edge.length
    edge.length <- edge.length[edge.filter]
    tip.id <- sort(setdiff(edge[,2], edge[,1]))
    tip.label <- x@label[tip.id]
    phylo <- list(edge = edge,
                  edge.length = edge.length,
                  tip.label = tip.label)

    node.id <- sort(unique(edge[,1]))
    node.id <- node.id[node.id != 0]
    node.label <- x@label[node.id]
    if (!all(is.na(node.label))) {
        phylo$node.label <- node.label
    }
    phylo$Nnode <- length(node.id)
    class(phylo) <- "phylo"
    return(phylo)
}


##' @method as.phylo pvclust
##' @export
as.phylo.pvclust <- function(x, ...) {
    as.phylo.hclust_node(x$hclust, ...)
}

##' @method as.phylo ggtree
##' @export
as.phylo.ggtree <- function(x, ...) {
    d <- as_tibble(x$data)
    class(d) <- c("tbl_tree", "tbl_df", "tbl", "data.frame")
    as.phylo(d)
}

##' @method as.phylo igraph
##' @export
as.phylo.igraph <- function(x, ...) {
    edge <- igraph::get.edgelist(x)
    as.phylo(edge)
}

##' @method as.phylo list
##' @export
as.phylo.list <- function(x, ...){
    max.depth <- purrr::vec_depth(x)
    while(max.depth > 1){
        trash = try(silent = TRUE,
                    expr = {
                       x <- purrr::map_depth(x, max.depth - 1, paste_nested_list)
                    }
                )
        max.depth <- max.depth - 1
    }
    x <- lapply(x, .paste0_)
    x <- .paste1_(x)
    x <- paste0(x, collapse=',')
    x <- paste0('(', x, ');')
    x <- ape::read.tree(text = x)
    return(x)
}

paste_nested_list <- function(x){
    if (length(x)>1){
        if (length(names(x))!=0){
            x <- paste0(paste0(x, names(x)), collapse=',')
        }else{
            x <- paste0("(", paste0(x, collapse=','), ")")
        }
    }else{
        if (!(grepl("^\\(", x) && grepl("\\)$", x))){
            x <- paste0('(', x, ')', names(x))
        }else{
            x <- paste0(x, names(x))
        }
    }
    return(x)
}

.paste0_ <- function(x){
    flag <- grepl("^\\(\\(", x) && grepl("\\)\\)$", x)
    if (flag){
        x <- gsub("^\\(\\(", "\\(", x)
        x <- gsub('\\)\\)$', "\\)", x)
    }else{
        x <- paste0('(', x, ')', names(x), collapse=',')
    }
    return(x)
}

.paste1_ <- function(x){
    index <- grepl('\\),', x)
    if (any(index)){
        x[index] <- paste0("(", x[index],")", names(x[index]))
        x[!index] <- paste0(x[!index], names(x[!index]))
    }else{
        x <- paste0(x, names(x))
    }
    return(x)
}

##' access phylo slot
##'
##'
##' @title get.tree
##' @param x tree object
##' @param ... additional parameters
##' @return phylo object
##' @export
##' @author Guangchuang Yu
get.tree <- function(x, ...) {
    as.phylo(x, ...)
}

