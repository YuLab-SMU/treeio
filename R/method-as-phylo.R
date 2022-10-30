##' @importFrom ape as.phylo
##' @export
ape::as.phylo


##' @method as.phylo tbl_df
##' @importFrom tibble as_tibble
##' @importFrom dplyr mutate_if
##' @export
as.phylo.tbl_df <- function(x, branch.length, label, ...) {
    x <- as_tibble(x) %>% mutate_if(is.factor, as.character)
    branch.length <- rlang::enquo(branch.length)
    label <- rlang::enquo(label)
    length_var <- root.edge <- edge.length <- NULL
    tip.label <- node.label <- labels <- NULL
    if (nrow(unique(x[, 1])) > nrow(unique(x[,2]))){
        x %<>% dplyr::select(rev(seq_len(2)), seq_len(ncol(x)))
    }

    if (!rlang::quo_is_missing(branch.length)){
        edge.length <- as.numeric(x %>% dplyr::pull(!!branch.length))
        length_var <- rlang::as_name(branch.length)
    }

    if (!rlang::quo_is_missing(label)){
        labels <- x %>% dplyr::pull(!!label) %>% as.character()
    }else{
        labels <- x %>% dplyr::pull(2) %>% as.character()
    }

    edge <- check_edgelist(x)

    indx <- attr(edge, "indx")
    if (is.null(indx)){
        indx <- c(FALSE, rep(TRUE, nrow(edge)))
        labels <- c(unique(edge[,1][!edge[,1] %in% edge[,2]]), labels)
        if (!is.null(edge.length)){
            edge.length <- c(0, edge.length)
        }
    }

    isTip <- !edge[,2] %in% edge[,1]

    index <- rep(NA, length(isTip))
    index[isTip] <- seq_len(sum(isTip))
    index[!isTip] <- seq(sum(isTip)+2, length(isTip)+1)
    mapping <- data.frame(node=index, labelnames=edge[,2], isTip)
    parent <- mapping[match(edge[,1], mapping$labelnames), "node"]
    child <- mapping[match(edge[,2], mapping$labelnames), 'node']
    edge <- as.matrix(cbind(parent, child))
    colnames(edge) <- NULL
    edge[is.na(edge)] <- sum(isTip) + 1

    if (!is.null(edge.length)){
        root.edge <- edge.length[!indx]
        if (length(root.edge)==0 || is.na(root.edge)){
            root.edge <- NULL
        }
        edge.length <- edge.length[indx]
    }

    if (!is.null(labels)){
        root.label <- labels[!indx]
        labels <- labels[indx]
        tip.label <- labels[isTip]
        node.label <- c(root.label, labels[!isTip])
        if (all(is.na(node.label))){
            node.label <- NULL
        }
    }

    Nnode <- length(unique(as.vector(edge))) - sum(isTip)
    phylo <- list(
                  edge = edge,
                  Nnode = Nnode,
                  tip.label = tip.label,
                  edge.length = edge.length
             )
    class(phylo) <- 'phylo'
    phylo$root.edge <- root.edge
    phylo$node.label <- node.label
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

