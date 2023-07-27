##' @importFrom ape as.phylo
##' @export
ape::as.phylo


##' @method as.phylo tbl_df
##' @importFrom tibble as_tibble
##' @importFrom dplyr mutate_if
##' @export
as.phylo.tbl_df <- function(x, branch.length, label, ...) {
    x <- data.frame(x) %>% mutate_if(is.factor, as.character)
    branch.length <- rlang::enquo(branch.length)
    label <- rlang::enquo(label)
    length_var <- root.edge <- edge.length <- NULL
    tip.label <- node.label <- labels <- NULL
    if (nrow(unique(x[, 1, drop=FALSE])) > nrow(unique(x[, 2, drop=FALSE]))){
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

##' @method as.phylo data.frame
##' @export
as.phylo.data.frame <- as.phylo.tbl_df

##' @method as.phylo matrix
##' @export
as.phylo.matrix <- as.phylo.tbl_df



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
    if (!igraph::is_tree(x)){
        cli::cli_abort("The graph is not a root graph.")
    }
    edge <- igraph::get.edgelist(x)
    trash <- try(
       silent = TRUE,
       expr = {
          x <- as.phylo(edge)
       }   
    )

    if (inherits(trash, 'try-error')){
        trash <- try(
            silent = TRUE,
            expr = {
               x <- .as.phylo.rev.edges(edge)
            }
        )
    }

    if (inherits(trash, 'try-error')){
        cli::cli_abort("The igraph is a network not a tree graph.")    
    }else{
        return(x) 
    }

}


.as.phylo.rev.edges <- function(x){
    x <- unique(x)
    if (any(duplicated(x[,2]))){
        x <- .adjust.tree.network.edge(x)
    }
    x <- as.phylo(x)
    return(x)
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


##' @method as.phylo dendro
##' @export
as.phylo.dendro <- function(x, ...){
    seg.da <- x$segments
    lab.da <- x$labels
    inode.da <- seg.da[,c(1, 2)] |>
                dplyr::bind_rows(
                  seg.da[,c(3, 4)] |>
                  dplyr::rename(x="xend", y="yend")
                )
    root.index <- apply(inode.da, 1,
                    function(x)!any(x[[1]] == seg.da$xend & x[[2]] == seg.da$yend),
                    simplify=F) |>
              unlist(use.names=F)
    root.da <- unique(inode.da[root.index, ])

    inode.da <- inode.da[apply(inode.da, 1, 
                               function(x)sum(x[[1]]==inode.da[,1] & x[[2]]==inode.da[,2])) != 2,]

    dt <- unique(rbind(lab.da[,c(1, 2)], root.da, inode.da))
    rownames(dt) <- seq_len(nrow(dt))

    parent.id <- do.call('rbind',
        apply(dt, 1, function(x)check.inode(x=x, y=dt, z=seg.da, root.id=root.da), simplify=FALSE)
    )

    edge <- cbind(as.numeric(parent.id), as.numeric(rownames(dt)))
    edge <- edge[!edge[,1]==edge[,2],]

    tr <- structure(
            .Data = list(
               edge = edge,
               edge.length = abs(dt[edge[,2],2] - dt[edge[,1],2]),
               tip.label = lab.da$label,
               Nnode = nrow(dt) - nrow(lab.da)
            ),
            class = 'phylo'
          )
    
    return(tr)
}



check_edgelist <- function(edgelist) {
    if (dim(edgelist)[2] < 2)
        stop("input should be a matrix of edge list that holds the relationships in the first two columns")
    if (length(unique(edgelist[,1,drop=TRUE])) > length(unique(edgelist[,2,drop=TRUE]))) {
        children <- edgelist[,1,drop=TRUE]
        parents <- edgelist[,2,drop=TRUE]
    } else {
        children <- edgelist[,2,drop = TRUE]
        parents <- edgelist[,1,drop = TRUE]
    }
    root1 <- unique(parents[!(parents %in% children)])
    root2 <- unique(parents[parents == children])
    if ((length(root1) != 1 && length(root2) != 1 )|| any(duplicated(children)))
        stop("Cannot find root. network is not a tree!")
    if (length(root1) != 1 && length(root2) == 1){
        indx <- parents != children
        parents <- parents[indx]
        children <- children[indx]
        edge <- matrix(c(parents, children), ncol=2)
        attr(edge, "indx") <- indx
    }else{
        edge <- matrix(c(parents, children), ncol=2)
    }
    return (edge)
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
check.inode <- function(x, y, z, root.id){
    check.root <- x[[1]] == root.id[[1]] && x[[2]] == root.id[[2]]
    if (check.root){
        nodeID <- rownames(y[x[[1]]==y[,1] & x[[2]]==y[,2], ])
        return(nodeID)
    }
    tmp <- z[x[[1]]==z[,3] & x[[2]] == z[,4],]
    x <- tmp[,c(1,2)]
    nodeID <- rownames(y[x[[1]] == y[,1] & x[[2]] == y[,2],])
    check.root <- x[[1]] == root.id[[1]] && x[[2]] == root.id[[2]]
    if (length(nodeID)>0 || check.root){
        return(nodeID)
    }else{
        check.inode(x=x, y=y, z=z, root.id=root.id)
    }
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


.adjust.tree.network.edge <- function(x){
    tip.nodes <- names(which(table(x) == 1))
    if (.check.no.tree.network(x, tip.nodes)){
        cli::cli_abort("The edge is not a edge of tree.")
    }
    x <- .rev.edge(x, tip.nodes, index = 1)
    internal.nodes <- names(which(table(x[,2])>1))
    while(length(internal.nodes) > 0){
        x <- .rev.edge(x, internal.nodes, index = 2)
        internal.nodes <- names(which(table(x[,2])>1))
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
