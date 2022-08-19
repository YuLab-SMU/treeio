##' @importFrom tidytree child
##' @method child phylo
##' @export
child.phylo <- function(.data, .node, type = 'children', ...) {
    res <- offspring(.data=.data, .node = .node, type = type)
    return(res)
}

##' @method child treedata
##' @export
child.treedata <- function(.data, .node, type = 'children', ...) {
    child.phylo(as.phylo(.data), .node, type = type, ...)
}

.internal.child <- function(data, node, type = 'children'){
    if (!is_numeric(node)){
        all.labs <- c(data$tip.label, data$node.label)
        names(all.labs) <- seq_len(length(all.labs))
        node <- names(all.labs[all.labs %in% node])
    }
    edge <- data$edge
    res <- edge[edge[,1] == node, 2]
    if (type != 'children'){
        alltips <- edge[,2][! edge[,2] %in% edge[,1]]
        w <- which(res >= length(alltips))
        if(length(w)>0){
            for(i in 1:length(w)){
                res <- c(res,
                         .internal.child(
                           data = data,
                           node = res[w[i]],
                           type = type
                         )
                       )
            }
        }
        if (type %in% c('tips', 'external')){
            res <- res[res %in% alltips]
        }else if(type == 'internal'){
            res <- res[!res %in% alltips]
        }
    }
    return(unname(res))
}

##' @importFrom tidytree offspring
##' @method offspring phylo
##' @export
offspring.phylo <- function(.data, .node, tiponly = FALSE, self_include = FALSE, type = 'all', ...){
    type <- match.arg(type, c("children", 'tips', 'internal', 'external', 'all'))

    if (tiponly){
        message('The "tiponly = TRUE" can be replaced by type="tips".')
        type = 'tips'
    }

    res <- lapply(.node, .internal.child, data = .data, type = type)
    if (length(res) <= 1){
        res <- unlist(res)
        if (self_include){
            res <- c(.node, res)
        }
    }else{
        if (self_include){
            res <- mapply(append, .node, res, SIMPLIFY=FALSE)
        }
        names(res) <- .node
    }
    return (res)
    #if (self_include) {
    #    sp <- .node
    #} else {
    #    sp <- child(.data, .node)
    #}

    #sp <- sp[sp != 0]
    #if (length(sp) == 0) {
    #    return(sp)
    #    ## stop("input node is a tip...")
    #}
    #i <- 1
    #while (i <= length(sp)) {
    #    sp <- c(sp, child(.data, sp[i]))
    #    sp <- sp[sp != 0]
    #    i <- i + 1
    #}
    #if (tiponly) {
    #    return(sp[sp <= Ntip(.data)])
    #}
    #return(sp)
}


##' @method offspring treedata
##' @export
offspring.treedata <- function(.data, .node, tiponly = FALSE, self_include = FALSE, type = 'all', ...) {
    offspring.phylo(as.phylo(.data), .node,
                    tiponly = tiponly, self_include = self_include, 
                    type = type,
                    ...)
}
