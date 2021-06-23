##' drop.tip method
##'
##'
##' @rdname drop.tip-methods
##' @aliases drop.tip,treedata
##' @exportMethod drop.tip
##' @author Casey Dunn \url{http://dunnlab.org}  and Guangchuang Yu \url{https://guangchuangyu.github.io}
##' @usage drop.tip(object, tip, ...)
##' @examples
##' nhxfile <- system.file("extdata/NHX", "ADH.nhx", package="treeio")
##' nhx <- read.nhx(nhxfile)
##' drop.tip(nhx, c("ADH2", "ADH1"))
setMethod("drop.tip", signature(object="treedata"),
          function(object, tip, ...) {
              drop.tip.treedata(object, tip, ...)
          })

drop.tip.treedata <- function(object, tip, ...){
    params <- list(...)
    if ("interactive" %in% names(params) && params$interactive){
        message("The interactive mode is not implemented for treedata object!")
        params$interactive <- FALSE
    }
    res <- build_new_labels(tree=object)
    tree <- res$tree
    old_and_new <- res$node2old_new_lab
    if(is.character(tip)){
        tip <- old_and_new[old_and_new$old %in% tip, "new"] %>% unlist(use.names=FALSE)
    }
    params$phy <- tree
    params$tip <- tip
    new_tree <- do.call(ape::drop.tip, params)
    
    trans_node_data <- old_new_node_mapping(tree, new_tree)
    object@phylo <- build_new_tree(tree=new_tree, node2old_new_lab=old_and_new)

    update_data <- function(data, trans_node_data) {
        data <- data[match(trans_node_data$old, data$node),]
        data$node <- trans_node_data$new
        return(data)    
    }

    if (nrow(object@data) > 0) {
        object@data <- update_data(object@data, trans_node_data)
    }

    if (nrow(object@extraInfo) > 0) {
        object@extraInfo <- update_data(object@extraInfo, trans_node_data)
    }
    return (object)
}

##' @rdname drop.tip-methods
##' @exportMethod drop.tip
##' @aliases drop.tip,phylo
##' @source
##' drop.tip for phylo object is a wrapper method of ape::drop.tip
##' from the ape package. The documentation you should
##' read for the drop.tip function can be found here: \link[ape]{drop.tip}
##'
##' @seealso
##' \link[ape]{drop.tip}
setMethod("drop.tip", signature(object="phylo"),
          function(object, tip, ...){
              ape::drop.tip(object, tip, ...)
          })
