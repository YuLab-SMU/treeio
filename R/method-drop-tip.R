##' @rdname drop.tip-methods
##' @aliases drop.tip,treedata
##' @exportMethod drop.tip
##' @author Casey Dunn \url{http://dunnlab.org}  and Guangchuang Yu \url{https://guangchuangyu.github.io}
##' @examples
##' nhxfile <- system.file("extdata/NHX", "ADH.nhx", package="treeio")
##' nhx <- read.nhx(nhxfile)
##' tr1 <- drop.tip(nhx, c("ADH2", "ADH1"))
##' tr2 <- keep.tip(nhx, c("ADH2", "ADH1"))
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

    if (is.null(new_tree)){
        return(new_tree)
    }
    
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
##' @seealso
##' \link[ape]{drop.tip}
setMethod("drop.tip", signature(object="phylo"),
          function(object, tip, ...){
              ape::drop.tip(object, tip, ...)
          })

##' @rdname drop.tip-methods
##' @export
setMethod("keep.tip", signature(object = 'treedata'),
          function(object, tip, ...){
     .internal.keep.tip(object, tip, ...)  
  }
)

##' @rdname drop.tip-methods 
##' @export
setMethod('keep.tip', signature(object = 'phylo'),
  function(object, tip, ...){
    .internal.keep.tip(object, tip, ...)
})

.internal.keep.tip <- function(object, tip, ...){
    if (inherits(object, 'treedata')){
       tip.label <- object@phylo$tip.label
    }
    if (inherits(object, 'phylo')){
       tip.label <- object$tip.label
    }
    Ntip <- length(tip.label)
    if (is.character(tip)) {
        idx <- match(tip, tip.label)
        if (anyNA(idx)) {
            cli::cli_abort(
              "unmatched {.var tip} label/labels was/were found in the {.class object} object.",
              "Considering remove the it/them: ",
              paste(tip[is.na(idx)], collapse = " ")
            )
        }
        tip <- idx
    }else{
        out.of.range <- tip > Ntip
        if (any(out.of.range)) {
            cli::cli_warn("some tip numbers were larger than the number of tips: they were ignored")
            tip <- tip[!out.of.range]
        }
    }
    toDrop <- setdiff(1:Ntip, tip)
    drop.tip(object, toDrop, ...)
}
