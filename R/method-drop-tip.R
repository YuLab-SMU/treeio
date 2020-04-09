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

drop.tip.treedata <- function(object, tip, ...) {
    ## Column name that has very low chance of collision
    ## with existing column
    node_label_name <- "cd8128f329f72c167a8028cf8"

    labels <- NULL
    if (!is.null(object@phylo$node.label)) {
        ## Tree has node labels. Put these in data
        ## for safe keeping and remove them from tree for now

        labels <- c(
            rep( NA, length( object@phylo$tip.label ) ),
            object@phylo$node.label
        )

        object@phylo$node.label <- NULL
    }

    ## label the internal tree nodes by their number
    object@phylo$node.label <- Ntip(object) + (1:Nnode(object))

    trans_node_data <- tibble(node = 1:Nnode(object, internal.only = FALSE),
                       node.label = c(object@phylo$tip.label,
                                      as.character(object@phylo$node.label)))
    if (!is.null(labels))
        trans_node_data[[node_label_name]] <- labels



    ## Will need to take different approaches for subsampling tips
    ## and internal nodes, add a column to make it easy to tell them apart
    trans_node_data$is_tip <- trans_node_data$node <= Ntip(object)


    ## Remove tips
    object@phylo <- ape::drop.tip( object@phylo, tip )


    ## Subsample the tags
    trans_node_data <- trans_node_data[trans_node_data$node.label %in%
                            (c(object@phylo$tip.label,
                               as.character(object@phylo$node.label))),]

    ## Update tip node numbers
    tip_nodes <- trans_node_data$node.label[ trans_node_data$is_tip ]
    trans_node_data$new_node <- NA
    trans_node_data$new_node[ trans_node_data$is_tip ] <- match(object@phylo$tip.label,
                                                                tip_nodes)

    internal_nodes <- trans_node_data$node.label[ !trans_node_data$is_tip ]
    trans_node_data$new_node[!trans_node_data$is_tip] <- match(object@phylo$node.label,
                                                               internal_nodes)+ Ntip(object@phylo)


    update_data <- function(data, trans_node_data) {
        data <- data[match(trans_node_data$node, data$node),]
        data$node <- trans_node_data$new_node
        return(data)
    }

    if (nrow(object@data) > 0) {
        object@data <- update_data(object@data, trans_node_data)
    }

    if (nrow(object@extraInfo) > 0) {
        object@extraInfo <- update_data(object@extraInfo, trans_node_data)
    }


    ## Add node labels back to tree, if there were any
    if (node_label_name %in% names( trans_node_data ) ) {
        labels <- trans_node_data[[ node_label_name ]]
        labels <- labels[1:Nnode(object) + Ntip(object)]
        object@phylo$node.label <- labels
    } else {
        object@phylo$node.label <- NULL
    }


    return(object)
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
