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

              ## label the internal tree nodes by their number
              no_node_label <- FALSE
              if (is.null(object@phylo$node.label)) {
                  object@phylo$node.label <- Ntip(object) + (1:Nnode(object))
                  no_node_label <- TRUE
              }

              ## Prepare the nhx object for subsampling
              object@data$node <- as.numeric(object@data$node)
              object@data <- object@data[order(object@data$node),]

              ## add a colmn that has labels for both tips and internal nodes
              object@data$node.label <- c(object@phylo$tip.label, as.character(object@phylo$node.label))

              ## Will need to take different approaches for subsampling tips
              ## and internal nodes, add a column to make it easy to tell them apart
              object@data$is_tip <- object@data$node <= Ntip(object)

              ## Remove tips
              object@phylo = ape::drop.tip( object@phylo, tip )

              ## Subsample the tags
              object@data = object@data[object@data$node.label %in% (c(object@phylo$tip.label, as.character(object@phylo$node.label))),]

              ## Update tip node numbers
              tip_nodes <- object@data$node.label[ object@data$is_tip ]
              object@data$node[ object@data$is_tip ] = match(object@phylo$tip.label, tip_nodes)

              internal_nodes <- object@data$node.label[ !object@data$is_tip ]
              object@data$node[ !object@data$is_tip ] = match(object@phylo$node.label, internal_nodes) + length(object@phylo$tip.label)

              ## Clean up
              object@data$node.label = NULL
              object@data$is_tip = NULL
              if (no_node_label) {
                  object@phylo$node.label <- NULL
              }

              return(object)
          })





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
