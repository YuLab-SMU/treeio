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

              #no_node_label <- FALSE

              # Column name that has very low chance of collision with existing column
              node_label_name = "cd8128f329f72c167a8028cf8"

              if (!is.null(object@phylo$node.label)) {
                # Tree has node labels. Put these in data 
                # for safe keeping and remove them from tree
                # for now

                labels = c( 
                  rep( NA, length( object@phylo$tip.label ) ), 
                  object@phylo$node.label
                )
                object@data[[ node_label_name ]] <- labels

                object@phylo$node.label <- NULL
              }

              ## label the internal tree nodes by their number
              object@phylo$node.label <- Ntip(object) + (1:Nnode(object))

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

              ## Add node labels back to tree, if there were any
              if (node_label_name %in% names( object@data ) ) {
                labels = object@data[[ node_label_name ]]
                ntips = length ( object@phylo$tip.label )
                labels = labels[ (ntips+1):nrow( object@data ) ]
                object@phylo$node.label = labels
                object@data[[ node_label_name ]] <- NULL
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
