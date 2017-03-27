## ##' @rdname get.tree-methods
## ##' @exportMethod get.tree
## setMethod("get.tree", signature(object="apeBootstrap"),
##           function(object,...) {
##               object@phylo
##           }
##           )

##' get.tree method
##'
##'
##' @docType methods
##' @name get.tree
##' @rdname get.tree-methods
##' @aliases get.tree,beast
##' @exportMethod get.tree
##' @author Guangchuang Yu \url{http://guangchuangyu.github.io}
##' @usage get.tree(object, ...)
setMethod("get.tree", signature(object="beast"),
          function(object,...) {
              object@phylo
          }
          )


##' @rdname get.tree-methods
##' @exportMethod get.tree
##' @param by one of rst or mlc
setMethod("get.tree", signature(object="codeml"),
          function(object, by="rst", ...) {
              if (by == "rst") {
                  return(object@rst@phylo)
              } else {
                  return(object@mlc@phylo)
              }
          })


##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object="jplace"),
          function(object) {
              object@phylo
          })

## ##' @rdname get.tree-methods
## ##' @exportMethod get.tree
## setMethod("get.tree", signature(object = "nhx"),
##           function(object) {
##               object@phylo
##           }
##           )

##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object="phylip"),
          function(object,...) {
              object@phylo
          }
          )

##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object="phylo"),
          function(object, ...) {
              return(object)
          })

##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object="treedata"),
          function(object, ...) {
              return(object@phylo)
          })



##' @rdname get.tree-methods
##' @exportMethod get.tree
##' @examples
##' nwk <- system.file("extdata/HYPHY", "labelledtree.tree", package="treeio")
##' ancseq <- system.file("extdata/HYPHY", "ancseq.nex", package="treeio")
##' hy <- read.hyphy(nwk, ancseq)
##' get.tree(hy)
setMethod("get.tree", signature(object = "hyphy"),
          function(object) {
              object@phylo
          }
          )

##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object = "paml_rst"),
          function(object) {
              object@phylo
          }
          )


##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object="phangorn"),
          function(object,...) {
              object@phylo
          }
          )


## ##' @rdname get.tree-methods
## ##' @exportMethod get.tree
## setMethod("get.tree", signature(object="raxml"),
##           function(object,...) {
##               object@phylo
##           }
##           )


##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object = "codeml_mlc"),
          function(object, ...) {
              object@phylo
          }
          )



##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object="r8s"),
          function(object,...) {
              object@phylo
          }
          )

