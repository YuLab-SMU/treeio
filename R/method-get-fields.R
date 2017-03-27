
##' get.fields method
##'
##'
##' @docType methods
##' @name get.fields
##' @rdname get.fields-methods
##' @aliases get.fields,jplace,ANY-method
##' @exportMethod get.fields
##' @author Guangchuang Yu \url{http://ygc.name}
##' @usage get.fields(object, ...)
##' @examples
##' jp <- system.file("extdata", "sample.jplace", package="treeio")
##' jp <- read.jplace(jp)
##' get.fields(jp)
setMethod("get.fields", signature(object = "jplace"),
          function(object, ...) {
              get.fields.tree(object)
          }
          )


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object="beast"),
          function(object, ...) {
              get.fields.tree(object)
          }
          )


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object="r8s"),
          function(object, ...) {
              get.fields.tree(object)
          }
          )


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "hyphy"),
          function(object, ...) {
              if(length(object@tip_seq) == 0) {
                  warning("tip sequence not available...\n")
              } else {
                  get.fields.tree(object)
              }
          })


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "paml_rst"),
          function(object) {
              if (length(object@tip_seq) == 0) {
                  warning("tip sequence not available...\n")
              } else {
                  get.fields.tree(object)
              }
          }
          )


## ##' @rdname get.fields-methods
## ##' @exportMethod get.fields
## setMethod("get.fields", signature(object="raxml"),
##           function(object, ...) {
##               get.fields.tree(object)
##           }
##           )



##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "codeml_mlc"),
          function(object) {
              get.fields.tree(object)
          })


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "treedata"),
          function(object) {
              get.fields.tree(object)
          })


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object="phangorn"),
          function(object, ...) {
              get.fields.tree(object)
          }
          )

