##' get.treetext method
##'
##'
##' @rdname get.treetext-methods
##' @exportMethod get.treetext
setMethod("get.treetext", signature(object = "treedata"),
          function(object) {
              object@treetext
          })
