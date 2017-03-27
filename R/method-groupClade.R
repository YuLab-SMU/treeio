##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="beast"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })

##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="codeml"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          }
          )


##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="jplace"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          }
          )

##' group selected clade
##'
##'
##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="treedata"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })

##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="phylip"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })


##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="phylo"),
          function(object, node, group_name="group") {
              groupClade.phylo(object, node, group_name)
          })


##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="phangorn"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })


##' @importFrom ape extract.clade
groupClade.phylo <- function(object, node, group_name) {
    if (length(node) == 1) {
        clade <- extract.clade(object, node)
        tips <- clade$tip.label
    } else {
        tips <- lapply(node, function(x) {
            clade <- extract.clade(object, x)
            clade$tip.label
        })
    }

    groupOTU.phylo(object, tips, group_name)
}


groupClade_ <- function(object, node, group_name) {
    if (is(object, "phylo")) {
        object <- groupClade.phylo(object, node, group_name)
    } else {
        object@phylo <- groupClade.phylo(get.tree(object), node, group_name)
    }
    return(object)
}


