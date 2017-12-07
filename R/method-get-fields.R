##' get.fields method
##'
##'
##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "treedata"),
          function(object) {
              get.fields.treedata(object)
          })


get.fields.treedata <- function(object) {
    if (nrow(object@data) > 0) {
        fields <- colnames(object@data)
        fields <- fields[fields != "node"]
    } else {
        fields <- ""
    }

    extraInfo <- object@extraInfo
    if (nrow(extraInfo) > 0) {
        cn <- colnames(extraInfo)
        i <- match(c("x", "y", "isTip", "node", "parent", "label", "branch", "branch.length"), cn)
        i <- i[!is.na(i)]
        fields %<>% c(cn[-i])
    }
    return(fields)
}

