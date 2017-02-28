## ##' @rdname groupOTU-methods
## ##' @exportMethod groupOTU
## setMethod("groupOTU", signature(object="apeBootstrap"),
##           function(object, focus, group_name="group", ...) {
##               groupOTU_(object, focus, group_name, ...)
##           }
##           )


##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="beast"),
          function(object, focus, group_name="group", ...) {
              groupOTU_(object, focus, group_name, ...)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="codeml"),
          function(object, focus, group_name="group", ...) {
              groupOTU_(object, focus, group_name, ...)
          }
          )


##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="codeml_mlc"),
          function(object, focus, group_name="group", ...) {
              groupOTU_(object, focus, group_name, ...)
          }
          )




##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="jplace"),
          function(object, focus, group_name="group", ...) {
              groupOTU_(object, focus, group_name, ...)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="treedata"),
          function(object, focus, group_name="group", ...) {
              groupOTU_(object, focus, group_name, ...)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="phangorn"),
          function(object, focus, group_name="group", ...) {
              groupOTU_(object, focus, group_name, ...)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="phylip"),
          function(object, focus, group_name="group", ...) {
              groupOTU_(object, focus, group_name, ...)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="paml_rst"),
          function(object, focus, group_name="group", ...) {
              groupOTU_(object, focus, group_name, ...)
          }
          )


##' group tree based on selected OTU, will traceback to MRCA
##'
##'
##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="phylo"),
          function(object, focus, group_name="group", ...) {
              groupOTU.phylo(object, focus, group_name, ...)
          })

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
##' @param tree which tree selected
setMethod("groupOTU", signature(object="r8s"),
          function(object, focus, group_name="group", tree="TREE", ...) {
              groupOTU_(get.tree(object)[[tree]], focus, group_name, ...)
          }
          )






##' @importFrom ape which.edge
gfocus <- function(phy, focus, group_name, focus_label=NULL, overlap="overwrite") {
    overlap <- match.arg(overlap, c("origin", "overwrite", "abandon"))

    if (is.character(focus)) {
        focus <- which(phy$tip.label %in% focus)
    }

    n <- getNodeNum(phy)
    if (is.null(attr(phy, group_name))) {
        foc <- rep(0, n)
    } else {
        foc <- attr(phy, group_name)
    }
    i <- max(suppressWarnings(as.numeric(foc)), na.rm=TRUE) + 1
    if (is.null(focus_label)) {
        focus_label <- i
    }

    ## sn <- phy$edge[which.edge(phy, focus),] %>% as.vector %>% unique
    hit <- unique(as.vector(phy$edge[which.edge(phy, focus),]))
    if (overlap == "origin") {
        sn <- hit[is.na(foc[hit]) | foc[hit] == 0]
    } else if (overlap == "abandon") {
        idx <- !is.na(foc[hit]) & foc[hit] != 0
        foc[hit[idx]] <- NA
        sn <- hit[!idx]
    } else {
        sn <- hit
    }

    if (length(sn) > 0) {
        foc[sn] <- focus_label
    }

    attr(phy, group_name) <- foc
    phy
}


##' group OTU
##'
##'
##' @title groupOTU.phylo
##' @param phy tree object
##' @param focus tip list
##' @param group_name name of the group
##' @param ... additional parameters
##' @return phylo object
##' @author ygc
groupOTU.phylo <- function(phy, focus, group_name="group", ...) {
    attr(phy, group_name) <- NULL
    if ( is(focus, "list") ) {
        for (i in 1:length(focus)) {
            phy <- gfocus(phy, focus[[i]], group_name, names(focus)[i], ...)
        }
    } else {
        phy <- gfocus(phy, focus, group_name, ...)
    }
    res <- attr(phy, group_name)
    res[is.na(res)] <- 0
    attr(phy, group_name) <- factor(res)
    return(phy)
}

groupOTU_ <- function(object, focus, group_name, ...) {
    if (is(object, "phylo")) {
        object <- groupOTU.phylo(object, focus, group_name, ...)
    } else {
        object@phylo <- groupOTU.phylo(get.tree(object), focus, group_name, ...)
    }
    return(object)
}


