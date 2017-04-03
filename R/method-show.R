##' @rdname show-methods
##' @importFrom ape print.phylo
##' @exportMethod show
setMethod("show", signature(object = "beast"),
          function(object) {
              cat("'beast' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'.\n\n"))
              cat("...@ tree: ")
              print.phylo(get.tree(object))
              cat("\nwith the following features available:\n")
              print_fields(object)
          })

##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "codeml"),
          function(object) {
              cat("'codeml' S4 object that stored information of\n\t",
                  paste0("'", object@rst@rstfile, "' and \n\t'",
                         object@mlc@mlcfile, "'."),
                  "\n\n")
              cat("...@ tree:")
              print.phylo(get.tree(object))
              print_fields(object)
          })

##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "codeml_mlc"),
          function(object) {
              cat("'codeml_mlc' S4 object that stored information of\n\t",
                  paste0("'", object@mlcfile, "'."),
                  "\n\n")

              cat("...@ tree:")
              print.phylo(get.tree(object))
              print_fields(object)
          }
          )

##' show method for \code{jplace} instance
##'
##'
##' @name show
##' @docType methods
##' @rdname show-methods
##'
##' @title show method
##' @param object one of \code{jplace}, \code{beast} object
##' @return print info
##' @importFrom methods show
##' @exportMethod show
##' @usage show(object)
##' @author Guangchuang Yu \url{https://guangchuangyu.github.io}
##' @examples
##' jp <- system.file("extdata", "sample.jplace", package="treeio")
##' jp <- read.jplace(jp)
##' show(jp)
setMethod("show", signature(object = "jplace"),
          function(object) {
              cat("'jplace' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'."),
                  "\n\n")

              cat("...@ tree: ")

              phylo <- get.tree(object)
              phylo$node.label <- NULL
              phylo$tip.label %<>% gsub("\\@\\d+", "", .)

              print.phylo(phylo)
              print_fields(object)
          }
          )

## ##' @rdname show-methods
## ##' @exportMethod show
## setMethod("show", signature(object = "nhx"),
##           function(object) {
##               cat("'nhx' S4 object that stored information of\n\t",
##                   paste0("'", object@file, "'.\n\n"))
##               cat("...@ tree: ")
##               print.phylo(get.tree(object))
##               cat("\nwith the following features available:\n")
##               print_fields(object)
##           })


##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "phylip"),
          function(object) {
              cat("'phylip' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'.\n\n"))
              cat("...@ tree: ")
              print.phylo(get.tree(object))
              msg <- paste0("\nwith sequence alignment available (", length(object@sequence),
                            " sequences of length ", nchar(object@sequence)[1], ")\n")
              cat(msg)
          })

##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "paml_rst"),
          function(object) {
              cat("'paml_rst' S4 object that stored information of\n\t",
                  paste0("'", object@rstfile, "'.\n\n"))
              ## if (length(object@tip.fasfile) != 0) {
              ##     cat(paste0(" and \n\t'", object@tip.fasfile, "'.\n\n"))
              ## } else {
              ##     cat(".\n\n")
              ## }
              fields <- get.fields(object)

              cat("...@ tree:")
              print.phylo(get.tree(object))
              print_fields(object)
          })



##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "r8s"),
          function(object) {
              cat("'r8s' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'.\n\n"))
              cat("...@ trees: \n")
              ## cat("\nwith the following features available:\n")
              print_fields(object)
          })



##' @rdname show-methods
##' @importFrom ape print.phylo
##' @exportMethod show
setMethod("show", signature(object = "hyphy"),
          function(object) {
              cat("'hyphy' S4 object that stored information of \n\t",
                  paste0("'", object@tree.file, "'"))
              if (length(object@tip_seq) == 0) {
                  cat(paste0("and '", object@ancseq.file, "'"), ".\n")
              } else {
                  cat(paste0(", \n\t'", object@ancseq.file, "'"),
                      paste0("and \n\t'", object@tip.fasfile, "'."),
                      "\n\n")
              }
              cat("...@ tree:")
              print.phylo(get.tree(object))
              print_fields(object)
          })



## ##' @rdname show-methods
## ##' @importFrom ape print.phylo
## ##' @exportMethod show
## setMethod("show", signature(object = "raxml"),
##           function(object) {
##               cat("'raxml' S4 object that stored information of\n\t",
##                   paste0("'", object@file, "'.\n\n"))
##               cat("...@ tree: ")
##               print.phylo(get.tree(object))
##               cat("\nwith the following features available:\n")
##               print_fields(object)
##           })


##' @rdname show-methods
##' @importFrom ape print.phylo
##' @exportMethod show
setMethod("show", signature(object = "treedata"),
          function(object) {
              cat("'treedata' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'.\n\n"))
              cat("...@ tree: ")
              print.phylo(get.tree(object))
              cat("\nwith the following features available:\n")
              print_fields(object)
          })


print_fields <- function(object) {
    cat("\nwith the following features availables:\n")
    ff <- paste0("\t'",paste(get.fields(object), collapse="',\t'"), "'.\n")
    cat(fields_wrap(ff))
}

fields_wrap <- function(ff) {
    w <- getOption('width')
    n <- nchar(ff)
    if (w < n) {
        s <- gregexpr("\t", substring(ff, 1, w))[[1]]
        i <- s[length(s)]
        ff2 <- substring(ff, 1:n, 1:n)
        ff2[i] <- '\n\t'
        n <- n+1
        i <- i+1
        ff <- paste0(ff2, collapse='')
        if (w < (n-i)) {
            ff1 <- substring(ff, 1, i)
            ff2 <- substring(ff, i+1, n)
            return(paste0(ff1, fields_wrap(ff2)))
        }
    }
    return(ff)
}

