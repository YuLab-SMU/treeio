##' @method print treedata
##' @importFrom ape print.phylo
##' @export
print.treedata <- function(x, ...) {
    cat("'treedata' S4 object that stored information of\n\t",
        paste0("'", x@file, "'.\n\n"))
    cat("...@ tree: ")
    print.phylo(as.phylo(x))
    print_fields(x)
}


print_fields <- function(object) {
    cat("\nwith the following features available:\n")
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

