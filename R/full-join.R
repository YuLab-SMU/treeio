##' @importFrom dplyr full_join
##' @importFrom tibble tibble
##' @importFrom cli cli_warn
##' @method full_join treedata
##' @export
full_join.treedata <- function(x, y, by = NULL,
                               copy = FALSE, suffix = c("", ".y"), ...) {

    dat <- .extract_annotda.treedata(x)
    ornm <- colnames(dat)
    msg <- c("The {.arg suffix} requires a character vector containing 2 different elements,",
             "The first element must be \"\", and the second element must not be \"\",",
             "it was set {.code suffix=c(\"\", \".y\")} automatically.")
    if (all(nchar(suffix)!=0)){
        cli::cli_warn(msg)
        suffix[1] = ""
    }
    if (all(nchar(suffix)==0)){
        cli::cli_warn(msg)
        suffix[2] = ".y"
    }
    if (nchar(suffix[1])!=0 && nchar(suffix[2])==0){
        cli::cli_warn(msg)
        suffix <- rev(suffix[seq_len(2)])
    }
    
    da <- dplyr::full_join(dat, y, by = by, copy = copy, suffix = suffix, ...)

    da <- da[!is.na(da$node),]

    if (any(duplicated(da$node))){
        da %<>% .internal_nest(keepnm=ornm)
    }

    tr <- .update.td.join(td=x, da=da)
    return(tr)    
}

##' @method full_join phylo
##' @export
full_join.phylo <- function(x, y, by = NULL,
                            copy = FALSE, suffix = c("", ".y"), ...) {
    full_join(as.treedata(x), y = y, by = by,
              copy = copy, suffix = suffix, ...)
}
