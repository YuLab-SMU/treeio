#' @method inner_join treedata
#' @importFrom cli cli_warn
#' @export
inner_join.treedata <- function(x, y, by = NULL, copy = FALSE, suffix=c("", ".y"), ...){
    x %<>% dplyr::mutate(.UNIQUE.ID=paste0('ID.', seq_len(treeio::Nnode(x, internal = FALSE))))
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
    da <- dplyr::inner_join(dat, y, by = by, copy = copy, suffix = suffix, ...)
    
    keep.nodes <- da %>% dplyr::filter(.data$isTip) %>% 
                  dplyr::pull(.data$node) %>% unique()

    x <- drop.tip(x, setdiff(dat$node[dat$isTip], keep.nodes))
    
    new.dat <- .extract_annotda.treedata(x)
    da$node <- new.dat$node[match(da$.UNIQUE.ID, new.dat$.UNIQUE.ID)]
    da <- da[!is.na(da$node),]

    if (any(duplicated(da$node))){
        da %<>% .internal_nest(keepnm=ornm)
    }

    tr <- .update.td.join(td=x, da=da)
    tr %<>% dplyr::select(-'.UNIQUE.ID', keep.td=TRUE)
    return(tr)
}

#' @method inner_join phylo
#' @export
inner_join.phylo <- function(x, y, by=NULL, copy=FALSE, suffix=c('', '.y'), ...){
    x <- treedata(phylo=x)
    tr <- x %>% inner_join(y, by = by, copy = copy, suffix=suffix, ...)
    return(tr)
}

