#' @title read.nextstrain.json
#' @param x the json tree file of auspice from nextstrain.
#' @return treedata object
#' @export
#' @author Shuangbin Xu
#' @examples
#' file1 <- system.file("extdata/nextstrain.json", "minimal_v2.json", package="treeio") 
#' tr <- read.nextstrain.json(file1)
#' tr
read.nextstrain.json <- function(x){
    x <- jsonlite::read_json(x)
    if (all(c('meta', 'tree') %in% names(x))){
        dt <- parser_children(x$tree)
    }else{
        dt <- parser_children(x)
    }
    if ('branch.length' %in% colnames(dt)){
        rmclnm <- c("parentID", "NodeID", "branch.length")
        edgedf <- dt[, rmclnm]
    }else{
        rmclnm <- c("parentID", "NodeID")
        edgedf <- dt[, rmclnm]
    }
    dd <- treeio::as.phylo(edgedf, "branch.length")
    dt$label <- as.character(dt$NodeID)
    dt <- dt[, !colnames(dt) %in% rmclnm, drop=FALSE]
    dd <- dd |> tidytree::as_tibble() |> dplyr::full_join(dt, by='label')
    if ("name" %in% colnames(dd)){
        dd$label <- dd$name
        dd$name <- NULL
    }
    tr <- dd |> treeio::as.treedata()
    return(tr)
}

parser_children <- function(x, id=list2env(list(id = 0L)), parent = 1){
    id[["id"]] <- id[["id"]] + 1L
    id[["data"]][[id[["id"]]]] <- extract_node_attrs(x, id=id[["id"]], isTip=FALSE, parent=parent)
    if ('div' %in% colnames(id[['data']][[id[['id']]]])){
        parent.index <- id[['data']][[id[['id']]]][['parentID']]
        id[['data']][[id[['id']]]][['branch.length']] <- as.numeric(id[['data']][[id[['id']]]][['div']]) - 
            as.numeric(id[['data']][[parent.index]][['div']])
    }
    if ('children' %in% names(x)){
        lapply(x$children, 
               parser_children, 
               id = id,
               parent = ifelse(id[['id']]>=2, id[["data"]][[id[["id"]]-1L]][["NodeID"]], 1)
        )
    }else{
        id[["data"]][[id[["id"]]]][["isTip"]] <- TRUE
    }
    dat <- dplyr::bind_rows(as.list(id[["data"]])) %>% dplyr::mutate_if(check_num, as.numeric)
    return(dat)
}

check_num <- function(x){
    is_numeric(x) && is.character(x)
}

extract_node_attrs <- function(x, id, isTip, parent){
    if ('node_attrs' %in% names(x)){
        res <- build_node_attrs(x[['node_attrs']])
    }else if('attr' %in% names(x)){
        res <- build_node_attrs(x[['attr']])
    }else{
        res <- data.frame()
    }
    if ('name' %in% names(x)){
        res$name <- x[['name']]
    }else if('strain' %in% names(x)){
        res$name <- x[['strain']]
    }
    res$parentID <- parent
    res$NodeID <- id
    res$isTip <- isTip
    return(res)
}

build_node_attrs <- function(x){
    x <- unlist(x)
    index <- grepl('\\.value$', names(x))
    names(x)[index] <- gsub('\\.value$', '', names(x)[index])
    x <- tibble::as_tibble(t(x))
    return(x)
}

