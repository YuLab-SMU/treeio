#' @title read.phyloxml
#' @param file phyloxml file
#' @return treedata class or treedataList class
#' @export
#' @examples
#' xmlfile1 <- system.file("extdata/phyloxml", "test_x2.xml", package="treeio")
#' px1 <- read.phyloxml(xmlfile1)
#' px1
#' xmlfile2 <- system.file("extdata/phyloxml", "phyloxml_examples.xml", package="treeio")
#' px2 <- read.phyloxml(xmlfile2)
#' px2
read.phyloxml <- function(file){
    x <- xml2::read_xml(file)
    x <- xml2::as_list(x)
    x <- x[["phyloxml"]]
    index <- which(names(x)=="phylogeny")
    if (length(index)==0){
        stop("The input file is not phyloxml format, please check it !")
    }
    if (length(index)==1){
        objtmp <- single_tree(index, x, file)
        obj <- objtmp[[1]]
    }else{
        objtmp <- lapply(index, single_tree, x, file)
        obj <- lapply(objtmp, function(x)x[[1]])
        names(obj) <- unlist(lapply(objtmp, function(x)x[[2]]))
        class(obj) <- "treedataList"
    }
    return(obj)
}

#' @keywords internal
single_tree <- function(i, phylogeny, file){
    rootflag <- unname(check_attrs(phylogeny[[i]]))
    treename <- extract_treename(i, phylogeny)
    dt <- parser_clade(phylogeny[[i]][["clade"]])
    dt <- dt[!is.na(dt$parentID), ]
    if ("branch_length" %in% colnames(dt) & !all(is.na(dt[["branch_length"]]))){
        edgedf <- dt[,c("parentID", "NodeID", "branch_length")]
        edgedf[(edgedf[,1]!=edgedf[,2] & is.na(edgedf[["branch_length"]])),"branch_length"] <- 0
        rmclumn <- c("parentID", "NodeID", "branch_length")
    }else{
        edgedf <- dt[,c("parentID", "NodeID")]
        rmclumn <- c("parentID", "NodeID")
    }
    dt <- dplyr::mutate(dt, label = as.character(dt$NodeID))
    dd <- as.phylo(edgedf, "branch_length")
    # check whether is rooted tree
    if (rootflag == "false"){
        if (ape::is.rooted(dd)){
            dd <- ape::unroot(dd)
        }
    }
    dd <- dd %>% as_tibble() %>%
            dplyr::full_join(dt, by='label')
    colnm <- colnames(dd)
    if ("accession" %in% colnm){
        dd$label <- as.vector(dd[["accession"]])
        rmclumn <- c(rmclumn, "accession")
    }else if ("scientific_name" %in% colnm && !"accession" %in% colnm){
        dd$label <- as.vector(dd[["scientific_name"]])
        rmclumn <- c(rmclumn, "scientific_name")
    }else if ("name" %in% colnm && !"accession" %in% colnm && !"scientific_name" %in% colnm){
        dd$label <- as.vector(dd[["name"]])
        rmclumn <- c(rmclumn, "name")
    }
    obj <- dd %>% dplyr::select(-dplyr::all_of(rmclumn)) %>% as.treedata()
    obj@file <- filename(file)
    return(c(obj, treename))
}


#' @keywords internal
parser_clade <- function(x, id=list2env(list(id = 0L)), parent=NULL){
    # to generate edge data
    id[["id"]] <- id[["id"]] + 1L
    id[["data"]][[id[["id"]]]] <- extract_values_attrs(x, id=id[["id"]], isTip=FALSE, parent=fill_id(parent))
    index <- which(names(x)=="clade")
    if (length(index)){
        lapply(x[index], parser_clade, id=id, parent=id[["data"]][[id[["id"]]-1L]][["NodeID"]])
    }else{
        id[["data"]][[id[["id"]]]][["isTip"]] <- TRUE
    }
    dat <- dplyr::bind_rows(as.list(id[["data"]]))
    return(dat)
}

#' @keywords internal
fill_id <- function(x){
    if (is.list(x)) {
        lapply(x, fill_id)
    }else{
        ifelse(length(x), unlist(x), NA)
    }
}

#' @keywords internal
extract_another <- function(x){
    namestmp <- names(x)
    index <- which(namestmp != "clade")
    if (length(index) >0){
        lapply(x[index], function(i){
               attrs1 <- check_attrs(i)
               attrs2 <- extract_attrs(i)
               attrs <- c(attrs1, attrs2)
               values <- check_value(i)
               if(inherits(attrs, "list")){attrs <- remove_names(attrs)}
               if(inherits(values,"list")){values <- remove_names(values)}
               res <- unlist(c(attrs, values))
               res <- res[!duplicated(res)]
               namestmp2 <- names(res)
               # rename the duplicated names
               dind <- which(duplicated(namestmp2)|duplicated(namestmp2, fromLast=TRUE))
               if (length(dind) & length(unique(res[dind]))>1){
                   namestmp2[dind] <- unlist(mapply(paste0, namestmp2[dind],seq_len(length(dind)), SIMPLIFY=FALSE))
               }
               names(res) <- unlist(namestmp2)
               return(res)
              })
    }
}

#' @keywords internal
check_attrs <- function(x){
    namesattrs <- names(attributes(x))
    index <- which(namesattrs != "names")
    if (length(index)){
        unlist(attributes(x)[index])
    }
}

#' @keywords internal
check_value <- function(x){
    if(length(unlist(x))>1){
        lapply(x, check_value)
    }else{
        if (length(unlist(x))==0){
            return(c(check_attrs(x)))
        }else{
            return(c(check_attrs(x), remove_names(x)))
        }
    }
}

extract_attrs <- function(x){
    lapply(x, check_attrs)
}

#' @keywords internal
extract_treename <- function(i, phylogeny){
    if ("name" %in% names(phylogeny[[i]])){
        treename <- unlist(phylogeny[[i]][["name"]])
    }else{
        treename <- paste0("phylogeny_", i)
    }
    return (treename)
}

remove_names <- function(x){
    for (i in seq_len(length(x))){
        if (!is.null(x[[i]])){
            if (is.null(names(x[[i]])) && !is.null(x[[i]])){
                names(x[[i]]) <- names(x[i])
            }else{
                names(x[[i]])[nchar(names(x[[i]]))==0] <- names(x[i])
            }
        }
    }
    names(x) <- NULL
    return(x)
}


#' @keywords internal
extract_values_attrs <- function(x, id, parent, isTip){
    # extract attributes of x to avoid them being remove in lapply.
    attr <- check_attrs(x)
    anothers <- extract_another(x)
    anothers <- unlist(remove_names(anothers))
    res <- c(attr, anothers)
    if (!is.null(res)){
        res <- data.frame(t(res), check.names=FALSE, stringsAsFactors=FALSE)
        res$parentID <- parent
        res$NodeID <- id
        res$isTip <- isTip
    }else{
        res <- data.frame(parentID=parent, NodeID=id, isTip=isTip,
                          check.names=FALSE, stringsAsFactors=FALSE)
    }
    if ("confidence" %in% colnames(res)){
        res$confidence <- as.numeric(res$confidence)
    }
    return(res)
}

