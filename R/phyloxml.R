#' @title read.phyloxml
#' @param file phyloxml file
#' @return treedata class or multitreedata class
#' @export
#' @examples
#' xmlfile <- system.file("extdata/phyloxml", "test_x2.xml", package="treeio")
#' px <- read.phyloxml(xmlfile)
#' px
#' xmlfile2 <- system.file("extdata/phyloxml", "phyloxml_examples.xml", package="treeio")
#' px2 <- read.phyloxml(xmlfile2)
#' class(px2)
read.phyloxml <- function(file){
    x <- xml2::read_xml(file)
    x <- xml2::as_list(x)
    x <- x[["phyloxml"]]
    index <- which(names(x)=="phylogeny")
    if (length(index)==0){
        stop("The input file is not phyloxml format, please check it !")
    }
    if (length(index)==1){
        obj <- single_tree(x[[index]], file)
    }else{
        obj <- lapply(x[index], single_tree, file)
        class(obj) <- "treedataList"
    }
    return(obj)
}

#' @keywords internal
single_tree <- function(phylogeny, file){
    dt <- parser_clade(phylogeny[["clade"]])
    dt <- dt[!is.na(dt$parentID), ]
    if ("branch_length" %in% colnames(dt)){
        edgedf <- dt[,c("parentID", "NodeID", "branch_length")]
        rmclumn <- c("parentID", "branch_length")
    }else{
        edgedf <- dt[,c("parentID", "NodeID")]
        rmclumn <- c("parentID")
    }
    dt <- dplyr::mutate(dt, label = as.character(dt$NodeID))
    dd <- as.phylo(edgedf, "branch_length") %>% as_tibble() %>%
            dplyr::full_join(dt, by='label')
    if ("accession" %in% colnames(dd)){
        dd$label <- as.vector(dd$accession)
    }else{
        if ("name" %in% colnames(dd)){
            dd$label <- as.vector(dd$name)
        }
        if ( "scientific_name" %in% colnames(dd) & !"name"%in%colnames(dd)){
            dd$label <- as.vector(dd$scientific_name)
	}
    }
    obj <- dd %>% dplyr::select(-rmclumn) %>% as.treedata
    obj@file <- filename(file)
    return(obj)
}


#' @keywords internal
parser_clade <- function(x, id=list2env(list(id = 0L)), parent=NULL){
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
    namestmp <- namestmp[index]
    namestmp2 <- namestmp
    dind <- which(duplicated(namestmp2)|duplicated(namestmp2, fromLast=TRUE))
    if (length(dind)){
        namestmp2 <- mapply(paste0, namestmp2[dind], 
                            seq_len(length(dind)), SIMPLIFY=FALSE)
    }
    if (length(index)){
        lapply(index, function(i){
                      attrs <- check_attrs(x[[i]])
                      values <- check_value(x[[i]])
                      if (length(values)!=0 & length(names(values))==0){
                          names(values) <- namestmp2[i]
                      }
                      c(values, attrs)
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
    if (inherits(x, "list")){
        lapply(x, check_value)
    }else{
        if(length(x)>2){
            lapply(x, check_value)
	}else{
            list(check_attrs(x), unlist(x))
        }
    }
}

#' @keywords internal
extract_values_attrs <- function(x, id, parent, isTip){
    attr <- check_attrs(x)
    anothers <- unlist(extract_another(x))
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
    return(res)
}

