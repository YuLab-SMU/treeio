#' @title read.phyloxml
#' @param file phyloxml file
#' @return treedata class or treedataList class
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
    dt <- get_newdata(dt)
    dt <- dt[!is.na(dt$parentID), ]
    if ("branch_length" %in% colnames(dt) & !all(is.na(dt[["branch_length"]]))){
        edgedf <- dt[,c("parentID", "NodeID", "branch_length")]
        rmclumn <- c("parentID", "NodeID", "branch_length")
    }else{
        edgedf <- dt[,c("parentID", "NodeID")]
        rmclumn <- c("parentID", "NodeID")
    }
    dt <- dplyr::mutate(dt, label = as.character(dt$NodeID))
    dd <- as.phylo(edgedf, "branch_length") 
    if (rootflag == "false"){
        dd <- ape::unroot(dd)
    }
    dd <- dd %>% as_tibble() %>%
            dplyr::full_join(dt, by='label')
    if ("name" %in% colnames(dd)){
        dd$label <- as.vector(dd$name)
        rmclumn <- c(rmclumn, "name")
    }else{
        if ("sequence.symbol" %in% colnames(dd)){
            dd$label <- as.vector(dd$sequence.symbol)
            rmclumn <- c(rmclumn, "sequence.symbol")
        }
        if ( "taxonomy.scientific_name" %in% colnames(dd) & !"sequence.symbol"%in%colnames(dd)){
            dd$label <- as.vector(dd$taxonomy.scientific_name)
            rmclumn <- c(rmclumn, "taxonomy.scientific_name")
        }
    }
    obj <- dd %>% dplyr::select(-rmclumn) %>% as.treedata()
    obj@file <- filename(file)
    return(c(obj, treename))
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
    if (length(index) >0){
        lapply(x[index], function(i){
               attrs <- check_attrs(i)
               values <- check_value(i)
               res <- unlist(c(values, attrs))
               namestmp2 <- names(res)
               dind <- which(duplicated(namestmp2)|duplicated(namestmp2, fromLast=TRUE))
               if (length(dind) & length(unique(res[dind]))>1){
                   namestmp2[dind] <- mapply(paste0, namestmp2[dind],seq_len(length(dind)), SIMPLIFY=FALSE)
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
        if (length(x)==0){
            return(check_attrs(x))
	}else{
	    return(c(check_attrs(x),unlist(x)))
	}
    }
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

