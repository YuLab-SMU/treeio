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
    check_installed('xml2', 'for `read.phyloxml()`.')
    ## NOBLANKS is the default of `read_xml()` and has to be repeated, it drops
    ## the indentation of the file, which would otherwise be parsed as the
    ## annotation of every clade; libxml2 refuses a document nested deeper than
    ## 256 elements by default, which a ladder tree of ~250 tips reaches
    x <- xml2::read_xml(file, options = c("NOBLANKS", "HUGE"))
    x <- xml2::xml_root(x)
    if (xml2::xml_name(x) != "phyloxml"){
        stop("The input file is not phyloxml format, please check it !")
    }
    phylogeny <- xml2::xml_children(x)
    index <- which(xml2::xml_name(phylogeny) == "phylogeny")
    if (length(index)==0){
        stop("The input file is not phyloxml format, please check it !")
    }
    if (length(index)==1){
        objtmp <- single_tree(phylogeny[[index]], index, file)
        obj <- objtmp[[1]]
    }else{
        objtmp <- lapply(index, function(i) single_tree(phylogeny[[i]], i, file))
        obj <- lapply(objtmp, function(x)x[[1]])
        names(obj) <- unlist(lapply(objtmp, function(x)x[[2]]))
        class(obj) <- "treedataList"
    }
    return(obj)
}

#' @keywords internal
single_tree <- function(phylogeny, i, file){
    rootflag <- unname(xml2::xml_attrs(phylogeny))
    treename <- extract_treename(phylogeny, i)
    dt <- parser_clade(phylogeny)
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
    if ('branch_length' %in% names(edgedf)){
        ## the argument of as.phylo() is 'branch.length', 'length' ends up
        ## in '...' and the branch lengths were dropped, #124
        dd <- as.phylo(edgedf, branch.length = branch_length)
    }else{
        dd <- as.phylo(edgedf)
    }
    # check whether is rooted tree
    if (any(rootflag == "false")){
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
## walk the clade tree of a <phylogeny> node to generate the edge data.
## The <clade> elements are nested, so the whole document cannot be turned into
## a list with xml2::as_list(), which recurses once per level: it overflowed
## the C stack on a ladder tree of ~500 tips. Only the flat annotation of a
## clade is converted with as_list() here and the clade tree itself is walked
## with an explicit stack, which also avoids the bind_rows() of all the nodes
## visited so far that the recursion did at every node (800 tips took 18s).
parser_clade <- function(x){
    kids <- xml2::xml_children(x)
    root <- kids[[match("clade", xml2::xml_name(kids))]]

    id <- 0L
    data <- list()
    ## `stack` is a local list and must be assigned with `<-`, `<<-` would skip
    ## it and reach utils::stack
    stack <- list(list(node=root, parent=NULL))
    top <- 1L

    while (top > 0L){
        cur <- stack[[top]]
        top <- top - 1L

        id <- id + 1L
        kids <- xml2::xml_children(cur$node)
        nms <- xml2::xml_name(kids)
        isClade <- nms == "clade"

        ## the annotation of the clade, its attributes and its children that
        ## are not a <clade>, in the shape xml2::as_list() would give
        anno <- list()
        if (any(!isClade)){
            anno <- lapply(kids[!isClade], xml2::as_list)
            names(anno) <- nms[!isClade]
        }
        attributes(anno) <- c(list(names=names(anno)),
                              as.list(xml2::xml_attrs(cur$node)))

        data[[id]] <- extract_values_attrs(anno, id=id,
                                           isTip=!any(isClade),
                                           parent=fill_id(cur$parent))
        if (any(isClade)){
            clades <- kids[isClade]
            ## the children are pushed in reverse order so that they are
            ## visited left to right, as the depth first recursion did
            for (j in rev(seq_along(clades))){
                top <- top + 1L
                stack[[top]] <- list(node=clades[[j]], parent=id)
            }
        }
    }

    dat <- dplyr::bind_rows(data)
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
extract_treename <- function(phylogeny, i){
    kids <- xml2::xml_children(phylogeny)
    j <- match("name", xml2::xml_name(kids))
    if (is.na(j)){
        treename <- paste0("phylogeny_", i)
    }else{
        treename <- xml2::xml_text(kids[[j]])
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

