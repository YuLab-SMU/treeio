#' @title read.phyloxml
#' @param file phyloxml file
#' @return treedata class
#' @export
#' @examples
#' xmlfile <- system.file("extdata/phyloxml", "test_x2.xml", package="treeio")
#' px <- read.phyloxml(xmlfile)
#' px
read.phyloxml <- function(file){
    x <- xml2::read_xml(file)
    x <- xml2::xml_root(x)
    phylogeny <- xml2::as_list(x)[["phyloxml"]]

    clade <- phylogeny[["phylogeny"]][["clade"]]
    dt <- parser_clade(clade)
    dt <- dt[!is.na(dt$nodeid),]
    edgedf <- dt[,c("nodeid", "child", "edge.length")]
    dt <- dplyr::mutate(dt, label = as.character(.data$child))
    dd <- as.phylo(edgedf, "edge.length") %>% as_tibble() %>%
          dplyr::full_join(dt, by='label')  
    obj <- dd %>% dplyr::mutate(label=as.vector(.data$labelnames)) %>% 
           dplyr::select(-c("nodeid", "child", "edge.length", "labelnames")) %>%
           as.treedata
    obj@file <- filename(file)
    return(obj)
}

#' @keywords internal
parser_clade <- function(x, id=list2env(list(id = 0L)), parent=NULL){
    id[["id"]] <- id[["id"]] + 1L
    tmpdf <- data.frame(nodeid = checkvalue(parent),
                        child = id[["id"]],
                        edge.length=checkvalue(x[["branch_length"]]),
                        labelnames=checkvalue(x[["name"]]))
    index <- which(names(x)=="clade")
    clades <- lapply(x[index], parser_clade, id=id, parent=tmpdf$child)
    clades <- do.call("rbind", clades)
    return(rbind(tmpdf, clades, make.row.names=FALSE))
}

#' @keywords internal
checkvalue <- function(x){
    n <- length(x)
    if (n == 0) return(NA)
    if (inherits(x, "list")) {
        if (n == 1) {
            x <- x[[1]]
        }
    }
    
    if (inherits(x, "list")) {
        lapply(x, checkvalue)    
    } else {
        unlist(x)
    }
}
