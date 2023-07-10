##' read nhx tree file
##'
##'
##' @title read.nhx
##' @param file nhx file
##' @return nhx object
##' @importFrom tibble as_tibble
##' @export
##' @examples
##' nhxfile <- system.file("extdata/NHX", "ADH.nhx", package="treeio")
##' read.nhx(nhxfile)
##' @author Guangchuang Yu \url{https://guangchuangyu.github.io}
read.nhx <- function(file) {
    treetext <- readLines(file, warn=FALSE)
    treetext <- treetext[treetext != ""]
    treetext <- treetext[treetext != " "]

    if (length(treetext) > 1) {
        treetext <- paste0(treetext, collapse = '')
    }
    treetext <- gsub(" ", "", treetext)
    # t1:0.04[&&NHX:test=1] -> t1[&&NHX:test=1]:0.04
    pattern <- "(\\w+)?(:?\\d*\\.?\\d*[Ee]?[\\+\\-]?\\d*)?(\\[&&NHX.*?\\])"
    treetext <- gsub(pattern, "\\1\\3\\2", treetext)
    #t1[&&NHX:test=1]:0.04 -> t1[&&NHX|test=1]:0.04
    #treetext <- gsub("\\:(?=[^\\[\\]]*\\])", "|", treetext, perl=TRUE)
    phylo <- read.nhx.tree(treetext)
    stats <- read.nhx.stats(treetext=treetext, phylo=phylo)
    
    new("treedata",
        file = filename(file),
        phylo = phylo,
        data = as_tibble(stats)
        )
}

read.nhx.tree <- function(treetext){
    pattern <- "(\\w+)?(\\[&&NHX.*?\\])(:?\\d*\\.?\\d*[Ee]?[\\+\\-]?\\d*)"
    treetext <- gsub(pattern, "\\1\\3", treetext)
    tree <- ape::read.tree(text=treetext)
    return(tree)
}

read.nhx.stats <- function(treetext, phylo){
    tree2 <- add_pseudo_label(phylo)
    nn <- strsplit(tree2, split=",") %>% unlist %>%
          strsplit(., split="\\)") %>% unlist %>%
          gsub("\\(*", "", .) %>%
          gsub("[:;].*", "", .) %>%
          gsub(" ", "", .) %>%
          gsub("'", "", .) %>%
          gsub('"', "", .)
    phylo <- ape::read.tree(text = tree2)
    root <- rootnode(phylo)
    nnode <- phylo$Nnode

    tree_label <- c(phylo$tip.label, phylo$node.label)
    ii <- match(nn, tree_label)
    label2 <- as.character(seq_len(getNodeNum(phylo)))
    node <- label2[match(nn, tree_label)]
    #if (!grepl(":?\\d*\\.?\\d*[Ee]?[\\+\\-]?\\d*", treetext)){
    stats <- strsplit(treetext, "[,\\)]") %>% unlist()
    #}else{
    #    stats <- strsplit(treetext, ":") %>% unlist()
    #}
    names(stats) <- node

    stats <- stats[grep("\\[&&NHX", stats)]
    stats <- sub("[^\\[]*\\[", "", stats) %>%
             # sub("^&&NHX:", "", .) %>%
             sub("^&&NHX", "", .) %>%
             sub("].*", "", .) %>%
             gsub("\"", "", .)
    stats <- extract_nhx_feature(stats=stats)
    return(stats)
}

extract_nhx_feature <- function(stats){
    stats2 <- get_nhx_feature(nhx_features=stats)
    stats2 <- convert_to_numeric(dat=stats2)
    stats2$node <- names(stats)
    stats2$node <- as.integer(stats2$node)
    stats2 <- as_tibble(stats2)
    return(stats2)

}

#' @importFrom ape Ntip Nnode
add_pseudo_label <- function(phylo){
    phylo$tip.label <- paste0("T", seq_len(Ntip(phylo)))
    phylo$node.label <- paste0("N", seq_len(Nnode(phylo)))
    treetext <- ape::write.tree(phylo)
    return(treetext)
}

get_nhx_feature <- function(nhx_features) {
    nameSET <- strsplit(nhx_features, split=":") %>% unlist %>%
        gsub("=.*", "", .) %>% unique %>% sort
    lapply(nhx_features, get_nhx_feature_internal, nameSET=nameSET) %>%
        do.call(rbind, .) %>% as.data.frame(., stringsAsFactors = FALSE)
}

get_nhx_feature_internal <- function(feature, nameSET) {
    x <- strsplit(feature, ":") %>% unlist
    name <- gsub("=.*", "", x)
    val <- gsub(".*=", "", x)

    names(val) <- name
    y <- character(length(nameSET))
    for (i in seq_along(nameSET)) {
        if (nameSET[i] %in% name) {
            y[i] <- val[nameSET[i]]
        } else {
            y[i] <- NA
        }
    }
    names(y) <- nameSET
    return(y)
}

convert_to_numeric <- function(dat){
    for (i in seq_len(ncol(dat))){
        x <- dat[, i]
        x <- x[!is.na(x)]
        #if (all(grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+", x))){
        if (is_numeric(x)){
            ## should be numerical varialbe
            dat[,i] <- as.numeric(dat[,i])
        }
    }
    return (dat)
}
