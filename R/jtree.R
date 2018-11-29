##' Import tree data from jtree file, which is JSON-based text and probably output by write.jtree
##'
##'
##' @title read.jtree
##' @rdname read-jtree
##' @param file tree file
##' @return treedata object
##' @export
##' @author guangchuang yu
read.jtree <- function(file) {
    jtree <- fromJSON(file)
    phylo <- jplace_treetext_to_phylo(jtree$tree)
    edgeNum.df <- attr(phylo, "edgeNum")
    d <- merge(edgeNum.df, jtree$data, by.x = "edgeNum", by.y = "edge_num") %>%
        as_tibble %>% select_(~ -edgeNum)
    new("treedata",
        treetext = jtree$tree,
        phylo = phylo,
        data = d,
        file = filename(file)
        )
}


##' Export \code{treedata} object to json tree file
##'
##'
##' @title write.jtree
##' @rdname write-jtree
##' @param treedata \code{treedata} object
##' @param file output file. If file = "", print the output content on screen
##' @return output file or file content on screen
##' @importFrom jsonlite toJSON
##' @export
##' @author guangchuang yu
write.jtree <- function(treedata, file = "") {
    phylo <- as.phylo(treedata)
    ntip <- Ntip(phylo)
    N <- Nnode(phylo, internal.only=FALSE)
    tip.label <- phylo[["tip.label"]]

    label <- rep("", N)

    label[1:ntip] <- tip.label
    if ( !is.null(phylo$node.label) ) {
        label[(ntip+1):N] <- phylo$node.label
    }

    label.df <- data_frame(node=1:N, label=label)
    label.df$label <- paste0(label.df$label, '@@', label.df$node)

    phylo$tip.label <- label.df$label[label.df$node <= ntip]
    phylo$node.label <- label.df$label[label.df$node > ntip]

    tt <- write.tree(phylo)
    if (is.null(phylo$edge.length)) {
        tree_text <- gsub("@@(\\d+)\\D", "{\\1}", tt)
    } else {
        tree_text <- gsub("@@(\\d+)(:[\\.0-9]+)", "\\2{\\1}", tt) %>%
            sub("@@(\\d+)", "{\\1}", .)
    }

    cat("{\n", file = file)

    cat(paste0('\t"tree": "', tree_text, '",\n'), file = file, append = TRUE)
    cat('\t"data":', file = file, append = TRUE)

    data <- get_tree_data(treedata)
    cn <- colnames(data)
    data <- data[, c("node", cn[cn != "node"])]
    data <- rename_(data, edge_num=~node)

    cat(toJSON(data, pretty=TRUE), file = file, append = TRUE)
    metainfo <- ',\n\t"metadata": {"info": "R-package treeio", '
    metainfo <- paste0(metainfo, '"data": ', paste0('"', date(), '"'), '}\n')
    cat(metainfo, file = file, append = TRUE)
    cat("}\n", file = file, append = TRUE)
}
