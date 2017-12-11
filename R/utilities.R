is_numeric <- function(x) !anyNA(suppressWarnings(as.numeric(as.character(x))))

filename <- function(file) {
    ## textConnection(text_string) will work just like a file
    ## in this case, just set the filename as ""
    file_name <- ""
    if (is.character(file)) {
        file_name <- file
    }
    return(file_name)
}

jplace_treetext_to_phylo <- function(tree.text) {
    ## move edge label to node label separate by @
    tr <- gsub('(:[0-9\\.eE\\+\\-]+)\\{(\\d+)\\}', '\\@\\2\\1', tree.text)
    phylo <- read.tree(text=tr)
    if (length(grep('@', phylo$tip.label)) > 0) {
        phylo$node.label[1] %<>% gsub("(.*)\\{(\\d+)\\}", "\\1@\\2", .)
        tip.edgeNum <- as.numeric(gsub("[^@]*@(\\d*)", "\\1",phylo$tip.label))
        node.edgeNum <- as.numeric(gsub("[^@]*@(\\d*)", "\\1",phylo$node.label))
        phylo$tip.label %<>% gsub("@\\d+", "", .)
        phylo$node.label %<>% gsub("@\\d+", "", .)
        if (all(phylo$node.label == "")) {
            phylo$node.label <- NULL
        }

        N <- getNodeNum(phylo)
        edgeNum.df <- data.frame(node=1:N, edgeNum=c(tip.edgeNum, node.edgeNum))
        ## root node is not encoded with edge number
        edgeNum.df <- edgeNum.df[!is.na(edgeNum.df[,2]),]
        attr(phylo, "edgeNum") <- edgeNum.df
    }

    ## using :edge_length{edge_num} to match edge_num to node_num
    ## this is not a good idea since there may exists identical edge_length.
    ## but we can use it to verify our method.
    ##
    ## en.matches <- gregexpr(":[0-9\\.eE\\+\\-]+\\{\\d+\\}", tree.text)
    ## matches <- en.matches[[1]]
    ## match.pos <- as.numeric(matches)
    ## match.len <- attr(matches, 'match.length')

    ## edgeLN <- substring(tree.text, match.pos+1, match.pos+match.len-2)
    ## edgeLN.df <- data.frame(length=as.numeric(gsub("\\{.+", "", edgeLN)),
    ##                         edgeNum = as.numeric(gsub(".+\\{", "", edgeLN)))

    ## xx <- merge(edgeLN.df, edgeNum.df, by.x="node", by.y="node")

    return(phylo)
}


## convert edge number to node number for EPA/pplacer output
edgeNum2nodeNum <- function(jp, edgeNum) {
    edges <- attr(jp@phylo, "edgeNum")

    idx <- which(edges$edgeNum == edgeNum)
    if (length(idx) == 0) {
        return(NA)
    }

    edges[idx, "node"]
}

is.tree <- function(x) {
    if (class(x) %in% c("phylo",
                        "phylo4",
                        "jplace",
                        "treedata")
        ) {
        return(TRUE)
    }
    return(FALSE)
}

##' @importFrom methods .hasSlot is missingArg new slot slot<-
has.slot <- function(object, slotName) {
    if (!isS4(object)) {
        return(FALSE)
    }
    .hasSlot(object, slotName)
}

