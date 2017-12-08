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

has.field <- function(tree_object, field) {
    if ( ! field %in% get.fields(tree_object) ) {
        return(FALSE)
    }

    if (is(tree_object, "codeml")) {
        is_codeml <- TRUE
        tree <- tree_object@rst
    } else {
        is_codeml <- FALSE
        tree <- tree_object
    }

    if (.hasSlot(tree, field)) {
        has_slot <- TRUE
    } else {
        has_slot <- FALSE
    }

    if (has_slot == FALSE) {
        if (has.extraInfo(tree_object) == FALSE) {
            return(FALSE)
        }

        if (nrow(tree_object@extraInfo) == 0) {
            return(FALSE)
        }

        if (!field %in% colnames(tree_object@extraInfo)) {
            return(FALSE)
        }
    }
    res <- TRUE
    attr(res, "has_slot") <- has_slot
    attr(res, "is_codeml") <- is_codeml
    return(res)
}

## append_extraInfo <- function(df, object) {
##     if (has.extraInfo(object)) {
##         info <- object@extraInfo
##         if ("parent" %in% colnames(info)) {
##             res <- merge(df, info, by.x=c("node", "parent"), by.y=c("node", "parent"))
##         } else {
##             res <- merge(df, info, by.x="node", by.y="node")
##         }
##     } else {
##         return(df)
##     }

##     i <- order(res$node, decreasing = FALSE)
##     res <- res[i,]
##     return(res)
## }







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

## extract.treeinfo.jplace <- function(object, layout="phylogram", ladderize=TRUE, right=FALSE, ...) {

##     tree <- get.tree(object)

##     df <- fortify.phylo(tree, layout=layout, ladderize=ladderize, right=right, ...)

##     edgeNum.df <- attr(tree, "edgeNum")
##     if (!is.null(edgeNum.df)) {
##         df2 <- merge(df, edgeNum.df, by.x="node", by.y="node", all.x=TRUE)
##         df <- df2[match(df[, "node"], df2[, "node"]),]
##     }

##     attr(df, "ladderize") <- ladderize
##     attr(df, "right") <- right
##     return(df)
## }

## convert edge number to node number for EPA/pplacer output
edgeNum2nodeNum <- function(jp, edgeNum) {
    edges <- attr(jp@phylo, "edgeNum")

    idx <- which(edges$edgeNum == edgeNum)
    if (length(idx) == 0) {
        return(NA)
    }

    edges[idx, "node"]
}

## is.character_beast <- function(stats3, cn) {
##     for (i in 1:nrow(stats3)) {
##         if ( is.na(stats3[i,cn]) ) {
##             next
##         } else {
##             ## res <- grepl("[a-df-zA-DF-Z]+", unlist(stats3[i, cn]))
##             ## return(all(res == TRUE))
##             res <- grepl("^[0-9\\.eE-]+$", unlist(stats3[i, cn]))
##             return(all(res == FALSE))
##         }
##     }
##     return(FALSE)
## }


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

is.tree_attribute <- function(df, var) {
    if(length(var) == 1 &&
       !is.null(var)    &&
       var %in% colnames(df)) {
        return(TRUE)
    }
    return(FALSE)
}

is.tree_attribute_ <- function(p, var) {
    is.tree_attribute(p$data, var)
}


has.extraInfo <- function(object) {
    if (!is.tree(object)) {
        return(FALSE)
    }

    if (! .hasSlot(object, "extraInfo")) {
        return(FALSE)
    }

    extraInfo <- object@extraInfo

    if (nrow(extraInfo) > 0) {
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


get.offspring <- function(tree, node) {
    sp <- getChild(tree, node)
    sp <- sp[sp != 0]
    if (length(sp) == 0) {
        stop("input node is a tip...")
    }
    i <- 1
    while (i <= length(sp)) {
        sp <- c(sp, getChild(tree, sp[i]))
        sp <- sp[sp != 0]
        i <- i + 1
    }
    return(sp)
}
