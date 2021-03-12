##' Export \code{treedata} object to BEAST NEXUS file. This function was adopted and modified from ape::write.nexus
##'
##'
##' @title write.beast
##' @param treedata \code{treedata} object
##' @param file output file. If file = "", print the output content on screen
##' @param translate whether translate taxa labels
##' @param tree.name name of the tree
##' @return output file or file content on screen
##' @importFrom ape .compressTipLabel
##' @importFrom ape .uncompressTipLabel
##' @importFrom tidytree get_tree_data
##' @export
##' @examples
##' nhxfile <- system.file("extdata/NHX", "phyldog.nhx", package="treeio")
##' nhx <- read.nhx(nhxfile)
##' write.beast(nhx)
##' @author Guangchuang Yu
write.beast <- function(treedata, file = "",
                        translate = TRUE, tree.name = "UNTITLED") {
    
    cat("#NEXUS\n", file = file)
    cat(paste("[R-package treeio, ", date(), "]\n\n", sep = ""),
        file = file, append = TRUE)
    
    N <- Ntip(treedata)
    
    obj <- list(as.phylo(treedata))
    ntree <- length(obj)
    cat("BEGIN TAXA;\n", file = file, append = TRUE)
    cat(paste("\tDIMENSIONS NTAX = ", N, ";\n", sep = ""),
        file = file, append = TRUE)
    cat("\tTAXLABELS\n", file = file, append = TRUE)
    cat(paste("\t\t", obj[[1]]$tip.label, sep = ""),
        sep = "\n", file = file, append = TRUE)
    cat("\t;\n", file = file, append = TRUE)
    cat("END;\n", file = file, append = TRUE)
    
    cat("BEGIN TREES;\n", file = file, append = TRUE)
    if (translate) {
        cat("\tTRANSLATE\n", file = file, append = TRUE)
        obj <- .compressTipLabel(obj)
        X <- paste("\t\t", 1:N, "\t", attr(obj, "TipLabel"), ",", sep = "")
        ## We remove the last comma:
        X[length(X)] <- gsub(",", "", X[length(X)])
        cat(X, file = file, append = TRUE, sep = "\n")
        cat("\t;\n", file = file, append = TRUE)
        class(obj) <- NULL
        for (i in 1:ntree)
            obj[[i]]$tip.label <- as.character(1:N)
    } else {
        if (is.null(attr(obj, "TipLabel"))) {
            for (i in 1:ntree)
                obj[[i]]$tip.label <- checkLabel(obj[[i]]$tip.label)
        } else {
            attr(obj, "TipLabel") <- checkLabel(attr(obj, "TipLabel"))
            obj <- .uncompressTipLabel(obj)
        }
    }
    treedata@phylo <- obj[[1]]
    
    root.tag <- if (is.rooted(treedata)) "= [&R] " else "= [&U] "
    cat("\tTREE *", tree.name, root.tag, file = file, append = TRUE)
    cat(write_beast_newick(treedata, file = ""),
        "\n", sep = "", file = file, append = TRUE)
    
    cat("END;\n", file = file, append = TRUE)
}

##' Export \code{treedata} object to BEAST Newick file. This is useful for making BEAST starting trees with metadata
##'
##'
##' @title write.beast.newick
##' @param treedata \code{treedata} object
##' @param file output file. If file = "", print the output content on screen
##' @param append logical. Only used if the argument 'file' is the name of file
##' (and not a connection or "|cmd").  If 'TRUE' output will be appended to 
##' 'file'; otherwise, it will overwrite the contents of file.
##' @param digits integer, the indicating the number of decimal places, default is 10.
##' @param tree.prefix, character the tree prefix, default is "". 
##' @return output file or file content on screen
##' @export
##' @examples
##' nhxfile <- system.file("extdata/NHX", "phyldog.nhx", package="treeio")
##' nhx <- read.nhx(nhxfile)
##' write.beast.newick(nhx)
##' @author Guangchuang Yu
write.beast.newick <- function(treedata, file = "",
                               append = FALSE, digits = 10, tree.prefix = "") {
    
    phy <- as.phylo(treedata)
    
    anno <- get_tree_data(treedata)
    anno$node <- as.integer(anno$node)
    ## currently substitution is not supported
    anno <- anno[!colnames(anno) %in% c('subs', "AA_subs")]
    
    cn <- colnames(anno)
    col_type <- vapply(anno, class, character(1))
    yy <- lapply(which(!cn %in% c('node', 'label')), function(i) {
        v <- cn[i]
        ## apply sprintf(f.d, anno[[v]]) to round digits?
        
        if (col_type[i] == "list") {
            rr <- paste0(
                v, "={",
                vapply(anno[[v]], function(x) {
                    paste(x, collapse=',')
                }, character(1)),
                "}")
        } else {
            rr <- paste0(v, '=', anno[[v]])
        }
        rr[is.na(anno[[v]])] <- NA
        return(rr)
    }) %>% do.call('cbind', .)
    
    anno_text <- vapply(seq_len(nrow(yy)), function(i) {
        rr <- yy[i,]
        rr <- rr[!is.na(rr)]
        if (length(rr) == 0) {
            return("")
        }
        paste0('[&', paste(rr, collapse=','), ']')
    }, character(1))
    node_anno <- rep(NA, max(anno$node))
    node_anno[anno$node] <- anno_text
    
    res <- .write.tree3(phy, digits = digits,
                        tree.prefix = tree.prefix, node_anno = node_anno)
    
    if (file == "") return(res)
    
    cat(res, file = file, append = append, sep = "\n")
}

.write.tree3 <- function(phy, digits = 10, tree.prefix = "", node_anno = NULL) {
    node.label <- checkLabel(phy$node.label)
    if (length(node.label) == 0) node.label <- NULL
    
    .write.tree4(phy$edge, digits = digits, tree.prefix = tree.prefix,
                 root.edge = phy$root.edge, edge.length = phy$edge.length,
                 tip.label = checkLabel(phy$tip.label),
                 node.label = node.label,
                 node_anno = node_anno)
}

# to preserve old name
write_beast_newick <- write.beast.newick

## derived from .write.tree3_old to tailor for edge list, then all tree-like graph can be supported.
.write.tree4 <- function(edge, digits = 10, tree.prefix = "", root.edge=NULL, edge.length = NULL,
                         tip.label=NULL, node.label=NULL, id_as_label = FALSE, node_anno = NULL) {
    
    edge.label <- edge
    edge <- matrix(as.numeric(as.factor(edge.label)), ncol=2)
    
    f.d <- paste("%.", digits, "g", sep = "")
    
    to_tiplab <- function(edge, i) {
        edge.label[edge[,2] == i, 2][1]
    }
    
    to_nodelab <- function(edge, i) {
        edge.label[edge[,1] == i, 1][1]
    }
    
    brl <- !is.null(edge.length)
    nodelab <- !is.null(node.label)
    
    if (id_as_label) nodelab <- TRUE
    
    cp <- function(x){
        STRING[k] <<- x
        k <<- k + 1
    }
    
    add.internal <- function(i) {
        cp("(")
        desc <- kids[[i]]
        for (j in desc) {
            if (j %in% edge[,1]) add.internal(j)
            ## if (j > n) add.internal(j)
            else add.terminal(ind[j])
            if (j != desc[length(desc)]) cp(",")
        }
        cp(")")
        ## if (nodelab && i > n) {
        if (nodelab && i %in% edge[,1]) {
            ## cp(phy$node.label[i - n]) # fixed by Naim Matasci (2010-12-07)
            if (is.null(node_anno)) {
                if (id_as_label) nl <- to_nodelab(edge, i)
                else nl <- node.label[i - n]
            } else if (!is.na(node_anno[i])) {
                if (id_as_label) nl <- paste0(to_nodelab(edge, i), node_anno[i])
                else nl <- paste0(node.label[i - n], node_anno[i])
            }
            cp(nl)
        } else if (i %in% edge[,1] && !is.null(node_anno)) {
            cp(node_anno[i])
        }
        if (brl) {
            cp(":")
            cp(sprintf(f.d, edge.length[ind[i]]))
        }
    }
    
    add.terminal <- function(i) {
        ii <- edge[i, 2]
        if (is.null(node_anno) || is.na(node_anno[ii])) {
            if (id_as_label) tl <- to_tiplab(edge, ii)
            else tl <- tip.label[ii]
        } else {
            if (id_as_label) tl <- paste0(to_tiplab(edge, ii), node_anno[ii])
            else tl <- paste0(tip.label[ii], node_anno[ii])
        }
        cp(tl)
        if (brl) {
            cp(":")
            cp(sprintf(f.d, edge.length[i]))
        }
    }
    
    Ntip <- function(edge) {
        tip <- edge[,2][!edge[,2] %in% edge[,1]]
        length(tip)
    }
    
    Nnode <- function(edge) {
        tip <- edge[,2][!edge[,2] %in% edge[,1]]
        node <- edge[,1][!edge[,1] %in% tip]
        length(node)
    }
    
    ## borrowed from phangorn:
    parent <- edge[, 1]
    children <- edge[, 2]
    n <- Ntip(edge)
    kids <- vector("list", n + Nnode(edge))
    
    for (i in seq_along(parent))
        kids[[parent[i]]] <- c(kids[[parent[i]]], children[i])
    
    ind <- match(1:max(edge), edge[, 2])
    
    LS <- 4*n + 5
    if (brl) LS <- LS + 4*n
    if (nodelab)  LS <- LS + n
    STRING <- character(LS)
    k <- 1
    cp(tree.prefix)
    cp("(")
    getRoot <- function(edge)
        edge[, 1][!match(edge[, 1], edge[, 2], 0)][1]
    root <- getRoot(edge) # replaced n+1 with root - root has not be n+1
    desc <- kids[[root]]
    for (j in desc) {
        if (j %in% edge[,1]) add.internal(j)
        ## if (j > n) add.internal(j)
        else add.terminal(ind[j])
        if (j != desc[length(desc)]) cp(",")
    }
    
    if (is.null(root.edge)) {
        cp(")")
        if (nodelab) {
            if (!is.null(node_anno) && !is.na(node_anno[root])) {
                if(id_as_label) cp(paste0(to_nodelab(edge, root), node_anno[root]))
                else cp(paste0(node.label[1], node_anno[root]))
            } else {
                if (id_as_label) cp(to_nodelab(edge, root))
                else cp(node.label[1])
            }
        } else if (!is.null(node_anno) && !is.na(node_anno[root])) {
            cp(node_anno[root])
        }
        cp(";")
    } else {
        cp(")")
        if (nodelab) cp(node.label[1])
        cp(":")
        cp(sprintf(f.d, root.edge))
        cp(";")
    }
    paste(STRING, collapse = "")
}


## this function was derived from ape:::.write.tree2
## by adding node/tip label with associated annotation in BEAST format
##' @importFrom ape checkLabel
.write.tree3_old <- function(phy, digits = 10, tree.prefix = "", node_anno = NULL) {
    brl <- !is.null(phy$edge.length)
    nodelab <- !is.null(phy$node.label)
    phy$tip.label <- checkLabel(phy$tip.label)
    if (nodelab) phy$node.label <- checkLabel(phy$node.label)
    f.d <- paste("%.", digits, "g", sep = "")
    
    cp <- function(x){
        STRING[k] <<- x
        k <<- k + 1
    }
    
    add.internal <- function(i) {
        cp("(")
        desc <- kids[[i]]
        for (j in desc) {
            if (j > n) add.internal(j)
            else add.terminal(ind[j])
            if (j != desc[length(desc)]) cp(",")
        }
        cp(")")
        if (nodelab && i > n) {
            ## cp(phy$node.label[i - n]) # fixed by Naim Matasci (2010-12-07)
            if (is.null(node_anno)) {
                nl <- paste0(phy$node.label[i - n])
            } else if (!is.na(node_anno[i])) {
                nl <- paste0(phy$node.label[i - n], node_anno[i])
            }
            cp(nl)
        } else if (i > n && !is.null(node_anno)) {
            cp(node_anno[i])
        }
        if (brl) {
            cp(":")
            cp(sprintf(f.d, phy$edge.length[ind[i]]))
        }
    }
    
    add.terminal <- function(i) {
        ii <- phy$edge[i, 2]
        if (is.null(node_anno) || is.na(node_anno[ii])) {
            tl <- phy$tip.label[ii]
        } else {
            tl <- paste0(phy$tip.label[ii], node_anno[ii])
        }
        cp(tl)
        if (brl) {
            cp(":")
            cp(sprintf(f.d, phy$edge.length[i]))
        }
    }
    
    n <- length(phy$tip.label)
    
    ## borrowed from phangorn:
    parent <- phy$edge[, 1]
    children <- phy$edge[, 2]
    kids <- vector("list", n + phy$Nnode)
    for (i in seq_along(parent))
        kids[[parent[i]]] <- c(kids[[parent[i]]], children[i])
    
    ind <- match(1:max(phy$edge), phy$edge[, 2])
    
    LS <- 4*n + 5
    if (brl) LS <- LS + 4*n
    if (nodelab)  LS <- LS + n
    STRING <- character(LS)
    k <- 1
    cp(tree.prefix)
    cp("(")
    getRoot <- function(phy)
        phy$edge[, 1][!match(phy$edge[, 1], phy$edge[, 2], 0)][1]
    root <- getRoot(phy) # replaced n+1 with root - root has not be n+1
    desc <- kids[[root]]
    for (j in desc) {
        if (j > n) add.internal(j)
        else add.terminal(ind[j])
        if (j != desc[length(desc)]) cp(",")
    }
    
    if (is.null(phy$root.edge)) {
        cp(")")
        if (nodelab) {
            if (!is.null(node_anno) && !is.na(node_anno[root])) {
                cp(paste0(phy$node.label[1], node_anno[root]))
            } else {
                cp(phy$node.label[1])
            }
        } else if (!is.null(node_anno) && !is.na(node_anno[root])) {
            cp(node_anno[root])
        }
        cp(";")
    } else {
        cp(")")
        if (nodelab) cp(phy$node.label[1])
        cp(":")
        cp(sprintf(f.d, phy$root.edge))
        cp(";")
    }
    paste(STRING, collapse = "")
}
