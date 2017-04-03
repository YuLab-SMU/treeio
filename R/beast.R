
##' read beast output
##'
##'
##' @title read.beast
##' @param file beast file
##' @return \code{beast} object
##' @importFrom ape read.nexus
##' @export
##' @author Guangchuang Yu \url{https://guangchuangyu.github.io}
##' @examples
##' file <- system.file("extdata/BEAST", "beast_mcc.tree", package="treeio")
##' read.beast(file)
read.beast <- function(file) {
    translation <- read.trans_beast(file)
    treetext <- read.treetext_beast(file)
    stats <- read.stats_beast(file)
    phylo <- read.nexus(file)

    if (length(treetext) == 1) {
        obj <- BEAST(file, treetext, translation, stats, phylo)
    } else {
        obj <- lapply(seq_along(treetext), function(i) {
            BEAST(file, treetext[i], translation, stats[[i]], phylo[[i]])
        })
        class(obj) <- "beastList"
    }
    return(obj)
}


BEAST <- function(file, treetext, translation, stats, phylo) {
    stats$node %<>% gsub("\"*'*", "", .)

    fields <- sub("_lower|_upper", "", names(stats)) %>% unique
    fields %<>% `[`(.!="node")

    phylo <- remove_quote_in_tree_label(phylo)

    obj <- new("beast",
               fields      = fields,
               treetext    = treetext,
               phylo       = phylo,
               translation = translation,
               stats       = stats,
               file        = filename(file)
               )
    return(obj)
}

remove_quote_in_tree_label <- function(phylo) {
    if (!is.null(phylo$node.label)) {
        phylo$node.label %<>% gsub("\"*'*", "", .)
    }
    if ( !is.null(phylo$tip.label)) {
        phylo$tip.label %<>% gsub("\"*'*", "", .)
    }
    return(phylo)
}


read.treetext_beast <- function(file) {
    beast <- readLines(file)

    ii <- grep("[Bb]egin trees;", beast)
    jj <- grep("[Ee]nd;", beast)
    jj <- jj[jj > max(ii)][1]
    jj <- c(ii[-1], jj)

    trees <- sapply(seq_along(ii), function(i) {
        tree <- beast[(ii[i]+1):(jj[i]-1)]
        tree <- tree[grep("^\\s*[Tt]ree", tree)]
        ## if (length(tree) > 1) {
        ##     tree <- paste0(tree, collapse='')
        ## }
        sub("[^(]*", "", tree)
    })

    return(trees)
}

read.trans_beast <- function(file) {
    beast <- readLines(file)
    i <- grep("TRANSLATE", beast, ignore.case = TRUE)
    if (length(i) == 0) {
        return(matrix())
    }
    end <- grep(";", beast)
    j <- end[end %>% `>`(i) %>% which %>% `[`(1)]
    trans <- beast[(i+1):j]
    trans %<>% gsub("\\t+", "", .)
    trans %<>% gsub(",|;", "", .)
    trans %<>% `[`(nzchar(trans))
    ## remove quote if strings were quoted
    trans %<>% gsub("'|\"", "",.)
    trans %<>% sapply(., strsplit, split="\\s+")
    trans %<>% do.call(rbind, .)
    ## trans is a matrix
    return(trans)
}


read.stats_beast <- function(file) {
    beast <- readLines(file)
    trees <- read.treetext_beast(file)
    if (length(trees) == 1) {
        return(read.stats_beast_internal(beast, trees))
    }
    lapply(trees, read.stats_beast_internal, beast=beast)
}



read.stats_beast_internal <- function(beast, tree) {
    tree2 <- gsub("\\[[^\\[]*\\]", "", tree)
    phylo <- read.tree(text = tree2)

    tree2 <- add_pseudo_nodelabel(phylo, tree2)

    ## node name corresponding to stats
    nn <- strsplit(tree2, split=",") %>% unlist %>%
        strsplit(., split="\\)") %>% unlist %>%
        gsub("\\(*", "", .) %>%
        gsub("[:;].*", "", .)

    phylo <- read.tree(text = tree2)
    root <- getRoot(phylo)
    nnode <- phylo$Nnode

    ## phylo2 <- read.nexus(file)
    ## treeinfo <- fortify.phylo(phylo)
    ## treeinfo2 <- fortify.phylo(phylo2)
    ## treeinfo$label2 <- NA
    ## treeinfo$label2[treeinfo$isTip] <- treeinfo2$node[as.numeric(treeinfo$label[treeinfo$isTip])]
    ## treeinfo$visited <- FALSE
    ## root <- getRoot(phylo2)
    ## treeinfo[root, "visited"] <- TRUE
    ## currentNode <- 1:Ntip(phylo2)
    ## while(any(treeinfo$visited == FALSE)) {
    ##     pNode <- c()
    ##     for (kk in currentNode) {
    ##         i <- which(treeinfo$label2 == kk)
    ##         treeinfo[i, "visited"] <- TRUE
    ##         j <- which(treeinfo2$node == kk)
    ##         ip <- treeinfo$parent[i]
    ##         if (ip != root) {
    ##             ii <- which(treeinfo$node == ip)
    ##             if (treeinfo$visited[ii] == FALSE) {
    ##                 jp <- treeinfo2$parent[j]
    ##                 jj <- which(treeinfo2$node == jp)
    ##                 treeinfo[ii, "label2"] <- treeinfo2[jj, "node"]
    ##                 pNode <- c(pNode, jp)
    ##             }
    ##             treeinfo[ii, "visited"] <- TRUE
    ##         }
    ##     }
    ##     currentNode <- unique(pNode)
    ## }
    ## treeinfo[root, "label2"] <- root
    ## ## convert nn to node that encoded in phylo2
    ## node <- treeinfo$label2[match(nn, treeinfo$label)]


    ####################################################
    ##                                                ##
    ##  after doing it in the hard way                ##
    ##  I finally figure out the following easy way   ##
    ##                                                ##
    ####################################################

    ## treeinfo <- fortify.phylo(phylo)
    tree_label <- c(phylo$tip.label, phylo$node.label)
    ii <- match(nn, tree_label)

    if (any(grepl("TRANSLATE", beast, ignore.case = TRUE))) {
        label2 <- c(phylo$tip.label,
                    root:getNodeNum(phylo))
        ## label2 <- c(treeinfo[treeinfo$isTip, "label"],
        ##             root:(root+nnode-1))

    } else {
        ## node <- as.character(treeinfo$node[match(nn, treeinfo$label)])
        label2 <- as.character(1:getNodeNum(phylo))
    }
    node <- label2[match(nn, tree_label)]

    ## stats <- unlist(strsplit(tree, "\\["))[-1]
    ## stats <- sub(":.+$", "", stats

    if (grepl("\\]:[0-9\\.eE+\\-]*\\[", tree) || grepl("\\]\\[", tree)) {
        ## MrBayes output
        stats <- strsplit(tree, "\\]:[0-9\\.eE+\\-]*\\[") %>% unlist
        lstats <- lapply(stats, function(x) unlist(strsplit(x, split="\\][,\\)]")))

        for (i in seq_along(stats)) {
            n <- length(lstats[[i]])
            if (i == length(stats)) {
                stats[i] <- lstats[[i]][n]
            } else {
                stats[i] <- paste0(lstats[[i]][n], sub("&", ",", lstats[[i+1]][1]))
            }
        }
        stats <- gsub("\\]\\[&", ",", stats)
    } else {
        ## BEAST output
        stats <- strsplit(tree, ":") %>% unlist
    }

    names(stats) <- node

    stats <- stats[grep("\\[", stats)]
    stats <- sub("[^\\[]*\\[", "", stats)

    stats <- sub("^&", "", stats)
    stats <- sub("];*$", "", stats)
    stats <- gsub("\"", "", stats)

    stats2 <- lapply(seq_along(stats), function(i) {
        x <- stats[[i]]
        y <- unlist(strsplit(x, ","))
        sidx <- grep("=\\{", y)
        eidx <- grep("\\}$", y)

        flag <- FALSE
        if (length(sidx) > 0) {
            flag <- TRUE
            SETS <- sapply(seq_along(sidx), function(k) {
                p <- y[sidx[k]:eidx[k]]
                gsub(".*=\\{", "", p) %>% gsub("\\}$", "", .) %>% list
            })
            names(SETS) <- gsub("=.*", "", y[sidx])

            kk <- sapply(seq_along(sidx), function(k) sidx[k]:eidx[k]) %>% unlist
            y <- y[-kk]
        }

        name <- gsub("=.*", "", y)
        val <- gsub(".*=", "", y) %>% gsub("^\\{", "", .) %>%
            gsub("\\}$", "", .)

        if (flag) {
            nn <- c(name, names(SETS))
        } else {
            nn <- name
        }

        res <- rep(NA, length(nn))
        names(res) <- nn

        for (i in seq_along(name)) {
            res[i] <- if(is_numeric(val[i])) as.numeric(val[i]) else val[i]
        }
        if (flag) {
            j <- i
            for (i in seq_along(SETS)) {
                res[i+j] <- if(is_numeric(SETS[[i]])) list(as.numeric(SETS[[i]])) else SETS[i]
            }
        }

        return(res)
    })

    nn <- lapply(stats2, names) %>% unlist %>%
        unique %>% sort


    stats2 <- lapply(stats2, function(x) {
        y <- x[nn]
        names(y) <- nn
        y[sapply(y, is.null)] <- NA
        y
    })

    stats3 <- do.call(rbind, stats2)
    stats3 <- as.data.frame(stats3, stringsAsFactor = FALSE)
    idx <- grep("\\+-", colnames(stats3))
    if (length(idx)) {
        for (i in idx) {
            stats3[,i] <- as.numeric(gsub("\\d+\\+-", "", stats3[,i]))
        }
    }

    cn <- gsub("(\\d+)%", "0.\\1", colnames(stats3))
    cn <- gsub("\\(([^\\)]+)\\)", "_\\1", cn)
    cn <- gsub("\\+-", "_", cn)

    colnames(stats3) <- cn
    stats3$node <- names(stats)

    i <- sapply(stats3, function(x) max(sapply(x, length)))
    for (j in which(i==1)) {
        stats3[,j] <- unlist(stats3[,j])
    }
    return(stats3)
}


add_pseudo_nodelabel <- function(phylo, treetext) {
    if(is.null(phylo$node.label)) {
        nnode <- phylo$Nnode
        nlab <- paste("X", 1:nnode, sep="")
        for (i in 1:nnode) {
            treetext <- sub("\\)([:;])", paste0("\\)", nlab[i], "\\1"), treetext)
        }
    }

   return(treetext)
}



