##  tree annotation of sequence substitution by comparing to parent node
## 
## 
##  @title phyPML
##  @param pmlTree tree in pml object, output of phangorn::optim.pml
##  @rdname as.treedata
##  @param type one of 'ml' and 'bayes' for inferring ancestral sequences
##  @return treedata object

##' @importFrom ape read.tree
##' @importFrom ape reorder.phylo
##' @method as.treedata pml
##' @export
##' @author Yu Guangchuang
as.treedata.pml <- function(tree, type = "ml", ...) {
    sequences <- pmlToSeqString(tree, type, includeAncestor=TRUE)
    tr <- tree$tree
    tr <- reorder.phylo(tr)
    
    if (is.null(tr$node.label)) {
        n <- Ntip(tr)
        nl <- 1:(Nnode2(tr) - n) + n
        tr$node.label <- as.character(nl)
    }
    names(sequences) <- c(tr$tip.label, tr$node.label)


    seq_type <- get_seqtype(sequences)

    ## seqlist <- lapply(seq_along(sequences), function(i) sequences[i])
    ## names(seqlist) <- names(sequences)
    ## seqs <- as.DNAbin(seqlist)
    seqs <- string2DNAbin(sequences)

    tip_seq <- seqs[labels(seqs) %in% tr$tip.label]
    anc_seq <- seqs[!labels(seqs) %in% tr$tip.label]

    res <- new("treedata",
               phylo = tr,
               tip_seq = tip_seq,
               anc_seq = anc_seq,
               seq_type = seq_type)

    set_substitution(res)
}



pmlToSeqString <- function(pml, type, includeAncestor=TRUE) {
    if (includeAncestor == FALSE) {
        phyDat <- pml$data
    } else {
        check_installed('phangorn', 'for `as.treedata()` with pml class and includeAncestor=TRUE.')
        phyDat <- phangorn::ancestral.pml(pml, type)
    }

    phyDat <- matrix2vector.phyDat(phyDat)
    ## defined by phangorn
    labels <- c("a", "c", "g", "t", "u", "m", "r", "w", "s",
                "y", "k", "v", "h", "d", "b", "n", "?", "-")
    labels <- toupper(labels)

    index <- attr(phyDat, "index")

    result <- do.call(rbind, phyDat)
    result <- result[, index, drop=FALSE]

    res <- apply(result, 2, function(i) labels[i])
    res <- apply(res, 1, paste, collapse="")
    names(res) <- rownames(result)
    return(res)
}



matrix2vector.phyDat <- function(x) {
    index <- attr(x, "index")
    res <- lapply(x, matrix2vector.phyDat.item)
    names(res) <- names(x)
    attr(res, "index") <- index
    class(res) <- "phyDat"
    return(res)
}

matrix2vector.phyDat.item <- function(y) {
    ii <- apply(y, 1, function(xx) {
        ## return index of a c g and t, if it has highest probability
        ## otherwise return index of -
        jj <- which(xx == max(xx))
        if ( length(jj) > 1) {
            if (length(jj) < 4) {
                warning("ambiguous found...\n")
            } else {
                ## cat("insertion found...\n")
            }
            ## 18 is the gap(-) index of base character defined in phangorn
            ## c("a", "c", "g", "t", "u", "m", "r", "w", "s",
            ##   "y", "k", "v", "h", "d", "b", "n", "?", "-")
            18
        } else {
            jj
        }
    })
    unlist(ii)
}

