##' parse sequences from hyphy output
##'
##'
##' @title read.hyphy.seq
##' @param file output of hyphy ancestral sequence inference; nexus format
##' @return DNAbin object
##' @export
##' @examples
##' ancseq <- system.file("extdata/HYPHY", "ancseq.nex", package="treeio")
##' read.hyphy.seq(ancseq)
##' @author Guangchuang Yu
read.hyphy.seq <- function(file) {
    anc <- scan(file, what="", sep="\n", quiet=TRUE)
    end <- grep("END;", anc, ignore.case=TRUE)

    seq.start <- grep("MATRIX", anc, ignore.case=TRUE)
    seq.end   <- end[end > seq.start][1]
    seq       <- anc[(seq.start+1):(seq.end-1)]
    seq       <- seq[seq != ";"]
    seq       <- gsub("^\\s+", "", seq)
    seq       <- gsub(";", "", seq)
    seq       <- seq[seq != ""]

    ## some files may only contains sequences
    ## (should have TAXALABELS block that contains seq names).
    ## some may contains sequence name like phylip format in
    ## MATRIX block (no need to have TAXALABELS block).
    ##
    ## extract sequence name if available
    if (all(grepl("\\s+", seq))) {
        ## if contains blank space, may contains seq name
        sn <- gsub("(\\w*)\\s.*", "\\1", seq)
    }

    seq <- gsub("\\w*\\s+", "", seq)

    label.start <- grep("TAXLABELS", anc, ignore.case = TRUE)
    if (length(label.start) == 0) {
        if (all(sn == "")) {
            stop("taxa labels is not available...")
        }
        label <- sn
    } else {
        label.end   <- end[end > label.start][1]
        label       <- anc[(label.start+1):(label.end-1)]

        label <- sub("^\t+", "", label)
        label <- sub("^\\s+", "", label)
        label <- sub("\\s*;*$", "", label)
        label <- label[label != ""]
        label <- unlist(strsplit(label, split="\\s+"))
        label <- gsub("'|\"", "", label)
    }

    names(seq) <- label
    res <- string2DNAbin(seq)

    attr(res, "seq_type") <- get_seqtype(seq[1])
    return(res)
}

##' read HYPHY output
##'
##'
##' @title read.hyphy
##' @param nwk tree file in nwk format, one of hyphy output
##' @param ancseq ancestral sequence file in nexus format,
##'               one of hyphy output
##' @param tip.fasfile tip sequence file
##' @return A hyphy object
##' @importFrom ape read.FASTA
##' @export
##' @author Guangchuang Yu \url{https://guangchuangyu.github.io}
##' @examples
##' nwk <- system.file("extdata/HYPHY", "labelledtree.tree", package="treeio")
##' ancseq <- system.file("extdata/HYPHY", "ancseq.nex", package="treeio")
##' read.hyphy(nwk, ancseq)
read.hyphy <- function(nwk, ancseq, tip.fasfile=NULL) {
    anc_seq <- read.hyphy.seq(ancseq)
    seq_type <- attr(anc_seq, 'seq_type')

    tr <- read.tree(nwk)
    nl <- tr$node.label
    ## root node may missing, which was supposed to be 'Node1'
    ##
    ## from a user's file, which is 'Node0', but it seems
    ## the file is not from the output of HYPHY.
    ##
    ## I am not sure. But it's safe to use "label[!label %in% nl]"
    ## instead of just assign it to "Node1".
    ##
    ## nl[nl == ""] <- "Node1"
    label <- labels(anc_seq)
    nl[nl == ""] <- label[!label %in% nl]
    tr$node.label <- nl

    res <- new("treedata",
               treetext = scan(nwk, what='', quiet=TRUE),
               phylo = tr,
               seq_type = seq_type,
               anc_seq = anc_seq,
               file = filename(nwk),
               ancseq_file = ancseq,
               info = list(parser = "read.hyphy")
               )

    if ( !is.null(tip.fasfile) ) {
        res@tipseq_file <- tip.fasfile
        res@tip_seq <- read.FASTA(tip.fasfile)
    }
    set_substitution(res)
}


set_substitution <- function(object, ...) {
    if (length(object@tip_seq) == 0) {
        return(object)
    }

    if (length(object@anc_seq) == 0) {
        return(object)
    }

    subs <- get.subs_(object, translate=FALSE, ...)

    if (object@seq_type == "NT") {
        AA_subs <- get.subs_(object, translate=TRUE, ...)
        AA_subs <- rename(AA_subs, AA_subs = .data$subs)
        subs <- full_join(subs, AA_subs, by=c("node", "parent"))
    }
    subs <- select(subs, - .data$parent)

    if (nrow(object@data) == 0) {
        object@data <- subs
    } else {
        object@data <- full_join(object@data, subs, by='node')
    }
    return(object)
}


get.subs_ <- function(object, translate=TRUE, removeGap=TRUE) {
    tree <- as.phylo(object)
    ancseq <- object@anc_seq
    tipseq <- object@tip_seq


    N <- getNodeNum(tree)
    node <- 1:N
    ## pp <- vapply(node, function(n) parent(tree, n), numeric(1))
    pp <- parent(tree, node)

    label <- getNodeName(tree)
    seqs <- c(as.list(ancseq), as.list(tipseq))
    subs <- vapply(seq_along(node), function(i) {
        if (i == rootnode(tree)) {
            return('')
        }

        res <- getSubsLabel(seqs, label[pp[i]], label[i], translate, removeGap)
        if (is.null(res)) {
            return('')
        }
        return(res)
    }, character(1))

    ## if `subs` is too long to plot, user can use
    ## `stringr::str_wrap` to format the text
    dd <- tibble(node=node, parent=pp, subs=subs)
    dd <- dd[dd$parent != 0,]
    return(dd)
}

##' @importFrom ape trans
getSubsLabel <- function(seqs, A, B, translate, removeGap) {
    seqA <- seqs[A]
    seqB <- seqs[B]
    if (translate) {
        seqA <- trans(seqA)
        seqB <- trans(seqB)
    }

    AA <- unlist(as.character(seqA))
    BB <- unlist(as.character(seqB))

    if (length(AA) != length(BB)) {
        stop("seqA should have equal length to seqB")
    }

    ii <- which(AA != BB)

    if (removeGap == TRUE) {
        if (length(ii) > 0 && translate == TRUE) {
            ii <- ii[AA[ii] != "X" & BB[ii] != "X"]
        }

        if (length(ii) > 0 && translate == FALSE) {
            ii <- ii[AA[ii] != "-" & BB[ii] != "-"]
        }
    }

    if (length(ii) == 0) {
        return(NULL)
    }

    res <- paste(AA[ii], ii, BB[ii], sep="", collapse=" / ")
    return(toupper(res))
}


get_seqtype <- function(seq) {
    if (grepl("[-ACGT]+", seq[1])) {
        seq_type <- "NT" ## NucleoTide
    } else {
        seq_type <- "AA" ## Amino Acid
    }
    return(seq_type)
}
