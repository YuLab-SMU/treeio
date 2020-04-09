##' parsing phylip tree format
##'
##'
##' @title read.phylip
##' @param file phylip file
##' @return an instance of 'phylip'
##' @export
##' @examples
##' phyfile <- system.file("extdata", "sample.phy", package="treeio")
##' read.phylip(phyfile)
##' @author Guangchuang Yu
read.phylip <- function(file) {
    tree <- read.phylip.tree(file)
    if (is(tree, 'multiPhylo')) {
        msg <- paste("input file contains multiple trees and",
                     "it is currently not supported ...")
        stop(msg)
    }
    new("treedata",
        file = filename(file),
        phylo = tree,
        tip_seq = read.phylip.seq(file)
        )
}

##' read aligned sequences from phylip format
##'
##'
##' @title read.phylip.seq
##' @param file phylip file, currently only sequential format is supported
##' @return DNAbin object
##' @export
##' @author Guangchuang Yu
##' @references \url{http://evolution.genetics.washington.edu/phylip/doc/sequence.html}
read.phylip.seq <- function(file) {
    phylip <- readLines(file)

    phylipInfo <- strsplit(phylip[1], split="\\s") %>% unlist
    nseq <- phylipInfo[1]
    seqLen <- phylipInfo[2]

    if (nchar(sub('.+\\s+', "", phylip[2])) != phylipInfo[2]) {
        stop("only sequential format is supported...")
    }
    seqlines <- phylip[1+(1:phylipInfo[1])]
    seq_with_name <- lapply(seqlines, function(x) unlist(strsplit(x, "\\s+")))
    seqs <- vapply(seq_with_name, function(x) x[2], character(1))
    names(seqs) <- vapply(seq_with_name, function(x) x[1], character(1))

    if (any(nchar(seqs) != seqLen)) {
        stop(paste("sequence length not consistent...\n->",
                   paste0(nchar(seqs), collapse=" ")))
    }

    res <- string2DNAbin(seqs)
    attr(res, "seq_type") <- get_seqtype(seqs[1])
    return(res)
}


##' parse tree from phylip file
##'
##'
##' @title read.phylip.tree
##' @param file phylip file
##' @return phylo or multiPhylo object
##' @export
##' @author Guangchuang Yu
read.phylip.tree <- function(file) {
    phylip <- readLines(file)
    i <- grep("^\\d+$", phylip)
    if (length(i) != 1) {
        stop("input file is not phylip tree format...")
    }
    n <- length(phylip)
    ntree <- as.numeric(phylip[i])
    trees <- read.tree(text=phylip[(i+1):n])
    return(trees)
}

getPhyInfo <- function(phy) {
    line1 <- readLines(phy, n=1)
    res <- strsplit(line1, split="\\s")[[1]]
    res <- res[res != ""]

    return(list(num=as.numeric(res[1]), width=as.numeric(res[2])))
}
