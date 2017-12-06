string2DNAbin <- function(seqs) {
    seqlist <- strsplit(seqs, "")
    names(seqlist) <- names(seqs)
    as.DNAbin(seqlist)
}

