##' @importFrom ape as.DNAbin
string2DNAbin <- function(seqs) {
    seqlist <- strsplit(seqs, "")
    names(seqlist) <- names(seqs)
    as.DNAbin(seqlist)
}

## seq2codon <- function(x) {
##     substring(x, first=seq(1, nchar(x)-2, 3), last=seq(3, nchar(x), 3))
## }

## codon2AA <- function(codon) {
##     ## a genetic code name vector
##     aa <- Biostrings::GENETIC_CODE[codon]
##     aa[is.na(aa)] <- "X"
##     return(aa)
## }



##' read FASTA file
##'
##' This function supports both DNA or AA sequences
##' @title read.fasta
##' @param fasta fasta file
##' @return DNAbin or AAbin object
##' @export
##' @author guangchuang yu
read.fasta <- function(fasta) {
    x <- Biostrings::readBStringSet(fasta)

    if (guess_fasta_type(fasta) == "NT") {
        class <- "DNAbin"
    } else {
        class <- "AAbin"
    }

    structure(lapply(x, .BStringSet2bin, class = class),
              class = class)
}

##' @importFrom ape as.DNAbin.character
##' @importFrom ape as.AAbin.character
.BStringSet2bin <- function(x, class = "DNAbin") {
    chars <- strsplit(tolower(as.character(x)), "")[[1]]
    if (class == "DNAbin") {
        res <- as.DNAbin.character(chars)
    } else {
        res <- as.AAbin.character(chars)
    }
    return(res)
}

guess_fasta_type <- function(fasta) {
    ## read second line, the sequence line and convert it to character vector
    a <- strsplit(toupper(readLines(fasta, n=2)[2]), split="")[[1]]
    freq <- mean(a %in% c('A', 'C', 'G', 'T', 'X', 'N', '-') )
    if (freq > 0.9) {
        return('NT')
    }
    return('AA')
}
