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
##' @param type sequence type of the input file, one of 'NT' or 'AA'.
##' Default is 'auto' and guess the sequence type automatically 
##' @return DNAbin or AAbin object
##' @export
##' @author Guangchuang Yu
read.fasta <- function(fasta, type = "auto") {
    type <- match.arg(type, c("auto", "NT", "AA"))

    if (type == "auto") type <- guess_fasta_type(fasta)

    if (type == "NT") {
        class <- "DNAbin"
    } else {
        class <- "AAbin"
    }

    x <- Biostrings::readBStringSet(fasta)

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

#guess_fasta_type <- function(fasta) {
#    ## read second line, the sequence line and convert it to character vector
#    a <- strsplit(toupper(readLines(fasta, n=2)[2]), split="")[[1]]
#    freq <- mean(a %in% c('A', 'C', 'G', 'T', 'X', 'N', '-') )
#    if (freq > 0.9) {
#        return('NT')
#    }
#    return('AA')
#}

guess_fasta_type <- function(fasta) {
    seqstr <- readLines(fasta, n=3)
    if (length(seqstr)==2 || grepl("^>", seqstr[[3]])){
        # >seq1
        # AGCGTACGTGACGTAGCGTAGC
        # >seq2
        a <- seqstr[2]
    }else{
        # >seq1
        # ---------
        # AGCG----C
        # ---------
        # >seq2
        seqstr <- readLines(fasta, n=20)
        seqind <- grep("^>", seqstr)
        if (length(seqind)==1){
            a <- paste0(seqstr[-1], collapse="")
        }else{
            inds <- seqind[1] + 1
            inde <- seqind[2] - 1
            a <- paste0(seqstr[inds:inde], collapse="")
        }
    }
    a <- strsplit(toupper(a), split="")[[1]]
    freq <- mean(a %in% c('A', 'C', 'G', 'T', 'X', 'N', '-'))
    # -------KKKKKKK------KKKK----------
    freq2 <- mean(a %in% c("A", "C", "G", "T", "N"))
    if (freq > 0.9 && freq2 > 0) {
        return('NT')
    }
    return('AA')
}
