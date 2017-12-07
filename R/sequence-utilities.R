string2DNAbin <- function(seqs) {
    seqlist <- strsplit(seqs, "")
    names(seqlist) <- names(seqs)
    as.DNAbin(seqlist)
}

## seq2codon <- function(x) {
##     substring(x, first=seq(1, nchar(x)-2, 3), last=seq(3, nchar(x), 3))
## }

## ## @importFrom Biostrings GENETIC_CODE
## ##' @importFrom rvcheck get_fun_from_pkg
## codon2AA <- function(codon) {
##     ## a genetic code name vector
##     GENETIC_CODE <- get_fun_from_pkg("Biostrings", "GENETIC_CODE")
##     aa <- GENETIC_CODE[codon]
##     aa[is.na(aa)] <- "X"
##     return(aa)
## }
