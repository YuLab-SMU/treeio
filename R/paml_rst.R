
##' read rst file from paml (both baseml and codeml) output
##'
##'
##' @title read.paml_rst
##' @param rstfile rst file
##' @param type one of 'Marginal' or 'Joint'
##' @return A \code{treedata} object
##' @export
##' @author Guangchuang Yu \url{https://guangchuangyu.github.io}
##' @examples
##' rstfile <- system.file("extdata/PAML_Baseml", "rst", package="treeio")
##' read.paml_rst(rstfile)
read.paml_rst <- function(rstfile, type = "Joint") {
    phylo <- read.phylo_paml_rst(rstfile)
    seqs <- read.ancseq_paml_rst(rstfile, by = type)
    seq_type <- get_seqtype(seqs[1])
    seqs <- string2DNAbin(seqs)

    tip_seq <- seqs[labels(seqs) %in% phylo$tip.label]
    anc_seq <- seqs[!labels(seqs) %in% phylo$tip.label]


    res <- new("treedata",
               treetext = read.treetext_paml_rst(rstfile),
               phylo    = phylo,
               seq_type = seq_type,
               anc_seq  = anc_seq,
               tip_seq  = tip_seq,
               file     = filename(rstfile),
               info     = list(type = type)
               )

    set_substitution(res)

}
