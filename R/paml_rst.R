## ##' read rst from baseml output
## ##'
## ##'
## ##' @title read.baseml
## ##' @param rstfile rst file
## ##' @param mlbfile mlb file
## ##' @return A \code{paml_rst} object
## ##' @export
## ##' @author Guangchuang Yu \url{http://ygc.name}
## ##' @examples
## ##' rstfile <- system.file("extdata/PAML_Baseml", "rst", package="treeio")
## ##' read.baseml(rstfile)
## read.baseml <- function(rstfile, mlbfile) {
##     res <- read.paml_rst(rstfile)
##     ## res@tip_seq <- read.tip_seq_mlb(mlbfile)
##     return(res)
## }

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
##     ms <- read.ancseq_paml_rst(rstfile, by="Marginal")
##     ## class(phylo) <- "list"
##     type <- get_seqtype(ms)
##     fields <- c("marginal_subs", "joint_subs")
##     if (type == "NT") {
##         fields <- c(fields, "marginal_AA_subs", "joint_AA_subs")
##     }
##     res <- new("paml_rst",
##                fields          = fields,
##                treetext        = read.treetext_paml_rst(rstfile),
##                phylo           = phylo,
##                seq_type        = type,
##                marginal_ancseq = ms,
##                joint_ancseq    = read.ancseq_paml_rst(rstfile, by = "Joint"),
##                rstfile = filename(rstfile)
##                )


##     ## if (!is.null(tip.fasfile)) {
##     ##     seqs <- readBStringSet(tip.fasfile)
##     ##     tip_seq <- sapply(seq_along(seqs), function(i) {
##     ##         toString(seqs[i])
##     ##     })
##     ##     res@tip_seq <- tip_seq
##     ##     res@tip.fasfile <- tip.fasfile
##     ## }
##     res@tip_seq <- ms[names(ms) %in% phylo$tip.label]

##     res <- set.paml_rst_(res)

##     if (nrow(res@marginal_subs) == 0) {
##         fields <- fields[fields != "marginal_subs"]
##         fields <- fields[fields != "marginal_AA_subs"]
##     }
##     if (nrow(res@joint_subs) == 0) {
##         fields <- fields[fields != "joint_subs"]
##         fields <- fields[fields != "joint_AA_subs"]
##     }
##     res@fields <- fields
##     return(res)
## }




## ##' get tipseq
## ##'
## ##'
## ##' @rdname get.tipseq-methods
## ##' @exportMethod get.tipseq
## setMethod("get.tipseq", signature(object="paml_rst"),
##           function(object, ...) {
##               if (length(object@tip_seq) == 0) {
##                   warning("tip sequence not available...\n")
##               } else {
##                   object@tip_seq
##               }
##           })





## ##' get substitution information
## ##'
## ##'
## ##' @rdname get.subs-methods
## ##' @exportMethod get.subs
## setMethod("get.subs", signature(object = "paml_rst"),
##           function(object, type, ...) {
##               get.subs_paml_rst(object, type)
##           }
##           )



