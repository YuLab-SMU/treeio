##' read mlc file of codeml output
##'
##'
##' @title read.codeml_mlc
##' @param mlcfile mlc file
##' @return A \code{codeml_mlc} object
##' @importFrom dplyr select
##' @export
##' @author Guangchuang Yu
##' @examples
##' mlcfile <- system.file("extdata/PAML_Codeml", "mlc", package="treeio")
##' read.codeml_mlc(mlcfile)
read.codeml_mlc <- function(mlcfile) {
    ## tip_seq <- read.tip_seq_mlc(mlcfile)
    dNdS <- read.dnds_mlc(mlcfile)
    if (is.null(dNdS)) {
        message("no dNdS information found...")
        dNdS <- matrix(NA)
        fields <- ""
    } else {
        fields <- colnames(dNdS)[-c(1,2)]
    }

    res <- new("treedata",
        treetext = read.treetext_paml_mlc(mlcfile),
        phylo    = read.phylo_paml_mlc(mlcfile),
        file     = filename(mlcfile))
    if (!is.null(dNdS)) {
        res@data <- as_tibble(dNdS) %>% select(-"parent")
    }
    return(res)
}


