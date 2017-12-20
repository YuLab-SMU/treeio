##' read baseml output
##'
##'
##' @title read.codeml
##' @param rstfile rst file
##' @param mlcfile mlc file
##' @param tree one of 'mlc' or 'rst'
##' @param type one of 'Marginal' or 'Joint'
##' @return A \code{treedata} object
##' @export
##' @author Guangchuang Yu
##' @examples
##' rstfile <- system.file("extdata/PAML_Codeml", "rst", package="treeio")
##' mlcfile <- system.file("extdata/PAML_Codeml", "mlc", package="treeio")
##' read.codeml(rstfile, mlcfile)
read.codeml <- function(rstfile, mlcfile, tree = "mlc", type = "Joint") {
    tree <- match.arg(tree, c("mlc", "rst"))

    rst <- read.paml_rst(rstfile, type=type)
    mlc <- read.codeml_mlc(mlcfile)

    res <- rst
    res@file <- c(res@file, mlc@file)
    if (tree == 'mlc') {
        res@phylo <- as.phylo(mlc)
        res@treetext <- mlc@treetext
    }

    if (nrow(res@data) == 0) {
        res@data <- mlc@data
    } else {
        res@data <- full_join(res@data, mlc@data, by = 'node')
    }

    if (nrow(res@extraInfo) == 0) {
        res@extraInfo <- mlc@extraInfo
    } else {
        res@extraInfo <- full_join(res@extraInfo, mlc@extraInfo, by = "node")
    }

    res@info <- c(res@info, mlc@info)
    return(res)
}



