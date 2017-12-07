##' parse output from r8s
##'
##'
##' @title read.r8s
##' @param file r8s output log file
##' @return multiPhylo object
##' @export
##' @examples
##' read.r8s(system.file("extdata/r8s", "H3_r8s_output.log", package="treeio"))
##' @author Guangchuang Yu
read.r8s <- function(file) {
    r8s <- readLines(file)
    label_idx <- grep("\\[\\w+\\sDESCRIPTION\\sof\\stree\\s.*\\]", r8s)
    tree_idx <- grep("^tree\\s.*\\s=\\s", r8s)
    if (length(label_idx) != length(tree_idx)) {
        stop("fail to parse the file...")
    }

    tree_text <- gsub("^tree\\s.*\\s=\\s", "", r8s[tree_idx])
    trees <- read.tree(text=tree_text)

    label <- gsub("^\\[(\\w+)\\s.*", "\\1", r8s[label_idx])
    names(trees) <- label

    ## new("r8s",
    ##     file = filename(file),
    ##     fields = label,
    ##     phylo = trees)

    return(trees)
}




