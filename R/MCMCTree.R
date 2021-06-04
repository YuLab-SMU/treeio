##' read MCMCTree output Tree
##'
##' @title read.mcmctree
##' @param file the output tree file of MCMCTree
##' @return treedata object
##' @export
##' @examples
##' file <- system.file("extdata/MCMCTree", "mcmctree_output.tree", package="treeio")
##' tr <- read.mcmctree(file)
##' tr
read.mcmctree <- function(file){
    text <- readLines(file)
    ind <- grep("^.*tree.*=.*", text, ignore.case=TRUE)
    text[ind] <- gsub("^.*TREE", "TREE", text[ind], ignore.case=TRUE)
    text <- paste(text, collapse="\n")
    newfile <- tempfile()
    writeLines(text, newfile)
    obj <- read.beast(file=newfile)
    if(inherits(obj, "treedata")){
        obj@file <- filename(file)
    } else{
        for (i in seq_len(length(obj))){
            obj[[i]]@file <- filename(file)
        }
    }
    return(obj)
}
