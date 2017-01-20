context("as.phylo")

test_nhx_text <- readLines(system.file("extdata/NHX", "ADH.nhx", package="treeio"))

test_that("as.phylo for data.frame and ggtree", {
    p <- read.nhx( textConnection(test_nhx_text) )
    x <- as.phylo(p) ## as.phylo.ggtree -> as.phylo.data.frame
    msg <- capture.output(ape::checkValidPhylo(x))
    expect_false(any(grepl("FATAL", msg)))
})


