context("phyloxml input")

library(treeio)
xmlfile1 <- system.file("extdata/phyloxml", "test_x2.xml", package="treeio")
xmlfile2 <- system.file("extdata/phyloxml", "phyloxml_examples.xml", package="treeio")
tx1 <- read.phyloxml(xmlfile1)
tx2 <- read.phyloxml(xmlfile2)

test_that("read.phyloxml should work for phyloxml",{
    expect_true(is(tx1, "treedata"))
    expect_true(is(tx2, "treedataList"))
    expect_equal(length(tx2), 13)
})

## the branch lengths were dropped because as.phylo() was called with
## 'length' instead of 'branch.length', #124
test_that("read.phyloxml keeps the branch lengths", {
    expect_false(is.null(tx1@phylo$edge.length))
    expect_equal(round(tx1@phylo$edge.length[1], 5), 0.21214)
    expect_false(is.null(tx2[[1]]@phylo$edge.length))
    expect_equal(tx2[[1]]@phylo$edge.length[1], 0.102)
})

dat <- list(A=list(a=12, b=c(B=1, "t")))
res1 <- extract_another(dat)
res2 <- list(A=c(a=12,B=1,b="t"))

test_that("checking extract_another",{
    expect_equal(res1, res2)
})

## a caterpillar tree of n tips, it is nested n levels deep; it is built here
## without a recursion, the tree is what is being tested
ladder <- function(n){
    x <- '<clade><name>t1</name><branch_length>0.1</branch_length></clade>'
    for (i in seq_len(n-1)){
        x <- paste0('<clade>', x,
                    '<clade><name>t', i+1L, '</name>',
                    '<branch_length>0.1</branch_length></clade>',
                    '<branch_length>0.1</branch_length></clade>')
    }
    paste0('<phyloxml><phylogeny rooted="true"><name>ladder</name>',
           x, '</phylogeny></phyloxml>')
}

## libxml2 refuses a document nested deeper than 256 elements by default and
## the clade tree used to be walked by a recursion that was quadratic
test_that("read.phyloxml supports a tree deeper than 256 levels", {
    xmlfile <- tempfile(fileext=".xml")
    writeLines(ladder(300), xmlfile)
    tx <- read.phyloxml(xmlfile)
    expect_equal(length(tx@phylo$tip.label), 300)
    expect_equal(tx@phylo$Nnode, 299)
    expect_equal(tx@phylo$tip.label[1], "t1")
    expect_equal(tx@phylo$tip.label[300], "t300")
    expect_false(is.null(tx@phylo$edge.length))
})

test_that("read.phyloxml reports a file that is not phyloxml", {
    xmlfile <- tempfile(fileext=".xml")
    writeLines("<phylogeny><clade><name>t1</name></clade></phylogeny>", xmlfile)
    expect_error(read.phyloxml(xmlfile), "not phyloxml format")
})


