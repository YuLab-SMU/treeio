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

dat <- list(A=list(a=12, b=c(B=1, "t")))
res1 <- extract_another(dat)
res2 <- list(A=c(a=12,B=1,b="t"))

test_that("checking extract_another",{
    expect_equal(res1, res2)
})


