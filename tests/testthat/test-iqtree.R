context("read.iqtree")

library(treeio)

iqtree <- system.file("extdata/IQTREE", package="treeio")

test_that("read.iqtree parses the support values of a tree file", {
    file <- file.path(iqtree, "ML.treefile")
    tr <- read.iqtree(file)

    expect_equal(Ntip(tr@phylo), 5)
    expect_equal(sort(tr@phylo$tip.label), LETTERS[1:5])
    expect_equal(as.data.frame(tr@data)$SH_aLRT, c(NA, 95, 100, 72))
    expect_equal(as.data.frame(tr@data)$UFboot, c(NA, 98, 100, 80))
})

test_that("read.iqtree rejects a file without a newick tree", {
    ## the report file of IQ-TREE is not a tree file, it used to be parsed
    ## into a meaningless tree or even crashed the R session, #98
    file <- file.path(iqtree, "ML.iqtree")

    expect_error(read.iqtree(file), "cannot find a Newick tree")
})

test_that("read.iqtree says it cannot split a single support value", {
    ## the branch support may be assessed with a single method (e.g. the
    ## standard bootstrap) and then there is only one value per node, #114
    txt <- "((A:0.11,B:0.22)98:0.33,C:0.44);"

    expect_message(tr <- read.iqtree(textConnection(txt)), "do not contain")
    expect_equal(as.data.frame(tr@data)$SH_aLRT, c(NA, 98))
    expect_equal(as.data.frame(tr@data)$UFboot, as.data.frame(tr@data)$SH_aLRT)
})

test_that("read.iqtree supports a tree without any branch support", {
    txt <- "((A:0.11,B:0.22):0.33,C:0.44);"
    tr <- read.iqtree(textConnection(txt))

    expect_equal(as.data.frame(tr@data)$SH_aLRT, c(NA_real_, NA_real_))
    expect_equal(as.data.frame(tr@data)$UFboot, c(NA_real_, NA_real_))
})

test_that("read.iqtree accepts a newick string wrapped over several lines", {
    txt <- c("((A:0.11,B:0.22)95/98:0.33,",
             "C:0.44);")
    tr <- read.iqtree(textConnection(txt))

    expect_equal(Ntip(tr@phylo), 3)
    expect_equal(as.data.frame(tr@data)$UFboot, c(NA, 98))
})
