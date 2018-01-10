context("accessor")

library(treeio)

jp <- system.file("extdata", "sample.jplace", package="treeio")
x <- read.jplace(jp)
pp <- get.placements(x)

test_that("access placements slot for jplace object", {
    expect_true(is(x, "jplace"))
    expect_equal(nrow(pp), 3)
    expect_equal(ncol(pp), 7)
    expect_true('likelihood' %in% names(pp))
})

file <- system.file("extdata/BEAST", "beast_mcc.tree", package="treeio")
beast <- read.beast(file)
tree <- read.tree(text = get.treetext(beast))

test_that("access treetext slot for treedata object", {
    expect_true(ape::all.equal.phylo(beast@phylo, tree, use.tip.label=FALSE))
})

test_that("is.rooted method for treedata object", {
    expect_equal(is.rooted(beast), ape::is.rooted(as.phylo(beast)))
})
