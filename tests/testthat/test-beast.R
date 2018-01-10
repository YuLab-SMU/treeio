context("beast input and output")

library(treeio)

file <- system.file("extdata/BEAST", "beast_mcc.tree", package="treeio")
beast <- read.beast(file)

test_that(".write.tree3 should works for ordinary phylo object", {
    expect_equal(treeio:::.write.tree3(as.phylo(beast)),
                 ape::write.tree(as.phylo(beast)))
})


beast_nwk <- treeio:::write_beast_newick(beast)
beast_file <- tempfile()
treeio:::write_beast_newick(beast, beast_file)
test_that("write_beast_newick output a newick string with annotation", {
    expect_true(grepl('HPD', beast_nwk))
    expect_true(grepl('\\{', beast_nwk))
    expect_true(grepl('^\\(', beast_nwk))
    expect_true(grepl(';$', beast_nwk))
    expect_gt(file.info(beast_file)$size, 0)
})


beast_file <- tempfile()
write.beast(beast, beast_file)

test_that("write.beast output a valid beast file", {
    expect_true(is(read.beast(beast_file), "treedata"))
})

