context("r8s")

file <- system.file("extdata/r8s", "H3_r8s_output.log", package="treeio")
r8s <- read.r8s(file)

test_that("parsing r8s log file", {
    expect_true(all(c("TREE", "RATO", "PHYLO") %in% names(r8s)))
    expect_true(is(r8s, "multiPhylo"))
})
