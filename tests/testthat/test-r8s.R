context("r8s")


r8s <- read.r8s(system.file("extdata/r8s", "H3_r8s_output.log", package="treeio"))

test_that("parsing r8s log file", {
    expect_true(all(c("TREE", "RATO", "PHYLO") %in% names(r8s)))
    expect_true(is(r8s, "multiPhylo"))
})
