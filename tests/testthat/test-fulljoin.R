context("fulljoin")


rstfile <- system.file("extdata/PAML_Baseml", "rst", package="treeio")
tree <- read.paml_rst(rstfile)
x <- tibble(label = as.phylo(tree)$tip.label, trait = rnorm(Ntip(tree)))
y <- full_join(tree, x, by="label")

test_that("linking external data to treedata", {
    expect_true(is(y, "treedata"))
    expect_true("trait" %in% get.fields(y))
})
