context("inner_join")

tr <- ape::rtree(8)
x <- data.frame(id = sample(tr$tip.label, 4), trait = rnorm(4))
y <- inner_join(tr, x, by=c("label"="id"))

test_that("linking external data to treedata", {
    expect_true(is(y, "treedata"))
    expect_equal(nrow(x), Ntip(y))
    expect_true("trait" %in% get.fields(y))
})
