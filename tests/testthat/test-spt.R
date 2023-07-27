context("shortest path tree")

test_that("spt for igraph",{
    set.seed(123)
    g <- igraph::sample_gnp(100, 3/100)
    tr <- spt(g, 6, igraph::V(g))
    expect_true(inherits(tr, 'phylo'))
})

