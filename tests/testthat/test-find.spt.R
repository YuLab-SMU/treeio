context("find.spt")

test_that("find.spt for igraph",{
    set.seed(123)
    g <- igraph::sample_gnp(100, 3/100)
    tr <- find.spt(g, 6, igraph::V(g))
    expect_true(inherits(tr, 'phylo'))
})

