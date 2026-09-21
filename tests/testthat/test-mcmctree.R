context("read.mcmctree")

library(treeio)

mcmctree <- system.file("extdata/MCMCTree", "mcmctree_output.tree",
                        package="treeio")

test_that("read.mcmctree imports the tree with its credibility interval", {
    tr <- read.mcmctree(mcmctree)

    expect_s4_class(tr, "treedata")
    expect_equal(Ntip(tr@phylo), 103)

    ## the node age is not reported by MCMCTree, it is added by treeio
    expect_true("reltime" %in% names(tr@data))

    ## the 95% CI of the node age, it used to be stored in a column named
    ## '0.95' which said neither what it is nor what it belongs to, #13
    expect_true("reltime_0.95_CI" %in% names(tr@data))

    ci <- tr@data[["reltime_0.95_CI"]]
    i <- which(vapply(ci, length, integer(1)) == 2)[1]
    expect_false(is.na(tr@data$reltime[i]))
    expect_length(ci[[i]], 2)
    expect_true(all(ci[[i]] > 0))
})

test_that("the credibility interval can be plotted with geom_range", {
    skip_if_not_installed("ggtree")

    tr <- read.mcmctree(mcmctree)
    p <- ggtree::ggtree(tr) +
        ggtree::geom_range("reltime_0.95_CI", center = "reltime")

    expect_true(inherits(p, "ggplot"))
})
