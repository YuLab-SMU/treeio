context("nextstrain json")

library(treeio)

file <- system.file("extdata/nextstrain.json", "minimal_v2.json", package="treeio")
tr <- read.nextstrain.json(file)

test_that("read.nextstrain.json works", {
    expect_s4_class(tr, "treedata")
    expect_s3_class(tr@phylo, "phylo")
    expect_false(is.null(tr@phylo$edge.length))
    expect_true("num_date" %in% colnames(tr@data))
})

## the divergence is the number of mutations, and a node with a character
## attribute used to turn its 'div' into a character, #126
div <- read.nextstrain.json(
    system.file("extdata/nextstrain.json", "divergence_v2.json", package="treeio")
)

test_that("read.nextstrain.json works for a divergence tree", {
    expect_s4_class(div, "treedata")
    expect_equal(sort(div@phylo$tip.label), c("A", "C", "D"))

    d <- as.data.frame(as_tibble(div))
    ## div is numeric for every node, the branch length is div - div(parent)
    expect_true(is.numeric(d$div))
    i <- match(c("A", "C", "D"), d$label)
    expect_equal(d$div[i], c(2, 5, 4))
    expect_equal(d$branch.length[i], c(2, 2, 1))

    ## the character attribute is kept as it is
    expect_equal(d$author[match("NODE_0000001", d$label)], "someone")
})
