context("root")

library(treeio)

## rooting is dispatched to tidytree::root.treedata, which rebuilds the phylo
## from a tibble; the tip/node labels and the branch lengths used to be lost
## in the process, #120 #134

test_that("root keeps the tip and node labels", {
    data(bird.orders, package="ape")
    bird.orders$node.label <- paste0("node", seq_len(Nnode(bird.orders)))
    tr <- as.treedata(bird.orders)

    x <- root(tr, outgroup = "Struthioniformes", edgelabel = TRUE)

    expect_equal(sort(x@phylo$tip.label), sort(tr@phylo$tip.label))
    expect_true(all(grepl("^node", x@phylo$node.label[nzchar(x@phylo$node.label)])))
})

test_that("root keeps the branch lengths", {
    txt <- "((A:0.11,B:0.22)95/98:0.33,C:0.44,D:0.55);"
    tr <- read.iqtree(textConnection(txt))

    x <- root(tr, outgroup = "A", resolve.root = TRUE)

    expect_equal(Ntip(x@phylo), Ntip(tr@phylo))
    expect_equal(x@phylo$tip.label, tr@phylo$tip.label)
    expect_false(is.null(x@phylo$edge.length))
    expect_equal(sum(x@phylo$edge.length), sum(tr@phylo$edge.length))
})
