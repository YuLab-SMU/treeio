context("as.phylo")

## library(ggtree)

## in tree 1.3.4 and ggtree 1.11.4, converting tree to data.frame is deprecated.
## new treeio and ggtree supports tbl_tree object

## test_that("as.phylo for tbl_tree and ggtree", {
##     p <- ggtree(rtree(30))
##     x <- as.phylo(p) ## as.phylo.ggtree -> as.phylo.data.frame
##     msg <- capture.output(ape::checkValidPhylo(x))
##     expect_false(any(grepl("FATAL", msg)))
## })

test_that("as.phylo for tree igraph",{
  tr <- rtree(10)
  g <- ape::as.igraph.phylo(tr)
  tr2 <- as.phylo(g)
  expect_true(is(tr2, 'phylo'))
  expect_true(all(tr$tip.label %in% tr2$tip.label))
  expect_equal(tr$Nnode, tr2$Nnode)
})

test_that("as.phylo uses the 'label' and 'branch.length' columns by default", {
    ## the node numbers were used as labels and the branch lengths were
    ## dropped when the tbl_tree class was lost (e.g. after rbind), #120 #134
    set.seed(2026)
    tr <- rtree(5)
    tr$node.label <- paste0("node", seq_len(Nnode(tr)))
    d <- as.data.frame(as_tibble(tr))

    x <- as.phylo(d)
    expect_equal(x$tip.label, tr$tip.label)
    expect_equal(x$node.label, tr$node.label)
    i <- match(paste(x$edge[, 1], x$edge[, 2]), paste(tr$edge[, 1], tr$edge[, 2]))
    expect_equal(x$edge.length, tr$edge.length[i])
})

test_that("as.phylo still falls back to the 2nd column for a plain edge list", {
    d <- data.frame(parent = c(6, 6, 7, 7, 7),
                    node = c(1, 2, 3, 4, 6))
    x <- as.phylo(d)
    expect_equal(x$tip.label, as.character(1:4))
})

test_that("as.phylo for tree igraph with weights",{
  set.seed(123)
  g <- igraph::sample_gnp(18, .3) %>%
        igraph::mst() %>%
        igraph::set_edge_attr(name='weight', value=abs(rnorm(length(igraph::E(.)))))
  tr <- as.phylo(g, branch.length = weight)
  expect_true(is(tr, 'phylo'))

  }
)
