context("related_nodes")

library(treeio)


set.seed(42)
# sample bifurcating tree
bi_tree <- rtree(10)
bi_tree$tip.label <- paste0("t", 1:10)
# sample non-bifurcating tree
multi_tree <- ape::di2multi(bi_tree, tol=0.5)
# bifurcating tree with node names
named_bi_tree <- bi_tree
named_bi_tree$node.label <- paste0("n", 11:19)
# non-bifurcating tree with node names
named_multi_tree <- multi_tree
named_multi_tree$node.label <- paste0("n", 11:16)


test_that("child works for bifurcating trees", {
  # a leaf has no children
  expect_equal(child(bi_tree, 1), integer(0))
  # can find node children
  expect_equal(sort(child(bi_tree, 19)), 7:8)
  # can find root children
  expect_equal(sort(child(bi_tree, 11)), c(10,12))
})

test_that("child works for non-bifurcating trees", {
  # a leaf has no children
  expect_equal(child(multi_tree, 1), integer(0))
  # can find node children
  expect_equal(sort(child(multi_tree, 12)), c(3,9,13,14))
  # can find root children
  expect_equal(sort(child(multi_tree, 11)), c(10,12))
})

test_that("offspring works on bifurcating trees", {
  expect_equal(sort(offspring(bi_tree, 11)), (1:19)[-11])
  expect_equal(sort(offspring(bi_tree, 17)), c(4:6, 18))
  expect_error(offspring(bi_tree, 1), "input node is a tip...")
})

test_that("offspring works on non-bifurcating trees", {
  expect_equal(sort(offspring(multi_tree, 11)), (1:16)[-11])
  expect_equal(sort(offspring(multi_tree, 14)), c(4:8, 15:16))
  expect_error(offspring(multi_tree, 1), "input node is a tip...")
})

test_that("parent works for bifurcating trees", {
  expect_equal(treeio:::parent.phylo(bi_tree, 11), 0)
  expect_equal(parent(bi_tree, 1), 15)
  expect_equal(parent(bi_tree, 17), 16)
  expect_error(parent(bi_tree, 20))
})

test_that("parent works for non-bifurcating trees", {
  expect_equal(parent(multi_tree, 11), 0)
  expect_equal(parent(multi_tree, 8), 16)
  expect_equal(parent(multi_tree, 14), 12)
  expect_error(parent(multi_tree, 20))
})


test_that("ancestor works for bifurcating trees", {
  expect_equal(treeio:::ancestor.phylo(bi_tree, 11), NA)
  expect_equal(sort(ancestor(bi_tree, 1)), 11:15)
  expect_equal(sort(ancestor(bi_tree, 17)), c(11:13, 16))
  expect_error(ancestor(multi_tree, 20))
})

test_that("ancestor works for non-bifurcating trees", {
    expect_equal(ancestor(multi_tree, 11), NA)
    expect_equal(sort(ancestor(multi_tree, 8)), c(11,12,14,16))
    expect_equal(sort(ancestor(multi_tree, 14)), 11:12)
    expect_error(ancestor(multi_tree, 20))
})

test_that("rootnode", {
    expect_equal(treeio:::rootnode.phylo(bi_tree), 11)
    expect_equal(rootnode(multi_tree), 11)
})


new_edge <- c(11, 15)
bi_tree$edge <- rbind(bi_tree$edge, new_edge)
multi_tree$edge <- rbind(multi_tree$edge, new_edge)
test_that("throw error if multiple parent exists", {
    expect_error(treeio:::parent.phylo(bi_tree, 15), "multiple parent found...")
    expect_error(parent(multi_tree, 15), "multiple parent found...")
})


new_edge <- c(20, 15)
bi_tree$edge <- rbind(bi_tree$edge, new_edge)
multi_tree$edge <- rbind(multi_tree$edge, new_edge)
test_that("throw error if multiple roots found", {
    expect_error(rootnode(bi_tree), "multiple roots found...")
    expect_error(rootnode(multi_tree), "multiple roots found...")
})
