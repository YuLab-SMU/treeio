context("Tree subset")

library(ape)


set.seed(42)
# sample bifurcating tree
bi_tree <- ape::rtree(10)
bi_tree$tip.label <- paste0("t", 1:10)
# sample non-bifurcating tree
multi_tree <- ape::di2multi(bi_tree, tol=0.5)
# bifurcating tree with node names
named_bi_tree <- bi_tree
named_bi_tree$node.label <- paste0("n", 11:19)
# non-bifurcating tree with node names
named_multi_tree <- multi_tree
named_multi_tree$node.label <- paste0("n", 11:16)


test_that("bi_tree and named_bi_tree return expected subsets", {
  bi_subset <- tree_subset(bi_tree, "t5", 2)

  bi_subset_lengths <- bi_subset %>%
    as_data_frame() %>%
    dplyr::filter(!is.na(label)) %>%
    dplyr::left_join(as_data_frame(bi_tree), by = "label")

  expect_equal(bi_subset$tip.label, paste0("t", 4:6))
  expect_equal(bi_subset_lengths$branch.length.x, bi_subset_lengths$branch.length.y)

  named_bi_subset <- tree_subset(named_bi_tree, "t5", 3)

  named_subset_lengths <- named_bi_subset %>%
    as_data_frame() %>%
    dplyr::filter(!is.na(branch.length)) %>%
    dplyr::left_join(as_data_frame(named_bi_tree), by = "label")

  expect_equal(named_bi_subset$tip.label, paste0("t", 4:8))
  expect_equal(named_subset_lengths$branch.length.x, named_subset_lengths$branch.length.y)


  # testing that "subsetting" to the number of levels_back to the root of the tree
  # returns the full tree - just have to remove focus and grouping
  expect_equal(tree_subset(bi_tree, "t5", 6) %>%
                 as_data_frame() %>%
                 dplyr::select(-c(focus, group)),
               as_data_frame(bi_tree))

})


test_that("multi_tree and named_multi_tree return expected subtrees", {
  multi_subset <- tree_subset(multi_tree, "t8", 2)

  multi_subset_lengths <- multi_subset %>%
    as_data_frame() %>%
    dplyr::filter(!is.na(label)) %>%
    dplyr::left_join(as_data_frame(multi_tree), by = "label")

  expect_equal(multi_subset$tip.label, paste0("t", 4:8))
  expect_equal(multi_subset_lengths$branch.length.x, multi_subset_lengths$branch.length.y)

  named_multi_subset <- tree_subset(named_multi_tree, "t8", 3)

  named_subset_length <- named_multi_subset %>%
    as_data_frame() %>%
    dplyr::filter(!is.na(branch.length)) %>%
    dplyr::left_join(as_data_frame(named_multi_tree), by = "label")

  expect_equal(named_multi_subset$tip.label, paste0("t", 1:9))
  expect_equal(named_subset_length$branch.length.x, named_subset_length$branch.length.y)

  expect_equal(tree_subset(multi_tree, "t8", 4) %>%
                 as_data_frame() %>%
                 dplyr::select(-c(focus, group)),
               as_data_frame(multi_tree))
})
