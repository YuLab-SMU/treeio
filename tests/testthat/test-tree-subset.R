context("Tree subset")

library(ape)
library(tidytree)

# testing that tree_subset works on phylo objects
#==============================================================
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
                 dplyr::select(-group),
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
                 dplyr::select(-group),
               as_data_frame(multi_tree))
})


# testing that tree_subset works on treedata objects
#====================================================================

beast_file <- system.file("examples/MCC_FluA_H3.tree", package="ggtree")
rst_file <- system.file("examples/rst", package="ggtree")
mlc_file <- system.file("examples/mlc", package="ggtree")
beast_tree <- read.beast(beast_file)
codeml_tree <- read.codeml(rst_file, mlc_file)

merged_tree <- merge_tree(beast_tree, codeml_tree)
merged_tree


test_that("treedata returns expected results", {
  merged_subset <- tree_subset(merged_tree, "A/Swine/GX/2242/2011", 3)

  expected_tips <- c("A/Swine/GD_NS2892/2012", "A/Swine/GD_NS2701/2012",
                     "A/Swine/GX_NS1409/2012", "A/Swine/HK/3280/2012",
                     "A/Swine/GX/650/2012", "A/Swine/GX/508/2012",
                     "A/Swine/GX/2242/2011", "A/Swine/GD/2919/2012",
                     "A/Swine/HK_NS1651/2012")

  merged_subset_df <- merged_subset %>%
    as_data_frame() %>%
    dplyr::filter(!node %in% parent) %>%
    tidyr::gather(key = data, value = value_subset, -c(parent, node, branch.length,
                                                       label, group)) %>%
    dplyr::left_join(merged_tree %>%
                as_data_frame() %>%
                tidyr::gather(key = data, value = value_orig,
                              -c(parent, node, branch.length,
                                 label)),
              by = c("label", "data"))

  expect_true(all(merged_subset@phylo$tip.label %in% expected_tips))
  expect_true(all(expected_tips %in% merged_subset@phylo$tip.label))

  expect_identical(merged_subset_df$value_subset, merged_subset_df$value_orig)
  expect_identical(merged_subset_df$branch.length.x, merged_subset_df$branch.length.y)
})
