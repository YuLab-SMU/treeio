context("merge_tree")

beast_file <- system.file("examples/MCC_FluA_H3.tree", package="ggtree")
rst_file <- system.file("examples/rst", package="ggtree")
mlc_file <- system.file("examples/mlc", package="ggtree")
beast_tree <- read.beast(beast_file)
codeml_tree <- read.codeml(rst_file, mlc_file)

merged_tree <- merge_tree(beast_tree, codeml_tree)


test_that('merging tree objects', {
    expect_true(is(merged_tree, "treedata"))
    expect_true(all(get.fields(codeml_tree) %in% get.fields(merged_tree)))
    expect_true(all(get.fields(beast_tree) %in% get.fields(merged_tree)))
})
