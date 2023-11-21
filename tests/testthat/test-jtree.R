context("jtree input and output")

rstfile <- system.file("extdata/PAML_Codeml", "rst", package="treeio")
mlcfile <- system.file("extdata/PAML_Codeml", "mlc", package="treeio")
ml <- read.codeml(rstfile, mlcfile)

jtree_file <- tempfile()
jtree_text <- write.jtree(ml, jtree_file)

js <- jsonlite::fromJSON(jtree_file)

tree <- read.jtree(jtree_file)

test_that("write.jtree output a json file", {
    expect_gt(file.info(jtree_file)$size, 0)
    expect_true(is(js, "list"))
    expect_true(all(c("tree", "data") %in% names(js)))
    expect_true(all(c("edge_num", "dN", "dS") %in% names(js$data)))
    expect_true(treeio:::is.tree(tree))
    expect_true(treeio:::has.slot(tree, "data"))
})
