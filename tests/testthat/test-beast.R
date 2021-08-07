context("beast input and output")

library(treeio)

file <- system.file("extdata/BEAST", "beast_mcc.tree", package="treeio")
beast <- read.beast(file)

test_that(".write.tree3 should works for ordinary phylo object", {
    expect_equal(treeio:::.write.tree3(as.phylo(beast)),
                 ape::write.tree(as.phylo(beast)))
})


beast_nwk <- treeio:::write_beast_newick(beast)
beast_file <- tempfile()
treeio:::write_beast_newick(beast, beast_file)
test_that("write_beast_newick output a newick string with annotation", {
    expect_true(grepl('HPD', beast_nwk))
    expect_true(grepl('\\{', beast_nwk))
    expect_true(grepl('^\\(', beast_nwk))
    expect_true(grepl(';$', beast_nwk))
    expect_gt(file.info(beast_file)$size, 0)
})


beast_file <- tempfile()
write.beast(beast, beast_file)

test_that("write.beast output a valid beast file", {
    expect_true(is(read.beast(beast_file), "treedata"))
})


xx <- "(a[&rate=1]:2,(b[&rate=1.1]:1,c[&rate=0.9]:1)[&rate=1]:1);\n(a[&rate=1]:2,(b[&rate=1.1]:1,c[&rate=0.9]:1)[&rate=1]:1);"

tree1 <- structure(list(edge=matrix(c(4L, 4L, 5L, 5L, 1L, 5L, 2L, 3L), ncol=2),
                        edge.length=c(2, 1, 1, 1),
                        Nnode=2L,
                        tip.label=c("a", "b", "c")),
                        class="phylo",
                        order="cladewise"
                        )
trees <- read.beast.newick(textConnection(xx))

test_that("read.beast.newick should work for multiple trees",{
    expect_true(inherits(trees, "treedataList"))
    expect_equal(trees[[1]]@phylo, tree1)
})
