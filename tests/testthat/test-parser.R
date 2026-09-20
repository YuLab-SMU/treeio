context("parse tree")

phyfile <- system.file("extdata", "sample.phy", package="treeio")
ptree1 <- read.phylip(phyfile)
ptree2 <- read.phylip.tree(phyfile)



raxml_file <- system.file("extdata/RAxML",
                          "RAxML_bipartitionsBranchLabels.H3",
                          package="treeio")

raxml <- read.raxml(raxml_file)

nwk_file <- tempfile()
raxml2nwk(raxml_file, nwk_file)

test_that("parsing trees", {
    expect_true(is(raxml, "treedata"))
    expect_true(is(ptree1, "treedata"))
    expect_true(is(ptree2, "phylo"))
    expect_true('bootstrap' %in% names(raxml@data))
    expect_equal(length(ptree1@tip_seq), Ntip(ptree1))
    expect_true(all.equal(read.newick(nwk_file), read.tree(nwk_file)))
    expect_true('support' %in% names(read.newick(nwk_file, "support")@data))
})

## the tree can be passed as a string instead of a file, #122
test_that("read.raxml accepts a text argument", {
    txt <- "((a:0.1,b:0.2):0.3[90],c:0.4);"
    x <- read.raxml(text = txt)
    expect_s4_class(x, "treedata")
    expect_equal(Ntip(x@phylo), 3)
    expect_equal(x@phylo$tip.label, c("a", "b", "c"))
    expect_true(90 %in% x@data$bootstrap)

    y <- read.raxml(text = textConnection(txt))
    expect_equal(y@phylo, x@phylo)

    expect_error(read.raxml(), "either 'file' or 'text'")
})

## a file of bootstrap replicates contains several trees and no support
## value, #121
raxml_boot <- read.raxml(
    system.file("extdata/RAxML", "RAxML_bootstrap.output", package="treeio")
)

test_that("read.raxml works for a file of bootstrap replicates", {
    expect_true(inherits(raxml_boot, "treedataList"))
    expect_equal(length(raxml_boot), 3)
    expect_true(all(vapply(raxml_boot, is, logical(1), "treedata")))
    expect_equal(Ntip(raxml_boot[[1]]@phylo), 3)
    expect_equal(raxml_boot[[1]]@phylo$edge.length, c(0.3, 0.1, 0.2, 0.4))
    expect_true(all(is.na(raxml_boot[[1]]@data$bootstrap)))
})

## the tree lines are located by ')...;', an annotated tree (e.g. '#1' for
## the branch models) was not found and strsplit() then failed on NA, #34
test_that("the tree of a PAML output is found even when it is annotated", {
    x <- c("",
           "TREE #  1:  ((1, 2) #1, 3) #1;", "",
           "((1: 0.1, 2: 0.2) #1: 0.3, 3: 0.4) #1;", "",
           "((A: 0.1, B: 0.2) #1: 0.3, C: 0.4) #1;")
    expect_equal(treeio:::get_tree_index_paml(x), c(2, 4, 6))
})

test_that("a PAML output without a tree reports it instead of failing", {
    mlc <- tempfile()
    writeLines(c("   3   60", "", "A   ATG GAA GAC"), mlc)
    expect_error(read.codeml_mlc(mlc), "cannot find the tree")
})
