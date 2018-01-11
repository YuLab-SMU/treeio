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
