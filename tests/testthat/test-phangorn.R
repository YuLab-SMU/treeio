context("as.treedata for pml")

library(treeio)

test_that("as.treedata converts a pml object", {
    ## the method is registered in the NAMESPACE since 0d49fb6, #127
    skip_if_not_installed("phangorn")

    set.seed(2026)
    tr <- rtree(8)
    dna <- as.DNAbin(
        matrix(sample(c("a", "c", "g", "t"), 8 * 60, TRUE), 8, 60,
               dimnames = list(tr$tip.label, NULL))
    )
    ## the alignment of a real data set contains gaps, #91
    dna[1:3, 20:30] <- as.DNAbin("-")

    fit <- phangorn::pml(tr, phangorn::as.phyDat(dna), k = 4)
    x <- as.treedata(fit)

    expect_s4_class(x, "treedata")
    expect_equal(Ntip(x@phylo), 8)
    expect_equal(length(x@tip_seq), 8)
    expect_equal(length(x@anc_seq), Nnode(x@phylo))

    d <- as.data.frame(x@data)
    expect_true(all(c("node", "subs", "AA_subs") %in% names(d)))
})
