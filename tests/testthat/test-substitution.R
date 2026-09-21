context("substitution")

library(treeio)

test_that("get.subs_ reports the nodes it cannot find a sequence for", {
    ## the sequences are named after the tip and node labels of the tree;
    ## when they do not match, the substitution used to be reported as
    ## 'seqA should have equal length to seqB' without saying why, #91
    set.seed(2026)
    tr <- rtree(5)
    tr$node.label <- paste0("node", seq_len(Nnode(tr)))

    seq <- rep("ATGCGTGCATTCGATCGGCATGCATTAGCATCAGTCAG", Ntip(tr) + Nnode(tr))
    names(seq) <- c(tr$tip.label, tr$node.label)

    td <- as.treedata(tr)
    td@seq_type <- "NT"
    td@tip_seq <- string2DNAbin(seq[tr$tip.label])
    td@anc_seq <- string2DNAbin(seq[tr$node.label])

    ## the node labels are dropped, so getNodeName() returns the node numbers
    ## and none of the ancestral sequences can be found (the root has no
    ## parent and is not looked up)
    td@phylo$node.label <- NULL

    expect_error(get.subs_(td), "cannot find the sequence of 3 node")
    expect_error(get.subs_(td), "7, 8, 9")
})

test_that("getSubsLabel reports which sequences do not match", {
    seqs <- as.list(string2DNAbin(c(A = "ATGCGTGCATT", B = "ATGCGTGCATTG")))

    expect_error(getSubsLabel(seqs, "A", "B", FALSE, TRUE),
                 "seqA \\(A\\) has 11 sites, while seqB \\(B\\) has 12")
})
