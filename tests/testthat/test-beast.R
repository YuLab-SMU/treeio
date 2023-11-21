context("beast input and output")

library(treeio)

file <- system.file("extdata/BEAST", "beast_mcc.tree", package="treeio")
beast <- read.beast(file)

test_that("read.beast works", {
    
    expect_s4_class(beast, 'treedata')
    # check some tree attributes
    expect_s3_class(beast@phylo, 'phylo')
    expect_equal(ape::Ntip(beast@phylo), 15)
    expect_equal(ape::Nnode(beast@phylo), 14)
    expect_true(ape::is.rooted(beast@phylo))
    expect_true("A_1995" %in% beast@phylo$tip.label)

    expect_s3_class(beast@data, 'data.frame')
    expect_equal(nrow(beast@data), 29)  # = Ntip + Nnode
    
    # check a branch at random: Node 18, parent of N_2010 and K_2013
    node <- subset(beast@data, node == 18)
    # N.b. some annotations are renamed from the attributes in the treefile,
    #   e.g. height_95%_HPD => height_0.95_HPD
    # and these are the values directly from the tree to make sure we get them
    # with the precision from the file
    expect_equal(node[['length_range']][[1]], c(15.724675549523745,36.72403651916805))
    expect_equal(node[['height_range']][[1]], c(5.941039739620447,15.10397485336565))
    expect_equal(node[['rate_range']][[1]], c(0.0018410123833317064,0.004221113122306114))
    expect_equal(node[['height_median']][[1]], 9.385096430786298)
    expect_equal(node[['height_0.95_HPD']][[1]], c(7.322099111116469,11.624410805545125))
    expect_equal(node[['height']][[1]], 9.470517085164637)
    expect_equal(node[['posterior']][[1]], 1.0)
    expect_equal(node[['rate']][[1]], 0.0029074382448820114)
    expect_equal(node[['rate_median']][[1]], 0.002896909674386843)
    expect_equal(node[['rate_0.95_HPD']][[1]], c(0.00237138786088183,0.0034784158116179195))
    expect_equal(node[['length']][[1]], 25.719898216070327)
    expect_equal(node[['length_median']][[1]], 25.579167352568767)
    expect_equal(node[['length_0.95_HPD']][[1]], c(22.048771671275617,30.115400624425263))
})


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


xx <- "(a:2L[&rate=1],(b:[&rate=1.1]1L,c[&rate=0.9]:1):-10e-6[&rate=1]);\n(a[&rate=1]:2,(b[&rate=1.1]:1,c[&rate=0.9]:1)[&rate=1]:1);"

tree1 <- structure(list(edge=matrix(c(4L, 4L, 5L, 5L, 1L, 5L, 2L, 3L), ncol=2),
                        edge.length=c(2, -1e-05, 1, 1),
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
