context("beast input and output")

library(treeio)

file <- system.file("extdata/BEAST", "beast_mcc.tree", package="treeio")
beast <- read.beast(file)

get_beast_annotation_text <- function(treedata) {
    anno <- tidytree::get_tree_data(treedata)
    anno$node <- as.integer(anno$node)
    anno <- anno[!colnames(anno) %in% c("subs", "AA_subs")]

    cn <- colnames(anno)
    col_type <- vapply(anno, class, character(1))
    yy <- lapply(which(!cn %in% c("node", "label")), function(i) {
        v <- cn[i]
        if (col_type[i] == "list") {
            rr <- paste0(
                v, "={",
                vapply(anno[[v]], function(x) paste(x, collapse = ","), character(1)),
                "}"
            )
        } else {
            rr <- paste0(v, "=", anno[[v]])
        }
        rr[is.na(anno[[v]])] <- NA
        rr
    }) %>% do.call("cbind", .)

    anno_text <- vapply(seq_len(nrow(yy)), function(i) {
        rr <- yy[i,]
        rr <- rr[!is.na(rr)]
        if (length(rr) == 0) {
            return("")
        }
        paste0("[&", paste(rr, collapse = ","), "]")
    }, character(1))

    setNames(anno_text, as.character(anno$node))
}

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

test_that("write_beast_newick preserves root annotation", {
    root <- treeio:::getRoot(as.phylo(beast)$edge)
    expected_root_anno <- get_beast_annotation_text(beast)[as.character(root)]

    expect_true(nzchar(expected_root_anno))
    expect_true(endsWith(beast_nwk, paste0(expected_root_anno, ";")))
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


## BEAST2 appends the partition name to the parameter name (e.g. blockcount.t:hi),
## the extra ':' used to break the parsing of the annotation, #136
beast2 <- read.beast(system.file("extdata/BEAST", "beast2_mcc.tree", package="treeio"))

test_that("read.beast strips the BEAST2 partition name from the parameter name", {
    expect_s4_class(beast2, "treedata")
    expect_equal(ape::Ntip(beast2@phylo), 4)
    expect_equal(beast2@phylo$tip.label, c("a", "b", "c", "d"))

    cn <- colnames(beast2@data)
    expect_true(all(c("blockcount.t", "blockstart.t", "blockend.t") %in% cn))

    ## the values are numbers, not the parameter name itself
    expect_true(is.numeric(beast2@data$blockcount.t))
    d <- as.data.frame(beast2@data)[match(as.character(1:4), beast2@data$node), ]
    expect_equal(d$blockcount.t, c(0, -1, 2, 1))
    expect_equal(d$blockstart.t, c(0.2, 0.5, 0.1, 0.5))
    expect_equal(d$blockend.t, c(0.2, 0.5, 0.5, 0.75))
})

## ':' inside an annotation value must not be treated as a partition name,
## otherwise the following parameter is swallowed
beast2_colon <- read.beast.newick(textConnection(
    '((a[&mutation="A:T",rate=1.5]:0.1,b[&rate=2]:0.2)[&rate=1]:0.3,c[&rate=0.5]:0.4);'
))

test_that("read.beast does not strip ':' that is inside an annotation value", {
    d <- as.data.frame(beast2_colon@data)
    expect_true("mutation" %in% colnames(d))
    ## the value is truncated at ':' (existing limitation of splitting by ':'),
    ## but it must not pick up the value of the next parameter
    expect_equal(d$mutation[1], "A")
})


## the keys of the TRANSLATE table are not necessarily 1:Ntip (e.g. MEGA
## output); ape::read.nexus() numbers the tips by the keys and returns a
## broken tree, and the node data was shifted accordingly, #132
nonconsecutive <- read.beast(
    system.file("extdata/MEGA7", "nonconsecutive_translate.nex", package="treeio")
)

test_that("read.beast works with a non-consecutive translate table", {
    expect_s4_class(nonconsecutive, "treedata")
    expect_equal(ape::Ntip(nonconsecutive@phylo), 4)
    expect_equal(nonconsecutive@phylo$tip.label, c("A", "B", "C", "D"))

    ## the tips are numbered 1:Ntip, not by the keys of the translate table
    n <- Ntip(nonconsecutive@phylo) + Nnode(nonconsecutive@phylo)
    expect_setequal(as.vector(nonconsecutive@phylo$edge), seq_len(n))

    d <- as.data.frame(nonconsecutive@data)
    d <- d[match(as.character(1:4), d$node), ]
    expect_equal(d$rate, c(0.1, 0.3, 0.6, 0.8))
})

test_that("read.mega works with a non-consecutive translate table", {
    mega <- read.mega(
        system.file("extdata/MEGA7", "nonconsecutive_translate.nex", package="treeio")
    )
    expect_equal(mega@phylo, nonconsecutive@phylo)
    expect_equal(mega@data, nonconsecutive@data)
})


## a nexus file written by e.g. FigTree quotes the taxon names, the quotes
## used to be kept and the tip labels did not match the sequence names, #47
quoted <- read.beast(
    system.file("extdata/BEAST", "beast_mcc_quoted.tree", package="treeio")
)

test_that("read.beast removes the quotes around the taxon names", {
    expect_equal(quoted@phylo$tip.label, beast@phylo$tip.label)
    expect_false(any(grepl("['\"]", quoted@phylo$tip.label)))
})


## a taxon name may contain a space, e.g. 'MF574563.1 _COL_2015'; it is single
## quoted and ape::read.nexus() used to split it, which returned a tree with
## the wrong number of tips and crashed the session when plotted, #71
spaced <- read.beast(
    system.file("extdata/BEAST", "beast_mcc_spaced.tree", package="treeio")
)

test_that("read.beast keeps the space inside a quoted taxon name", {
    expect_equal(ape::Ntip(spaced@phylo), 4)
    expect_equal(spaced@phylo$tip.label,
                 c("A_1995", "B_1996", "MF574563.1 _COL_2015", "D_1987"))

    d <- as.data.frame(spaced@data)
    d <- d[match(as.character(1:4), d$node), ]
    expect_equal(d$length, c(1, 1, 2, 2))
})


## the dating of IQ-TREE (LSD2) writes an unrooted tree with 'UTREE', which
## ape::read.nexus() does not know, and only some of the nodes are annotated,
## #111
lsd2 <- read.beast(system.file("extdata/LSD2", "timetree.nex", package="treeio"))

test_that("read.beast supports the UTREE keyword of an LSD2 timetree", {
    expect_s4_class(lsd2, "treedata")
    expect_equal(lsd2@phylo$tip.label, LETTERS[1:4])

    d <- as.data.frame(lsd2@data)
    d <- d[match(as.character(1:4), d$node), ]
    expect_equal(d$date, c(0, 0, NA, NA))
    expect_equal(d$height, c(0, 0, NA, NA))
})

test_that("write.beast does not annotate the nodes without data", {
    ## a node without data used to be annotated with 'NULL' and the node label
    ## was left undefined, which stopped the export, #111
    file <- tempfile()
    write.beast(lsd2, file = file)

    txt <- readLines(file)
    expect_false(any(grepl("NULL", txt, fixed = TRUE)))

    tr <- read.beast(file)
    expect_equal(tr@phylo$tip.label, lsd2@phylo$tip.label)
    expect_equal(as.data.frame(tr@data)$date, as.data.frame(lsd2@data)$date)
})
