context("phyloT")


nwk <- paste0("((Escherichia_coli,(Drosophila_melanogaster,",
              "((Homo_sapiens,Mus_musculus)Euarchontoglires,",
              "Gallus_gallus)Amniota)Bilateria)cellular_organisms);")

tree <- read.phyloT(file=textConnection(nwk))

test_that("capable to parse phyloT output", {
    expect_true(is(tree, "phylo"))
})
