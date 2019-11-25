context("jplace input")

library(treeio)
file1 <- system.file("extdata", "ref35extend.jplace", package="treeio")
file2 <- system.file("extdata", "upwelling.jplace", package="treeio")
jp1 <- read.jplace(file1)
jp2 <- read.jplace(file2)

test_that("read.jplace should work for old version jplace",{
    expect_true(is(jp1, "jplace"))
    expect_true(is(jp2, "jplace"))
})

library(jsonlite)
js1 <- fromJSON(file1)
js2 <- fromJSON(file2)
numplace <- function(p_field, n_field){
    p_nrow <- ifelse(is.null(nrow(p_field)), 1, nrow(p_field))
    n_nrow <- ifelse(is.matrix(n_field), nrow(n_field), length(n_field))
    return (p_nrow * n_nrow)
}

nrowplace1 <- sum(unlist(mapply(numplace, js1$placements$p, js1$placements$n, SIMPLIFY=FALSE)))
js2_names <- mapply(treeio:::mergenm,js2$placements$n, js2$placements$nm, SIMPLIFY=FALSE)
nrowplace2 <- sum(unlist(mapply(numplace, js2$placements$p, js2_names, SIMPLIFY=FALSE)))

test_that("check the nrow of placements.",{
    jt1 <- treeio:::jplace_treetext_to_phylo(js1$tree)
    jt2 <- treeio:::jplace_treetext_to_phylo(js2$tree)
    expect_true(is(jt1, "phylo"))
    expect_true(is(jt2, "phylo"))
    jplacement1 <- treeio:::extract.placement(js1, jt1)
    jplacement2 <- treeio:::extract.placement(js2, jt2)
    expect_equal(nrow(jplacement1), nrowplace1)
    expect_equal(nrow(jplacement2), nrowplace2)
})
