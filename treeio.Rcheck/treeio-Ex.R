pkgname <- "treeio"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('treeio')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Nnode")
### * Nnode

flush(stderr()); flush(stdout())

### Name: Nnode
### Title: Nnode
### Aliases: Nnode

### ** Examples

Nnode(rtree(30))



cleanEx()
nameEx("Ntip")
### * Ntip

flush(stderr()); flush(stdout())

### Name: Ntip
### Title: Ntip
### Aliases: Ntip

### ** Examples

Ntip(rtree(30))



cleanEx()
nameEx("beast-mrbayes-parser")
### * beast-mrbayes-parser

flush(stderr()); flush(stdout())

### Name: read.beast
### Title: read.beast
### Aliases: read.beast read.mrbayes

### ** Examples

file <- system.file("extdata/BEAST", "beast_mcc.tree", package="treeio")
read.beast(file)



cleanEx()
nameEx("drop.tip-methods")
### * drop.tip-methods

flush(stderr()); flush(stdout())

### Name: drop.tip
### Title: drop.tip method
### Aliases: drop.tip drop.tip,treedata-method drop.tip,treedata
###   drop.tip,phylo-method drop.tip,phylo

### ** Examples

nhxfile <- system.file("extdata/NHX", "ADH.nhx", package="treeio")
nhx <- read.nhx(nhxfile)
drop.tip(nhx, c("ADH2", "ADH1"))



cleanEx()
nameEx("getNodeNum")
### * getNodeNum

flush(stderr()); flush(stdout())

### Name: getNodeNum
### Title: getNodeNum
### Aliases: getNodeNum Nnode2

### ** Examples

getNodeNum(rtree(30))
Nnode2(rtree(30))



cleanEx()
nameEx("getRoot")
### * getRoot

flush(stderr()); flush(stdout())

### Name: getRoot
### Title: getRoot
### Aliases: getRoot

### ** Examples

getRoot(rtree(10))



cleanEx()
nameEx("is.ggtree")
### * is.ggtree

flush(stderr()); flush(stdout())

### Name: is.ggtree
### Title: is.ggtree
### Aliases: is.ggtree

### ** Examples

library(ggtree)
p <- ggtree(rtree(30))
is.ggtree(p)



cleanEx()
nameEx("read.baseml")
### * read.baseml

flush(stderr()); flush(stdout())

### Name: read.baseml
### Title: read.baseml
### Aliases: read.baseml

### ** Examples

rstfile <- system.file("extdata/PAML_Baseml", "rst", package="treeio")
mlbfile <- system.file("extdata/PAML_Baseml", "mlb", package="treeio")
read.baseml(rstfile, mlbfile)



cleanEx()
nameEx("read.codeml")
### * read.codeml

flush(stderr()); flush(stdout())

### Name: read.codeml
### Title: read.codeml
### Aliases: read.codeml

### ** Examples

rstfile <- system.file("extdata/PAML_Codeml", "rst", package="treeio")
mlcfile <- system.file("extdata/PAML_Codeml", "mlc", package="treeio")
read.codeml(rstfile, mlcfile) 



cleanEx()
nameEx("read.codeml_mlc")
### * read.codeml_mlc

flush(stderr()); flush(stdout())

### Name: read.codeml_mlc
### Title: read.codeml_mlc
### Aliases: read.codeml_mlc

### ** Examples

mlcfile <- system.file("extdata/PAML_Codeml", "mlc", package="treeio")
read.codeml_mlc(mlcfile)



cleanEx()
nameEx("read.hyphy")
### * read.hyphy

flush(stderr()); flush(stdout())

### Name: read.hyphy
### Title: read.hyphy
### Aliases: read.hyphy

### ** Examples

nwk <- system.file("extdata/HYPHY", "labelledtree.tree", package="treeio")
ancseq <- system.file("extdata/HYPHY", "ancseq.nex", package="treeio")
read.hyphy(nwk, ancseq)



cleanEx()
nameEx("read.hyphy.seq")
### * read.hyphy.seq

flush(stderr()); flush(stdout())

### Name: read.hyphy.seq
### Title: read.hyphy.seq
### Aliases: read.hyphy.seq

### ** Examples

ancseq <- system.file("extdata/HYPHY", "ancseq.nex", package="treeio")
read.hyphy.seq(ancseq)



cleanEx()
nameEx("read.jplace")
### * read.jplace

flush(stderr()); flush(stdout())

### Name: read.jplace
### Title: read.jplace
### Aliases: read.jplace

### ** Examples

jp <- system.file("extdata", "sample.jplace", package="treeio")
read.jplace(jp)



cleanEx()
nameEx("read.nhx")
### * read.nhx

flush(stderr()); flush(stdout())

### Name: read.nhx
### Title: read.nhx
### Aliases: read.nhx

### ** Examples

nhxfile <- system.file("extdata/NHX", "ADH.nhx", package="treeio")
read.nhx(nhxfile)



cleanEx()
nameEx("read.paml_rst")
### * read.paml_rst

flush(stderr()); flush(stdout())

### Name: read.paml_rst
### Title: read.paml_rst
### Aliases: read.paml_rst

### ** Examples

rstfile <- system.file("extdata/PAML_Baseml", "rst", package="treeio")
read.paml_rst(rstfile)



cleanEx()
nameEx("read.phylip")
### * read.phylip

flush(stderr()); flush(stdout())

### Name: read.phylip
### Title: read.phylip
### Aliases: read.phylip

### ** Examples

phyfile <- system.file("extdata", "sample.phy", package="treeio")
read.phylip(phyfile)



cleanEx()
nameEx("read.r8s")
### * read.r8s

flush(stderr()); flush(stdout())

### Name: read.r8s
### Title: read.r8s
### Aliases: read.r8s

### ** Examples

read.r8s(system.file("extdata/r8s", "H3_r8s_output.log", package="treeio"))



cleanEx()
nameEx("read.raxml")
### * read.raxml

flush(stderr()); flush(stdout())

### Name: read.raxml
### Title: read.raxml
### Aliases: read.raxml

### ** Examples

raxml_file <- system.file("extdata/RAxML", "RAxML_bipartitionsBranchLabels.H3", package="treeio")
read.raxml(raxml_file)



cleanEx()
nameEx("show-methods")
### * show-methods

flush(stderr()); flush(stdout())

### Name: show
### Title: show method
### Aliases: show show,codeml-method show,codeml_mlc-method
###   show,phylip-method show,paml_rst-method

### ** Examples

jp <- system.file("extdata", "sample.jplace", package="treeio")
jp <- read.jplace(jp)
show(jp)



cleanEx()
nameEx("write.jplace")
### * write.jplace

flush(stderr()); flush(stdout())

### Name: write.jplace
### Title: write.jplace
### Aliases: write.jplace

### ** Examples

tree <- system.file("extdata", "pa.nwk", package="treeio")
data <- read.csv(system.file("extdata", "pa_subs.csv", package="treeio"),
                stringsAsFactor=FALSE)
outfile <- tempfile()
write.jplace(tree, data, outfile)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
