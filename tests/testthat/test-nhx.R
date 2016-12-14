context('nhx')

## Some sample NHX tree strings
test_nhx_text <- readLines(system.file("extdata/NHX", "ADH.nhx", package="treeio"))

test_phyldog_nhx_text = readLines(system.file("extdata/NHX", "phylodog.nhx", package="treeio"))

test_notung_nhx_text = readLines(system.file("extdata/NHX", "notung.nhx", package="treeio"))

# A function to simplify NHX text so that it can be parsed by
# ape::read.tree(). Discards much useful information. Intent is to
# be able to compare node annotations that have been independently
# parsed with different methods.
simplify_nhx_string <- function( text ){
	# Remove branch lengths so NHX tags are adjacent to nodes
	# Accommodate lengths in scientific notation, eg 1e-6
	text = gsub( "\\:[\\d\\.]+e[\\d\\-]+","", text, perl=TRUE )
	text = gsub( "\\:[\\d\\.]+","", text, perl=TRUE )

	# Remove NHX tags at tips
	text = gsub( "([^\\)])\\[.+?\\]","\\1", text, perl=TRUE )

	# Remove brackets
	text = gsub( "\\[&&NHX:","", text, perl=TRUE )
	text = gsub( "\\]","", text, perl=TRUE )

	# Replace NHX tag formatting characters that aren't allowed
	text = gsub( ":","_", text, perl=TRUE )
	text = gsub( "=","-", text, perl=TRUE )

	return(text)
}


test_that("can parse example ggtree nhx tree string", {
	nhx <- read.nhx( textConnection(test_nhx_text) )
	ntips = length(nhx@phylo$tip.label)

	# Correct number of tips
	expect_equal( ntips , 8 )

	# All node numbers, including tips, are parsed
	expect_true( all(!is.na(nhx@data$node)) )
})

test_that("can parse phyldog nhx tree string", {
	nhx <- read.nhx( textConnection(test_phyldog_nhx_text) )
	ntips = length(nhx@phylo$tip.label)

	# Correct number of tips
	expect_equal( ntips , 16 )

	# All node numbers, including tips, are parsed
	expect_true( all(!is.na(nhx@data$node)) )

	# Verify that the S field of internal nodes was correctly parsed
	# Assumes that node identity order is the same between phy and nhx@phylo
	tags = nhx@data
	tags$node = as.numeric(tags$node)
	tags = tags[order(tags$node),]
	internal_tags = tags[ tags$node > length(nhx@phylo$tip.label), ] # Consider internal nodes only


	phy = read.tree(text=simplify_nhx_string(test_phyldog_nhx_text))
	phy_S=unlist(lapply(strsplit(phy$node.label, "_"), function(x) x[[2]])) # Get the S field
	phy_S=unlist(lapply(strsplit(phy_S, "-"), function(x) x[[2]])) # Get the value
	phy_S=as.numeric(phy_S)
	expect_equal( phy_S, as.numeric(internal_tags$S) )

	# Verify that S fild of tips was correctly parsed
	# by comparison against expected values
	tip_tags = tags[1:length(nhx@phylo$tip.label),]
	tip.labels = c("Prayidae_D27SS7@2825365", "Kephyes_ovata@2606431", "Chuniphyes_multidentata@1277217", "Apolemia_sp_@1353964", "Bargmannia_amoena@263997", "Bargmannia_elongata@946788", "Physonect_sp_@2066767", "Stephalia_dilata@2960089", "Frillagalma_vityazi@1155031", "Resomia_ornicephala@3111757", "Lychnagalma_utricularia@2253871", "Nanomia_bijuga@717864", "Cordagalma_sp_@1525873", "Rhizophysa_filiformis@3073669", "Hydra_magnipapillata@52244", "Ectopleura_larynx@3556167")
	S.tip.values = c(58, 69, 70, 31, 37, 38, 61, 52, 53, 54, 65, 71, 64, 26, 16, 15)
	expect_equal( S.tip.values[match(nhx@phylo$tip.label, tip.labels)], as.numeric(tip_tags$S))

})

test_that("can drop tips", {
	nhx <- read.nhx( textConnection(test_phyldog_nhx_text) )
	to_drop = c("Physonect_sp_@2066767", "Lychnagalma_utricularia@2253871", "Kephyes_ovata@2606431")

	nhx_reduced = drop.tip(nhx, to_drop)

	# Make sure node numbers unique
	expect_false( any(duplicated(nhx_reduced@data$node)) )

	# Make sure the same node numbers are present in @nhx_tag and @phylo
	edge = nhx_reduced@phylo$edge
	dim(edge) = NULL
	edge = unique(edge)
	expect_true( setequal( edge, nhx_reduced@data$node ) )

	# Check the expected number of tips after dropping
	expect_equal( length(nhx_reduced@phylo$tip.label), 13 )
	expect_true( all(nhx_reduced@data$node %in% 1:Nnode2(nhx_reduced)) )
})

