context('nhx')

## Some sample NHX tree strings
test_nhx_text <- readLines(system.file("extdata/NHX", "ADH.nhx", package="treeio"))

test_phyldog_nhx_text = readLines(system.file("extdata/NHX", "phylodog.nhx", package="treeio"))

test_compra_nhx_text = readLines(system.file("extdata/NHX", "compra.nhx", package="treeio"))


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

	# expect_equal( ncol( nhx@data ), ncol( nhx_reduced@data ) )

})

test_that("can drop tips from ensembl compra tree", {
	nhx <- read.nhx( textConnection( test_compra_nhx_text ) )
	to_drop = 
		c(1, 2, 3, 4, 5, 6, 7, 8, 12, 13, 14, 15, 16, 18, 
		19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 
		32, 33, 34, 35, 36, 39, 40, 41, 42, 43, 44, 45, 46, 
		47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 
		60, 61, 62, 63, 64, 68, 69, 71, 72, 74, 75, 76, 77, 
		78, 79, 80, 81, 82, 84, 85, 86, 87, 89, 90, 91, 92, 
		93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 
		105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 
		116, 117, 119, 120, 121, 122, 123, 124, 125, 126, 127, 
		128, 129, 130, 134, 135, 137, 138, 139, 140, 141, 142, 
		143, 144, 146, 147, 149, 151, 152, 153, 154, 155, 156, 
		157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 
		169, 170, 171, 175, 176, 178, 179, 180, 181, 182, 183, 
		184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 
		195, 196, 197, 198, 201, 203, 204, 205, 206, 207, 208, 
		209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 
		220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 
		231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 242, 
		243, 244, 246, 247, 248, 249, 250, 251, 252, 254, 255, 
		259, 260, 262, 264, 265, 266, 267, 268, 269, 270, 271, 
		272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 
		283, 284, 285, 286, 287, 288, 289, 290, 291, 293, 294, 
		295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 306, 
		307, 308, 309, 310, 311, 313, 314, 316, 317, 318, 319, 
		323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 
		334, 335, 336, 337, 338, 339, 340, 341, 342, 344, 345, 
		346, 347, 348, 349, 350, 351, 353, 354, 355, 357, 358, 
		362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 
		373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 
		384, 385, 387, 388, 389, 390, 391, 392, 393, 394, 395, 
		396, 397, 398, 399)

		nhx_reduced = drop.tip(nhx, to_drop)
			
		# Make sure node numbers unique
		expect_false( any(duplicated(nhx_reduced@data$node)) )

		# Make sure the same node numbers are present in @nhx_tag and @phylo
		edge = nhx_reduced@phylo$edge
		dim(edge) = NULL
		edge = unique(edge)
		expect_true( setequal( edge, nhx_reduced@data$node ) )

		# Check the expected number of tips after dropping
		expect_equal( length(nhx_reduced@phylo$tip.label), ( length(nhx@phylo$tip.label) - length(to_drop) ) )
		expect_true( all(nhx_reduced@data$node %in% 1:Nnode2(nhx_reduced)) )

		# Check that node labels are still there
		expect_equal( length( nhx_reduced@phylo$node.label ), 50 )

		# Check that the number of columns is the same before and 
		# after reduction. This makes sure temporary book keepping 
		# columns were removed
		expect_equal( ncol( nhx@data ), ncol( nhx_reduced@data ) )
})
