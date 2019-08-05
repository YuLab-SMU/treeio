##' read jplace file
##'
##'
##' @title read.jplace
##' @param file jplace file
##' @return \code{jplace} instance
##' @importFrom jsonlite fromJSON
##' @export
##' @author Guangchuang Yu
##' @examples
##' jp <- system.file("extdata", "sample.jplace", package="treeio")
##' read.jplace(jp)
read.jplace <- function(file) {
    fields <- tree <- placements <- NULL
    version <- metadata <- NULL
    jtree <- fromJSON(file)
    phylo <- jplace_treetext_to_phylo(jtree$tree)
    placements <- extract.placement(jtree, phylo)
    info <- c(jtree$metadata, version=jtree$version, parser = "read.jplace")
    res <- new("jplace",
        treetext   = jtree$tree,
        phylo      = phylo,
        placements = placements,
        info       = info,
        file       = filename(file)
        )

    res@data <- summarize_placement(res)
    return(res)
}

##' @importFrom dplyr summarize
##' @importFrom dplyr n
##' @importFrom dplyr mutate_
summarize_placement <- function(tree) {
    place <- get.placements(tree, by="best")
    ids <- data_frame(node = nodeIds(tree, internal.only = FALSE))
    group_by_(place, ~node) %>% summarize(nplace=n()) %>%
        full_join(ids, by='node') %>%
        mutate_(nplace = ~ ifelse(is.na(nplace), 0, nplace))
}

##' @method get.placements jplace
##' @param by one of 'best' and 'all'
##' @export
##' @rdname get-placements
##' @importFrom dplyr group_by_
##' @importFrom dplyr filter_
get.placements.jplace <- function(tree, by="best", ...) {
    placements <- tree@placements
    if (!'likelihood' %in% names(placements))
        return(placements)

    if (by == "best") {
        ## http://astrostatistics.psu.edu/su07/R/html/base/html/all.equal.html
        ## due to precision, number are identical maynot be equal,
        ## so use all.equal which can test nearly equal number
        ## if not equals, the output is a descript string of the differences
        placements <- group_by_(placements, ~name) %>%
            filter_(~likelihood == min(likelihood))
    }
    return(placements)
}

getplacedf <- function(places, nm){
	nplaces <- nrow(places)
	if (inherits(nm, "matrix")){
		nmsize <- nrow(nm)
		tmpn <- nm[,1]
	}
	if (inherits(nm, "list")){
		nmsize <- length(nm)
		tmpn <- vapply(nm, function(x)x[1], character(1))
	}
	if (inherits(nm, "character")){
		nmsize <- length(nm)
		tmpn <- as.vector(nm)
	}
	places.df <- rep(list(places), nmsize)
	name <- rep(tmpn, rep(nplaces, nmsize)) 
	places.df <- do.call("rbind", places.df)
	places.df <- data.frame(name=name, places.df, stringsAsFactors=FALSE)
	return(places.df) 
}


mergenm <- function(n, nm){
	if(is.null(n)&&!is.null(nm)){return(nm)}
	if(is.null(nm)&&!is.null(n)){return(n)}
	if(is.null(n)&&is.null(nm)){ 
		stop("the placements of jplace should have corresponding name!")
	}
}


extract.placement <- function(object, phylo) {
    placements <- object$placements
	if (ncol(placements)==2){
		place.df <- mapply(getplacedf,
						   placements[,1],
						   placements[,2],
						   SIMPLIFY=FALSE)
	}
	if(ncol(placements)==3){
		tmpname <- mapply(mergenm,
						  placements[,2],
						  placements[,3], 
						  SIMPLIFY=FALSE)
		place.df <- mapply(getplacedf,
						   placements[,1],
						   tmpname,
						   SIMPLIFY=FALSE)
	}
	place.df <- do.call("rbind", place.df)
	colnames(place.df) <- c("name", object$fields)
    #place <- placements[,1]

    #ids <- NULL
    #if (length(placements) == 2) {
	#	tmpids <- placements[,2]
    #}else{
	#	tmpids <- list(unlist(placements[,2]), unlist(placements[,3]))
	#}
	#ids <- vapply(tmpids, function(x) x[1], character(1))
	#names(place) <- ids
    #place.df <- do.call("rbind", place)
    #row.names(place.df) <- NULL
    #if (!is.null(ids)) {
    #    nn <- rep(ids, vapply(place, function(x) {
    #        nr <- nrow(x)
    #        if (is.null(nr))
    #            return(1)
    #        return(nr)
    #    }, numeric(1)))
    #    place.df <- data.frame(name=nn, place.df)
    #    colnames(place.df) <- c("name", object$fields)
    #} else {
    #    colnames(place.df) <- object$fields
    #}
    edgeNum.df <- attr(phylo, "edgeNum")
    place.df <- merge(place.df, edgeNum.df, by.x = "edge_num", by.y = "edgeNum")
	place.df <- getnewplacements(place.df)
    as_tibble(place.df)
}

# To avoid the character column
getnewplacements <- function(placedf){
	tmpfile <- tempfile()
	write.csv(placedf, tmpfile)
	placementdf <- read.csv(tmpfile, row.names=1)
	file.remove(tmpfile)
	return(placementdf)
}


