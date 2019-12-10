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
    ## the first column of placements maybe a matrix or one numeric vector,
    ## so when it is numeric vector, the nplaces will be 1.
    ## and the type of nm also is various.
    if (!inherits(places, "matrix")){
        nplaces <- 1
    } else{
        nplaces <- nrow(places)
    }
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
    ##example:
    ## when first column of plamcements is [[1,2,3,4,5],[3,4,5,6,7],[6,7,3,2,4]] (3 row x 5 columns matrix),
    ## and the n column is ["read1", "read2"] (the type of n is character vector), so 
    ## will use "inherits(nm, "character")" block.
   	## this will first generate two same matrix contained 3 row x 5 columns, because the length of n is two (the nmsize argument).
    places.df <- rep(list(places), nmsize)
    ## then this will generate the names of each matrix for the nm.
    ## example result is: rep(c("read1", "read2"), rep(3,2)), here 3 is nplaces (the nrow of first column of placements),
    ## 2 is the length of nm.
    name <- rep(tmpn, rep(nplaces, nmsize)) 
    places.df <- do.call("rbind", places.df)
    places.df <- data.frame(name=name, places.df, stringsAsFactors=FALSE)
    return(places.df) 
}


mergenm <- function(n, nm){
    ## merge the n and nm.
    ## it is impossible that n and nm is empty simultaneously,
    ## so we will keep the column not NULL.
    if(is.null(n)&&!is.null(nm)) {return(nm)}
    if(is.null(nm)&&!is.null(n)) {return(n)}
    if(is.null(n)&&is.null(nm)){ 
        stop("the placements of jplace should have corresponding name!")
    }
}


extract.placement <- function(object, phylo) {
    placements <- object$placements
    if (ncol(placements)==2){
        ## when placements contained p and n two columns,
        ## this will process placements row by row with getplacedf function.
        ## The order of `p` and `n` column is not fixed. I think colnames of
        ## placements (`p`, `n`, `nm`) are fixed, but when column number is
        ## two, the `n` or `nm` is not fixed.
        nameidx <- match("p", colnames(placements))
        place.df <- mapply(getplacedf,
                           placements$p,
                           placements[,-nameidx],
                           SIMPLIFY=FALSE)
    }
    if(ncol(placements)==3){
        ## when placements contained p ,n and nm three columns,
        ## first, we merge n and nm row by row.
        tmpname <- mapply(mergenm,
                          placements$n,
                          placements$nm, 
                          SIMPLIFY=FALSE)
        ## then, it becomes the same as two columns.
        place.df <- mapply(getplacedf,
                           placements$p,
                           tmpname,
                           SIMPLIFY=FALSE)
    }
    place.df <- do.call("rbind", place.df)
    colnames(place.df) <- c("name", object$fields)
    ## place <- placements[,1]
    
    ## ids <- NULL
    ## if (length(placements) == 2) {
    ##	tmpids <- placements[,2]
    ## }else{
    ##	tmpids <- list(unlist(placements[,2]), unlist(placements[,3]))
    ## }
    ## ids <- vapply(tmpids, function(x) x[1], character(1))
    ## names(place) <- ids
    ## place.df <- do.call("rbind", place)
    ## row.names(place.df) <- NULL
    ## if (!is.null(ids)) {
    ##    nn <- rep(ids, vapply(place, function(x) {
    ##        nr <- nrow(x)
    ##        if (is.null(nr))
    ##            return(1)
    ##        return(nr)
    ##    }, numeric(1)))
    ##    place.df <- data.frame(name=nn, place.df)
    ##    colnames(place.df) <- c("name", object$fields)
    ## } else {
    ##    colnames(place.df) <- object$fields
    ## }
    edgeNum.df <- attr(phylo, "edgeNum")
    place.df <- merge(place.df, edgeNum.df, by.x = "edge_num", by.y = "edgeNum")
    place.df <- get_newdata(place.df)
    as_tibble(place.df)
}

## To avoid the character column
get_newdata <- function(placedf){
    tmpfile <- tempfile()
    utils::write.csv(placedf, tmpfile)
    placementdf <- utils::read.csv(tmpfile, row.names=1, stringsAsFactors=FALSE)
    ## file.remove(tmpfile)
    return(placementdf)
}


