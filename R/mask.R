##' site mask
##'
##'
##' @title mask
##' @param tree_object tree object
##' @param field selected field
##' @param site site
##' @param mask_site if TRUE, site will be masked.
##'                  if FALSE, selected site will not be masked, while other sites will be masked.
##' @return updated tree object
##' @export
##' @author Guangchuang Yu
mask <- function(tree_object, field, site, mask_site=FALSE) {
    if (! field %in% get.fields(tree_object)) {
        stop("'field' is not available in 'tree_object'...")
    }

    in_data_slot <- TRUE
    if (field %in% colnames(tree_object@data)) {
        field_data <- tree_object@data[[field]]
    } else {
        field_data <- tree_object@extraInfo[[field]]
        in_data_slot <- FALSE
    }

    ## field_data <- sapply(field_data, gsub, pattern="\n", replacement="/")
    tbl <- tibble(field = field_data, id = seq_along(field_data)) %>%
        filter_(~ !is.na(field)) %>% filter_(~field != "")

    tokens <- strsplit(tbl$field, " / ")
    lpos <- lapply(tokens, function(x) {
        gsub("^[a-zA-Z]+", "", x) %>%
            gsub("[a-zA-Z]\\s*$", "", .) %>%
            as.numeric
    })

    pos <- unique(unlist(lpos))

    if (mask_site == FALSE) {
        pos2 <- 1:max(pos)
        pos2 <- pos2[-site]
        site <- pos2
    }

    site <- site[site %in% pos]

    for (i in seq_along(tbl$field)) {
        if (any(lpos[[i]] %in% site)) {
            j <- which(lpos[[i]] %in% site)
            x <- paste(tokens[[i]][-j], collapse = " / ")
            if (length(x) == 0) {
                tbl$field[[i]] <- ""
            } else {
                tbl$field[[i]] <- x
            }
        }
    }

    field_data[tbl$id] <- tbl$field

    if (in_data_slot) {
        tree_object@data[[field]] <- field_data
    } else {
        tree_object@extraInfo[[field]] <- field_data
    }

    tree_object
}
