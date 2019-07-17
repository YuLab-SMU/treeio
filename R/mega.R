##' parse tabular output of MEGA
##'
##' 
##' @title read.mega_tabular
##' @param file MEGA tabular file
##' @return treedata object
##' @export
##' @author Guangchuang Yu
read.mega_tabular <- function(file) {
    ## output of MEGA 7 has "datafile=X", while output of MEGA X may not
    skip <- ifelse(grepl("datafile=",
                         suppressMessages(scan(file, n = 1, what = character()))),
                   1, 0)
    d <- suppressMessages(vroom::vroom(file, skip = skip))
    d1 <- d[,c('NodeId','Des1')] %>% dplyr::rename(child=.data$Des1)
    d2 <- d[,c('NodeId','Des2')] %>% dplyr::rename(child=.data$Des2)
    dd <- dplyr::bind_rows(d1, d2) %>% dplyr::filter(child != '-')

    d <- dplyr::mutate(d, label = as.character(.data$NodeId))

    as.phylo(dd) %>% as_tibble %>%
        dplyr::full_join(d, by='label') %>%
        dplyr::mutate(label=sub("^-$", "", .data$NodeLabel)) %>%
        dplyr::select(-c('NodeLabel', 'NodeId', 'Des1', 'Des2')) %>%
        as.treedata
}

##' @rdname beast-parser
##' @export
read.mega <- read.beast
