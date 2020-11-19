##' @importFrom dplyr full_join
##' @importFrom tibble tibble
##' @method full_join treedata
##' @export
full_join.treedata <- function(x, y, by = NULL,
                               copy = FALSE, suffix = c(".x", ".y"), ...) {

    by <- match.arg(by, c("node", "label"))
    y <- as_tibble(y)
    if (by == "label") {
        ntip <- Ntip(x)
        N <- Nnode2(x)
        label <- rep(NA, N)
        label[1:ntip] <- x@phylo[["tip.label"]]
        if (!is.null(x@phylo$node.label)) {
            label[(ntip+1):N] <- x@phylo$node.label
        }
        lab <- tibble(node = 1:N, label = label)
        y <- full_join(lab, y, by = "label") %>% select(-.data$label)
    }

    if (nrow(x@extraInfo) == 0) {
        x@extraInfo <- y
    } else {
        x@extraInfo <- full_join(x@extraInfo, y, by = "node", copy = copy, suffix = suffix)
    }
    return(x)
}

##' @method full_join phylo
##' @export
full_join.phylo <- function(x, y, by = NULL,
                            copy = FALSE, suffix = c(".x", ".y"), ...) {
    full_join(as_tibble(x), y = y, by = by,
              copy = copy, suffix = suffix, ...) %>%
        as.treedata
}
