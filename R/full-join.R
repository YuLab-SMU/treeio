##' @importFrom dplyr full_join
##' @importFrom tibble data_frame
##' @importFrom dplyr select_
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
        lab <- data_frame(node = 1:N, label = label)
        y <- full_join(lab, y, by = "label") %>% select_(~ -label)
    }

    if (nrow(x@extraInfo) == 0) {
        x@extraInfo <- y
    } else {
        x@extraInfo %<>% full_join(y, by = "node", copy = copy, suffix = suffix)
    }
    return(x)
}
