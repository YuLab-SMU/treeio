build_new_labels <- function(tree){
    node2label_old <- tree %>% as_tibble() %>% dplyr::select(c("node", "label"))
    if (inherits(tree, "treedata")){
        tree <- tree@phylo
    }
    tree$tip.label <- paste0("t", seq_len(Ntip(tree)))
    tree$node.label <- paste0("n", seq_len(Nnode(tree)))
    node2label_new <- tree %>% as_tibble() %>% dplyr::select(c("node", "label"))
    old_and_new <- node2label_old %>%
                   dplyr::inner_join(node2label_new, by="node") %>%
                   dplyr::rename(old="label.x", new="label.y")
    return (list(tree=tree, node2old_new_lab=old_and_new))
}

build_new_tree <- function(tree, node2old_new_lab){
    # replace new label with old label
    treeda <- tree %>% as_tibble()
    treeda1 <- treeda %>%
               dplyr::filter(.data$label %in% node2old_new_lab$new)
    treeda2 <- treeda %>%
               dplyr::filter(!(.data$label %in% node2old_new_lab$new))
    # original label
    treeda1$label <- node2old_new_lab[match(treeda1$label, node2old_new_lab$new), "old"] %>%
                     unlist(use.names=FALSE)
    treeda <- rbind(treeda1, treeda2)
    tree <- treeda[order(treeda$node),] %>% as.phylo()
    return (tree)
}

old_new_node_mapping <- function(oldtree, newtree){
    treelab1 <- oldtree %>%
                as_tibble() %>%
                dplyr::select(c("node", "label"))
    treelab2 <- newtree %>%
                as_tibble() %>%
                dplyr::select(c("node", "label"))
    node_map <- dplyr::inner_join(treelab1, treelab2, by="label") %>%
                dplyr::select(c("node.x", "node.y")) %>%
                dplyr::rename(c(old="node.x", new="node.y"))
    return(node_map)
}
