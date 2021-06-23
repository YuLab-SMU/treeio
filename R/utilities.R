
check_edgelist <- function(edgelist) {
    if (dim(edgelist)[2] < 2)
        stop("input should be a matrix of edge list that holds the relationships in the first two columns")
    if (length(unique(edgelist[[1]])) > length(unique(edgelist[[2]]))) {
        children <- edgelist[[1]]
        parents <- edgelist[[2]]
    } else {
        children <- edgelist[[2]]
        parents <- edgelist[[1]]
    }
    root1 <- unique(parents[!(parents %in% children)])
    root2 <- unique(parents[parents == children])
    if (length(root1) != 1 && length(root2) != 1)
        stop("Cannot find root. network is not a tree!")
    if (length(root1) != 1 && length(root2) == 1){
        indx <- parents != children
        parents <- parents[indx]
        children <- children[indx]
        edge <- matrix(c(parents, children), ncol=2)
        attr(edge, "indx") <- indx
    }else{
        edge <- matrix(c(parents, children), ncol=2)
    }
    return (edge)
}


is_numeric <- function(x) !anyNA(suppressWarnings(as.numeric(as.character(x))))

filename <- function(file) {
    ## textConnection(text_string) will work just like a file
    ## in this case, just set the filename as ""
    file_name <- ""
    if (is.character(file)) {
        file_name <- file
    }
    return(file_name)
}

jplace_treetext_to_phylo <- function(tree.text) {
    ## move edge label to node label separate by @@
    ## The tree in jplace file format: The edge number is not always in curly braces.
    ## they are sometimes in square brackets.
    if(grepl("\\{(\\d+)\\}", tree.text)){
        tr <- gsub('(:[0-9\\.eE\\+\\-]+)\\{(\\d+)\\}', '\\@@\\2\\1', tree.text)
    }
    if(grepl("\\[(\\d+)\\]", tree.text)){
        message("The version of jplace file is 1.0.")
        tr <- gsub('(:[0-9\\.eE\\+\\-]+)\\[(\\d+)\\]', '\\@@\\2\\1', tree.text)
    }
    phylo <- read.tree(text=tr)
    if (length(grep('@@', phylo$tip.label)) > 0) {
        phylo$node.label[1] <- gsub("(.*)\\{(\\d+)\\}", "\\1@@\\2",  phylo$node.label[1])
        tip.edgeNum <- as.numeric(gsub(".*@@(\\d*)", "\\1",phylo$tip.label))
        node.edgeNum <- as.numeric(gsub(".*@@(\\d*)", "\\1",phylo$node.label))
        phylo$tip.label <- gsub("@@\\d+", "", phylo$tip.label)
        phylo$node.label <- gsub("@@\\d+", "", phylo$node.label)
        if (all(phylo$node.label == "")) {
            phylo$node.label <- NULL
        }

        N <- getNodeNum(phylo)
        edgeNum.df <- data.frame(node=1:N, edgeNum=c(tip.edgeNum, node.edgeNum))
        ## root node is not encoded with edge number
        edgeNum.df <- edgeNum.df[!is.na(edgeNum.df[,2]),]
        attr(phylo, "edgeNum") <- edgeNum.df
    }

    ## using :edge_length{edge_num} to match edge_num to node_num
    ## this is not a good idea since there may exists identical edge_length.
    ## but we can use it to verify our method.
    ##
    ## en.matches <- gregexpr(":[0-9\\.eE\\+\\-]+\\{\\d+\\}", tree.text)
    ## matches <- en.matches[[1]]
    ## match.pos <- as.numeric(matches)
    ## match.len <- attr(matches, 'match.length')

    ## edgeLN <- substring(tree.text, match.pos+1, match.pos+match.len-2)
    ## edgeLN.df <- data.frame(length=as.numeric(gsub("\\{.+", "", edgeLN)),
    ##                         edgeNum = as.numeric(gsub(".+\\{", "", edgeLN)))

    ## xx <- merge(edgeLN.df, edgeNum.df, by.x="node", by.y="node")

    return(phylo)
}


## convert edge number to node number for EPA/pplacer output
edgeNum2nodeNum <- function(jp, edgeNum) {
    edges <- attr(jp@phylo, "edgeNum")
    idx <- match(edgeNum, edges$edgeNum)
    flagna <- is.na(idx)
    idx <- idx[!flagna]
    if (any(flagna) & length(idx)>0){
        na_edgeNum <- paste(edgeNum[which(flagna)], collapse="; ")
        stop(paste("The following edges: ",na_edgeNum, ", couldn't be found", sep=""), call. = FALSE)
        #idx <- idx[!flagna]
    }
    #idx <- which(edges$edgeNum == edgeNum)
    if (length(idx) == 0) {
        return(NA)
    }

    edges[idx, "node"]
}

is.tree <- function(x) {
    if (class(x) %in% c("phylo",
                        "phylo4",
                        "jplace",
                        "treedata")
        ) {
        return(TRUE)
    }
    return(FALSE)
}

##' @importFrom methods .hasSlot is missingArg new slot slot<-
has.slot <- function(object, slotName) {
    if (!isS4(object)) {
        return(FALSE)
    }
    .hasSlot(object, slotName)
}

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
