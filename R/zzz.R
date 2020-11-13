
globalVariables(".")


##' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname) {
    pkgVersion <- packageDescription(pkgname, fields="Version")
    msg <- paste0(pkgname, " v", pkgVersion, "  ",
                  "For help: https://yulab-smu.top/treedata-book/", "\n\n")

    citation <- paste0("If you use ", pkgname, " in published research, please cite:\n\n",
                       "LG Wang, TTY Lam, S Xu, Z Dai, L Zhou, T Feng, P Guo, CW Dunn, BR Jones, T Bradley, H Zhu, Y Guan, Y Jiang, G Yu. ",
                       "treeio: an R package for phylogenetic tree input and output with richly annotated and associated data. ",
                       "Molecular Biology and Evolution 2020, 37(2):599-603. doi: 10.1093/molbev/msz240\n"
                       )

    packageStartupMessage(paste0(msg, citation))
}
