
globalVariables(".")


##' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname) {
    pkgVersion <- packageDescription(pkgname, fields="Version")
    msg <- paste0(pkgname, " v", pkgVersion, "  ",
                  "For help: https://yulab-smu.github.io/treedata-book/", "\n\n")

    citation <- paste0("If you use ", pkgname, " in published research, please cite:\n\n",
                       "LG Wang, TTY Lam, S Xu, Z Dai, L Zhou, T Feng, P Guo, CW Dunn, BR Jones, T Bradley, H Zhu, Y Guan, Y Jiang, G Yu. ",
                       "treeio: an R package for phylogenetic tree input and output with richly annotated and associated data. ",
                       "Molecular Biology and Evolution 2019, accepted.\n"
                       )

    packageStartupMessage(paste0(msg, citation))
}
