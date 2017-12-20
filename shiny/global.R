# if cellranger R kit is not installed then install it
if( !{"cellrangerRkit" %in% installed.packages()} ) {
  source("http://cf.10xgenomics.com/supp/cell-exp/rkit-install-2.0.0.R")
}

pkgs <- c("shiny", "shinyFiles", "shinycssloaders", "cellranger", "Seurat", 
          "ggplot2", "plotly", "dplyr", "Matrix")
pkgs <- pkgs[!{pkgs %in% installed.packages()[,1]}]

if( !{all(pkgs %in% installed.packages()[,1])} ) install.packages(pkgs)

