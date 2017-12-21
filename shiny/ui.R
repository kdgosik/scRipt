#' ShinyUI
#'
#' Runs and visulizes the 10X clustering from the Cell Ranger website
#'
#' Runs and visulizes the 10X clustering from the Cell Ranger website
#'
#' @param id shiny id
#' @return shinyUI for module
#' @export
#' @import shiny
#' @import shinyFiles
#' @import cellranger
#' @author Kirk Gosik <kgosik@broadinstitute.org>
#' @examples


# if cellranger R kit is not installed then install it
if( !{"cellrangerRkit" %in% installed.packages()} ) {
  source("http://cf.10xgenomics.com/supp/cell-exp/rkit-install-2.0.0.R")
}
require(cellrangerRkit)


# source("../R/ModularUMItSNEPlot.R") ## will move to main part of the package
# source("../R/ModularClusterExplore10x.R") ## will move to main part of the package
# source("../R/ModularIdentifytSNE.R") ## will move to main part of the package
# #source("src/ModularMorpheusPlot.R")

shinyUI(
  fluidPage(

  # Application title
  titlePanel("Gene Expression Explore"),

  # Sidebar with a selector for genes
  sidebarLayout(

    sidebarPanel(

      radioButtons(inputId = "task",
                   label = "Select Task",
                   choices = c("Create Output", "Select Data")),

      conditionalPanel(

        condition = "input.task == 'Create Output'",

        shinyDirButton(id = "file_path",
                       label = "10X Path",
                       title = "Button"),

        p("eg  ../filtered_gene_bc_matrices/hg19"),

        textInput("project", "Project Title"),

        textInput("tissue_type", "Tissue Type"),

        textInput("cell_type", "Cell Type (if known)"),

        sliderInput(inputId = "cells",
                    label = "Minimum Cells Per Gene",
                    value = 3,
                    min = 1,
                    max = 20),

        sliderInput(inputId = "genes",
                    label = "Minimum Genes Per Cell",
                    value = 200,
                    min = 10,
                    max =  500),

        sliderInput(inputId = "max_mt",
                     label = "Max Percent Mitochondrial Genes Present",
                     value = 0.05,
                     min = 0,
                     max = 0.5,
                     step = 0.01),

        actionButton("create_output", "Create Output")

      ), # conditionalPanel

      conditionalPanel(

        condition = "input.task == 'Select Data'",

        selectInput("data_source", "Select Data Source", choices = gsub(".rds","",dir("../data", pattern = ".rds"))),
        actionButton("read_data", "Read Data"),
        uiOutput("ui_data_load")


      ) # conditionalPanel


    ), # sidebarPanel


    mainPanel(

      tabsetPanel(

        tabPanel(title = "Introduction",
                 includeMarkdown("www/AppIntroduction.md")
        ), # tabPanel

        # Show the t-SNE plot
        tabPanel(title = "Cellranger tSNE",
          UMItSNEPlotUI("tSNE")
          ), # tabPanel

        tabPanel(title = "Cellranger Cluster",
          ClusterExplore10xUI("cluster_explore")
          ), # tabPanel

        tabPanel(title = "Seurat",
                textOutput("seurat"),
                IdentifytSNEUI("seurat_out")
        ) # tabPanel
        #), # tabPanel
        #
        # tabPanel(title = "Morpheus",
        #          MorpheusUI("morpheus_out")
        #
        # ) # tabPanel

      ) # tabsetPanel

      ) # mainPanel

    ) # sidebarLayout

  ) # fluidPage

) # shinyUI
