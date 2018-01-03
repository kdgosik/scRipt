#' ShinyUI
#'
#' Runs and visulizes the 10X clustering from the Cell Ranger website
#'
#' Runs and visulizes the 10X clustering from the Cell Ranger website
#'
#' @param id The namespace for the shiny application.
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

## comment out the source statements
## comment out the shinyApp() statement at bottom
## change line 110 to take away the ../

# source("ModularUMItSNEPlot.R")  ## will move to main part of the package
# source("ModularClusterExplore10x.R") ## will move to main part of the package
# source("ModularIdentifytSNE.R")  ## will move to main part of the package


ui <- shinyUI(
  fluidPage(

    # Application title
    titlePanel("Gene Expression Explore"),

    # Sidebar with a selector for genes
    sidebarLayout(

      sidebarPanel(

        shiny::radioButtons(inputId = "task",
                            label = "Select Task",
                            choices = c("Create Output", "Select Data")),

        conditionalPanel(

          condition = "input.task == 'Create Output'",

          shinyFiles::shinyDirButton(id = "file_path",
                                     label = "10X Path",
                                     title = "Button"),

          shiny::p("eg  ../filtered_gene_bc_matrices/hg19"),

          shiny::textInput("project", "Project Title"),

          shiny::textInput("tissue_type", "Tissue Type"),

          shiny::textInput("cell_type", "Cell Type (if known)"),

          shiny::sliderInput(inputId = "cells",
                             label = "Minimum Cells Per Gene",
                             value = 3,
                             min = 1,
                             max = 20),

          shiny::sliderInput(inputId = "genes",
                             label = "Minimum Genes Per Cell",
                             value = 200,
                             min = 10,
                             max =  500),

          shiny::sliderInput(inputId = "max_mt",
                             label = "Max Percent Mitochondrial Genes Present",
                             value = 0.05,
                             min = 0,
                             max = 0.5,
                             step = 0.01),

          shiny::actionButton("create_output", "Create Output")

        ), # conditionalPanel

        conditionalPanel(

          condition = "input.task == 'Select Data'",

          shinyFiles::shinyDirButton(id = "data_source",
                                     label = "Created Output Path",
                                     title = "Button"),

          shiny::selectInput(inputId = "rds_file",
                             label = "Select RDS file",
                             choices = ""),

          shiny::actionButton("read_data", "Read Data"),
          shiny::uiOutput("ui_data_load")


        ) # conditionalPanel


      ), # sidebarPanel


      mainPanel(

        tabsetPanel(

          tabPanel(title = "Introduction",
                   includeMarkdown("inst/www/AppIntroduction.md")
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





#' ShinyServer
#'
#' Runs and visulizes the 10X clustering from the Cell Ranger website
#'
#' Runs and visulizes the 10X clustering from the Cell Ranger website
#'
#' @param id shiny id
#' @return shinyUI for module
#' @export
#' @param input List-like object that stores the current values of all of the widgets in your app.
#' @param output List-like object that stores instructions for building the R objects in your app.
#' @param session List-like object about the session to be passed to the UI.
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


server <- function(input, output, session) {

  # defines root directory for the user
  shinyFiles::shinyDirChoose(input, 'file_path', roots = c(root = '/'))

  # defines root directory for the user
  shinyFiles::shinyDirChoose(input, 'data_source', roots = c(root = '/'))

  # creates seurat tutorial markdown to html file
  observeEvent(input$create_output, {

    withProgress(message = "Creating Output File...", {

      project_inpt <- gsub(" ", "_", input$project)
      home <- normalizePath("/") # normalizes home path
      path <- file.path(home, paste(unlist(input$file_path$path[-1]), collapse = .Platform$file.sep))

      tissue_inpt <- ifelse(is.null(input$tissue_type), NULL, gsub(" ", "_", input$tissue_type))
      celltype_inpt <- ifelse(is.null(input$cell_type), NULL, gsub(" ", "_", input$cell_type))

      project_name <- paste0(c(project_inpt,
                               tissue_inpt,
                               celltype_inpt,
                               format(Sys.Date())), collapse = "-")

      rmarkdown::render(input = "R/Seurat_to_Markdown.Rmd",
                        params = list(project = project_name,
                                      path = path,
                                      cells = input$cells,
                                      genes = input$genes,
                                      max_mt = input$max_mt),
                        output_file = paste0("output/", project_name, ".html"))

    })

  })

  created_output_path <- reactive({

    home <- normalizePath("/") # normalizes home path
    path <- file.path(home, paste(unlist(input$data_source$path[-1]), collapse = .Platform$file.sep))
    path

  })

  observe({

    updateSelectInput(session,
                      inputId = "rds_file",
                      label = "Select RDS file",
                      choices = gsub(".rds", "", dir(created_output_path(), pattern = ".rds")))
  })

  ## reads in already created seurat object
  seurat_obj <- eventReactive(input$read_data, {

    withProgress(message = "Loading Data...", {

      readRDS(file = dir(created_output_path(), pattern = paste0(input$rds_file, ".rds"), full.names = TRUE))

    })

  })


  outs <- eventReactive(input$read_data, {

    # load(file = dir("data", pattern = input$data_source, full.names = TRUE))
    # path stored in obj@misc from running Seurat_to_Markdown
    selected_path <- seurat_obj()@misc

    # reducing Seurat path for Read10x to cellranger expected path for load_cellranger_matrix
    outs_pos <- grep("outs", unlist(strsplit(selected_path, "/"))) - 1
    selected_path <- file.path(paste(unlist(strsplit(selected_path, "/"))[1:outs_pos], collapse = .Platform$file.sep))

    # loads gene - barcode matrix
    gbm <- cellrangerRkit::load_cellranger_matrix( selected_path )

    # normalize nonzero genes
    use_genes <- cellrangerRkit::get_nonzero_genes(gbm)
    gbm_bcnorm <- cellrangerRkit::normalize_barcode_sums_to_median(gbm[use_genes, ])
    gbm_log <- cellrangerRkit::log_gene_bc_matrix(gbm_bcnorm, base = 10)

    # loads analysis results
    analysis_results <- cellrangerRkit::load_cellranger_analysis_results( selected_path )

    # returns list of the outputs needed for plots
    return(list(gbm = gbm,
                gbm_log = gbm_log,
                tsne_proj = analysis_results$tsne, # tSNE projects from analysis results
                clustering = analysis_results$clustering) # clustering from analysis results
    )

  })

  # when input directory is selected updates gene symbols name for selection
  observeEvent(!is.null(outs()), {

    updateSelectizeInput(session = session,
                         inputId = "gene_symbol",
                         label = "Select Gene Symbols",
                         choices = fData(outs()[["gbm_log"]])$symbol)

  })


  # calling Modular functions
  callModule(module = UMItSNEPlotServer,
             id = "tSNE",
             outs = outs)

  callModule(module = ClusterExplore10xServer,
             id = "cluster_explore",
             outs = outs)

  callModule(module = IdentifytSNEServer,
             id = "seurat_out",
             obj = seurat_obj)

  # callModule(module = MorpheusServer,
  #            id = "morpheus_out",
  #            outs = outs)


}


shinyApp(ui = ui, server = server)
