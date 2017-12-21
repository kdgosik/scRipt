#' ShinyServer
#'
#' Runs and visulizes the 10X clustering from the Cell Ranger website
#'
#' Runs and visulizes the 10X clustering from the Cell Ranger website
#'
#' @param id shiny id
#' @return shinyUI for module
#' @export
#' @import shiny shinyFiles cellranger
#' @author Kirk Gosik <kgosik@broadinstitute.org>
#' @examples


# if cellranger R kit is not installed then install it
if( !{"cellrangerRkit" %in% installed.packages()} ) {
  source("http://cf.10xgenomics.com/supp/cell-exp/rkit-install-2.0.0.R")
}
require(cellrangerRkit)

# source("../R/ModularUMItSNEPlot.R")  ## will move to main part of the package
# source("../R/ModularClusterExplore10x.R") ## will move to main part of the package
# source("../R/ModularIdentifytSNE.R")  ## will move to main part of the package


shinyServer(function(input, output, session) {
    ## supposed to dynamically update data sources but doesn't seem to
  reactive({

    updateSelectInput(session, "data_source", "Select Data Source",
                      choices = gsub(".rds","",dir("data", pattern = ".rds")))

  })

  # defines root directory for the user
  shinyDirChoose(input, 'file_path', roots = c(root = '/'))

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

      rmarkdown::render(input = "../R/Seurat_to_Markdown.Rmd",
                        params = list(project = project_name,
                                      path = path,
                                      cells = input$cells,
                                      genes = input$genes,
                                      max_mt = input$max_mt),
                        output_file = paste0("../output/", project_name, ".html"))

    })

  })

    ## reads in already created seurat object
  seurat_obj <- eventReactive(input$read_data, {

    withProgress(message = "Loading Data...", {

      readRDS(file = dir("../data", pattern = input$data_source, full.names = TRUE))

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
    gbm <- load_cellranger_matrix( selected_path )

    # normalize nonzero genes
    use_genes <- get_nonzero_genes(gbm)
    gbm_bcnorm <- normalize_barcode_sums_to_median(gbm[use_genes, ])
    gbm_log <- log_gene_bc_matrix(gbm_bcnorm, base = 10)

    # loads analysis results
    analysis_results <- load_cellranger_analysis_results( selected_path )

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


})
