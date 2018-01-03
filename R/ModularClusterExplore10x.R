#' ClusterExplore10xUI
#'
#' Runs and visulizes the 10X clustering from the Cell Ranger website.  This function is to render
#' the UI for the shiny module.
#'
#' The module allows for the input of different parameters such as the number of clusters and limits
#' for the number of genes or the expression limits in the heatmap.
#'
#' @seealso shiny
#' @export
#' @param id The namespace for the module
#' @keywords shiny
#' @import cellranger
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny div
#' @importFrom shiny tagList
#' @importFrom shiny NS
#' @importFrom shiny sliderInput
#' @importFrom shiny plotOutput
#' @importFrom shiny numericInput
#' @author Kirk Gosik <kgosik@broadinstitute.org>

# if cellranger R kit is not installed then install it
if( !{"cellrangerRkit" %in% installed.packages()} ) {
  source("http://cf.10xgenomics.com/supp/cell-exp/rkit-install-2.0.0.R")
}
require(cellrangerRkit)


# MODULE UI
ClusterExplore10xUI <- function(id) {
  ns <- NS(id)

  ## Ui Outputs Here from server below
  tagList(

    div(

      ## numeric input for the number of clusters to include
      shiny::sliderInput(inputId = ns("num_clusters"),
                         label = "Number of Clusters",
                         min = 2, max = 10,
                         value = 5, step = 1)

      ), # div

    div(

      ## css spinner on top of ploting the scatter plot of the clusters
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = ns("cluster_plot")))

      ), # div

    div(

      ## numeric input for the number of genes to include
      shiny::numericInput(inputId = ns("n_genes"),
                          label = "Number of Genes",
                          value = 3,
                          min = 1, max = 20,
                          step = 1)

      ), # div

    div(

      ## slider choice of heatmap limits for gene expression
      shiny::sliderInput(inputId = ns("hm_limits"),
                         label = "Heatmap Limits",
                         min = -5, max = 5,
                         value = c(-1, 2), step = 0.5)

      ), # div

    div(

      ## plotting the heatmap
      shiny::plotOutput(outputId = ns("pheatmap"))

      ) # div

    ) # tagList

} # ClusterExplore10xUI



#' ClusterExplore10xServer
#'
#' Runs and visulizes the 10X clustering from the Cell Ranger website.  This function is to render
#' the Server for the shiny module.
#'
#' The module allows for the input of different parameters such as the number of clusters and limits
#' for the number of genes or the expression limits in the heatmap.
#'
#' @seealso shiny
#' @export
#' @param input List-like object that stores the current values of all of the widgets in your app.
#' @param output List-like object that stores instructions for building the R objects in your app.
#' @param session List-like object about the session to be passed to the UI.
#' @param outs a reactive object from the cellranger toolkit
#' @keywords shiny
#' @importFrom shiny reactive
#' @importFrom shiny validate
#' @importFrom shiny observe
#' @author Kirk Gosik <kgosik@broadinstitute.org>


ClusterExplore10xServer <- function(input, output, session, outs) {

    ## creates reactive element for the clusters that were made from the cell ranger pipeline
  cluster_result <- reactive({

    outs()[["clustering"]][[paste("kmeans", input$num_clusters, "clusters", sep = "_")]]

  })

    ## dynamically chooses cluster colors based off of the total number of clusters.
  example_col <- reactive({
    rev(brewer.pal(input$num_clusters, ifelse(input$num_clusters < 9, "Set2", "Set3"))) # customize plotting colors
  })

  output$cluster_plot <- renderPlot({

    visualize_clusters(cluster_result = cluster_result()$Cluster,
                       projection = outs()[["tsne_proj"]][c("TSNE.1","TSNE.2")],
                       colour = example_col())

  })

  output$pheatmap <- renderPlot({

    # sort the cells by the cluster labels
    cells_to_plot <- order_cell_by_clusters(outs()[["gbm"]], cluster_result()$Cluster)

    # order the genes from most up-regulated to most down-regulated in each cluster
    prioritized_genes <- prioritize_top_genes(outs()[["gbm"]], cluster_result()$Cluster, "sseq", min_mean=0.5)

      ## heatmap of gene - barcode matrix
    gbm_pheatmap(log_gene_bc_matrix(outs()[["gbm"]]),
                 genes_to_plot = prioritized_genes,
                 cells_to_plot = cells_to_plot,
                 n_genes = input$n_genes,
                 colour = example_col(),
                 limits = input$hm_limits)

  })



  #
  # ## Work into module
  # output_folder <-"/path_to_your_local_folder/pbmc_data_public/pbmc3k/gene_sets"
  # write_cluster_specific_genes(prioritized_genes, output_folder, n_genes=10)


  # create values and axis annotations for pheatmap

} # ClusterExplore10xServer
