#' UMItSNEPlotUI
#'
#' Renders tSNE or multiple tSNE plots for gene expression exploration
#'
#' @seealso shiny
#' @export
#' @param id The namespace for the module
#' @keywords shiny
#' @import ggplot2
#' @import plotly
#' @import cellranger
#' @importFrom shiny div
#' @importFrom shiny NS
#' @importFrom shiny tagList
#' @importFrom shiny sliderInput
#' @importFrom shiny selectizeInput
#' @importFrom shiny verbatimTextOutput
#' @author Kirk Gosik <kgosik@broadinstitute.org>

# if cellranger R kit is not installed then install it
if( !{"cellrangerRkit" %in% installed.packages()} ) {
  source("http://cf.10xgenomics.com/supp/cell-exp/rkit-install-2.0.0.R")
}
require(cellrangerRkit)

# MODULE UI
UMItSNEPlotUI <- function(id) {
  ns <- NS(id)

  ## Ui Outputs Here from server below
  shiny::tagList(

    div(

      shiny::sliderInput(inputId = ns("plot_limits"),
                         label = "Value Limits",
                         min = 0, max = 10,
                         value = c(0, 4), step = 0.5)

      ), # div

    div(

      shiny::selectizeInput(inputId = ns("gene_symbol"),
                     label = "Select Gene Symbols",
                     choices = "",
                     multiple = TRUE)

    ),

    div(

        plotly::plotlyOutput(outputId = ns("plot1"))

        ), # div

    div(

        shiny::verbatimTextOutput(outputId = ns("transform"))

        ) # div

    ) # tagList

} # UMItSNEPlotUI


#' UMItSNEPlotServer
#'
#' Renders tSNE or PCA plot from a Seurat object
#'
#' @seealso shiny
#' @export
#' @param input List-like object that stores the current values of all of the widgets in your app.
#' @param output List-like object that stores instructions for building the R objects in your app.
#' @param session List-like object about the session to be passed to the UI.
#' @param outs a reactive object from the cellranger toolkit
#' @keywords shiny
#' @import ggplot2
#' @improt plotly
#' @import dplyr
#' @importFrom shiny reactive
#' @importFrom shiny renderPrint
#' @importFrom shiny observeEvent
#' @author Kirk Gosik <kgosik@broadinstitute.org>


# MODULE Server
UMItSNEPlotServer <- function(input, output, session, outs) {

  # outputs plotly version of tSNE plot
  output$plot1 <- plotly::renderPlotly({

    # if not genes are provide, displays total counts
    if( is.null(input$gene_symbol) ){

      cellrangerRkit::visualize_umi_counts(gbm = outs()[["gbm"]],
                                           projection = outs()[["tsne_proj"]][c("TSNE.1", "TSNE.2")],
                                           limits = input$plot_limits)

    }else{

      # display plot by genes provided
      cellrangerRkit::visualize_gene_markers(gbm = outs()[["gbm_log"]],
                                             gene_probes = input$gene_symbol,
                                             projection = outs()[["tsne_proj"]][c("TSNE.1", "TSNE.2")],
                                             limits = input$plot_limits)

    }

  })

  # printing out the number of non-zero results
  output$transform <- renderPrint({

    paste("After transformation, the gene-barcode matrix contains",
          dim(outs()[["gbm_log"]])[1], "genes for",
          dim(outs()[["gbm_log"]])[2], "cells")

  })


  # when input directory is selected updates gene symbols name for selection
  observeEvent(!is.null(outs()), {

    updateSelectizeInput(session = session,
                         inputId = "gene_symbol",
                         label = "Select Gene Symbols",
                         choices = fData(outs()[["gbm_log"]])$symbol)

  })


} # UMItSNEPlotServer
