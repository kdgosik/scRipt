#' IdentifytSNEUI
#'
#' Renders tSNE or PCA plot from a seurat object
#'
#' @seealso shiny
#' @export
#' @param id The namespace for the module
#' @keywords shiny
#' @import ggplot2
#' @import plotly
#' @import Seurat
#' @import dplyr
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny div
#' @importFrom shiny NS
#' @importFrom shiny selectInput
#' @importFrom shiny verbatimTextOutput
#' @author Kirk Gosik <kgosik@broadinstitute.org>


# MODULE UI
IdentifytSNEUI <- function(id) {
  ns <- NS(id)

  ## Ui Outputs Here from server below
  tagList(

    div(

      ## selection of using tSNE componenets or PCA componenets
      shiny::selectInput(inputId = ns("reduct"),
                         label = "Reduction Type",
                         choices = c("tsne", "pca")),

      ## css spinner loader on top of plotly output
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = ns("plot1")))

        ), # div

    div(

      ## printing output letting user know about the transformation performed on data
      shiny::verbatimTextOutput(outputId = ns("transform"))

        ) # div

    ) # tagList

} # IdentifytSNEUI


#' IdentifytSNEServer
#'
#' Renders tSNE or PCA plot from a Seurat object
#'
#' @seealso shiny
#' @export
#' @param input List-like object that stores the current values of all of the widgets in your app.
#' @param output List-like object that stores instructions for building the R objects in your app.
#' @param session List-like object about the session to be passed to the UI.
#' @param obj a reactive Seurat object
#' @keywords shiny
#' @import ggplot2
#' @improt plotly
#' @import Seurat
#' @import dplyr
#' @importFrom shiny renderPrint
#' @author Kirk Gosik <kgosik@broadinstitute.org>

# MODULE Server
IdentifytSNEServer <- function(input, output, session, obj) {

  # outputs plotly version of tSNE or PCA plot
  output$plot1 <- plotly::renderPlotly({

   Seurat::DimPlot(object = obj(), reduction.use = input$reduct)$plot

  })

  # printing out the number of non-zero results
  output$transform <- renderPrint({
    ## event data from the plotly selection.  still needs work bc it prints NAs
    d <- plotly::event_data("plotly_selected")

    if (is.null(d)) {

      "Click and drag events (i.e., select/lasso) appear here (double-click to clear)"

      }else {
## somehow substring x and y down to 6 decimals places as a character
        join_data <- data.frame( obj()@dr$tsne@cell.embeddings, stringsAsFactors = FALSE ) %>%
          mutate(cell = rownames(.), x = as.character(round(tSNE_1, 6)), y = as.character(round(tSNE_2, 6)))

        out <- d %>%
          mutate(x = as.character(x), y = as.character(y)) %>%
          dplyr::left_join( join_data )

        list(d, join_data[1:5,], out, join_data[join_data$x %in% d$x & join_data$y %in% d$y,])



      }

  })

} # IdentifytSNEServer
