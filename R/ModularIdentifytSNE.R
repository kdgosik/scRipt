## require()  for required libraries for module
library(ggplot2)
library(plotly)
library(dplyr)
library(Seurat)
library(shinycssloaders)

# MODULE UI
IdentifytSNEUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  tagList(
    
    div(
      shiny::selectInput(ns("reduct"), "Reduction Type", choices = c("tsne", "pca")),
      withSpinner(plotly::plotlyOutput(ns("plot1")))
        ), # div
    
    div(
      shiny::verbatimTextOutput(ns("transform"))
        ) # div
    
    ) # tagList

}



# MODULE Server
IdentifytSNEServer <- function(input, output, session, obj) {
  
  ## Place server code here to be called by callModule
    ## place whatever inputs needed in function call
  
  # outputs plotly version of tSNE plot
  output$plot1 <- plotly::renderPlotly({
    
   DimPlot(object = obj(), reduction.use = input$reduct)$plot
    
  })
  
  # printing out the number of non-zero results
  output$transform <- renderPrint({
    
    d <- event_data("plotly_selected")
    
    if (is.null(d)) {
      
      "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" 
      
      }else {
    
        d %>%
          dplyr::rename(tSNE_1 = x, tSNE_2 = y) %>%
          left_join({ 
            obj()@dr$tsne@cell.embeddings %>%
              data.frame(., cell = rownames(.), stringsAsFactors = FALSE) 
            }) %>%
          .$cell
          
      }
    
  })
  
}
