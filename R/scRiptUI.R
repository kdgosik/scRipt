#' scRiptUI
#'
#' Launch the single cell R interactive playground tool user interface for your local machine
#'
#' This function will automatically launch the scRipt user interface in a web browser.
#' The user interface will allow you to create static reports about your data.  It will also allow you
#' to interactively explore the data in more detail.
#'
#' @seealso shiny
#' @export
#' @import shiny
#' @author Kirk Gosik <kgosik@broadinstitute.org>
#' @examples
#' \dontrun{
#'    scRiptUI()
#' }


scRiptUI <- function() {

  shiny::shinyApp(ui = ui, server = server)

}
