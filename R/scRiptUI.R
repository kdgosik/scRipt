#' scRiptUI
#'
#' Launch the single cell R interactive playground tool user interface in local machine
#'
#' This function will automatically launch the scRipt user interface in a web browser.
#' The user interface
#'
#' @export
#' @import shiny
#' @author Kirk Gosik <kgosik@broadinstitute.org>
#' @examples
#' \dontrun{
#'    scRipt()
#' }


scRiptUI <- function() {
  shiny::runApp(system.file("shiny", package="scRipt"))
}
