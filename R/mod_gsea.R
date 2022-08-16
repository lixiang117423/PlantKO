
#' gsea UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gsea_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' gsea Server Functions
#'
#' @noRd
mod_gsea_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_gsea_ui("gsea_1")

## To be copied in the server
# mod_gsea_server("gsea_1")
