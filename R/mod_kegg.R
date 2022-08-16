#' kegg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_kegg_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' kegg Server Functions
#'
#' @noRd 
mod_kegg_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_kegg_ui("kegg_1")
    
## To be copied in the server
# mod_kegg_server("kegg_1")
