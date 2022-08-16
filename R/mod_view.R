#' View UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT renderDataTable datatable
#' @importFrom dplyr select mutate
mod_View_ui <- function(id) {
  ns <- NS(id)
  tagList(
    col_12(
      DT::dataTableOutput(ns("view")) %>%
        tags$div(style = "height::400px")
    )
  )
}

#' View Server Functions
#'
#' @noRd
mod_View_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    load("./data/meta.table.rda")
    output$view <- DT::renderDataTable({
      DT::datatable(meta.table %>%
        dplyr::select(
          Plant_latin,
          Plant_Chinese,
          Order,
          Family,
          Version
        ) %>%
        dplyr::mutate(Version = as.character(Version)),
      options = list(
        lengthMenu = c(10, 30, 50),
        pageLength = 10
      )
      )
    })
  })
}

## To be copied in the UI
# mod_View_ui("View_1")

## To be copied in the server
# mod_View_server("View_1")
