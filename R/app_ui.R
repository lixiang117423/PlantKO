#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    tagList(
      tags$style(
        "h1 {
        color: #008000;
        }"
      ),
      nav_(
        h1("PlantKO"),
        c(
          "home" = "Home",
          "view" = "View",
          "go" = "GO",
          "kegg" = "KEGG",
          "gsea" = "GSEA",
          "about" = "About"
        )
      ),
      tags$div(
        class = "container",

        # homepage
        fluidRow(
          id = "home",
          mod_home_ui("home")
        ) %>%
          tagAppendAttributes(
            style = "display::nline-block"
          ),

        # view page
        fluidRow(
          id = "view",
          mod_View_ui("view")
        ) %>%
          tagAppendAttributes(
            styles = "display::nline-block"
          )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    system.file("app/www", package = "PlantKO")
  )

  tags$head(
    tags$head(
      golem::activate_js(),
      golem::favicon(),
      tags$title("PlantKO"),
      # Add here all the external resources
      # If you have a custom.css in the inst/app/www
      # Or for example, you can add shinyalert::useShinyalert() here
      tags$link(
        rel="stylesheet",
        type="text/css",
        href="www/bootstrap.min.css",
        integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T",
        crossorigin="anonymous"
      ),
      tags$script(
        src="www/bootstrap.min.css",
        integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM",
        crossorigin="anonymous"
      ),
      tags$link(
        rel="stylesheet",
        type="text/css",
        href="www/custom.css"
      ),
      tags$script(src="www/script.js")
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
