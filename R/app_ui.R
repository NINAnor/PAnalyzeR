#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import giscoR
#' @import sf
#' @import leaflet
#' @import leafem
#' @import mapedit
#' @import shinyFiles
#' @import dplyr
#' @import DT
#' @import shinycssloaders
#' @import rgee
#' @import factoextra
#' @import missForest
#' @import mapview
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      h1("PAnalyzeR"),
      selectInput("country", "Choose one or several countries", c("NO","FR","SVK"), selected = NULL, multiple = TRUE,
                  selectize = TRUE, width = NULL),
      uiOutput("conf_cntry"),
      selectModUI("NUTS3_map"),
      actionButton("confirm2","confirm"),
      uiOutput("com_tab"),
      uiOutput("fin_tab"),
      leafletOutput("clus_map")


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
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PAnalyzeR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
