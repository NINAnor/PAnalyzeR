#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  ## create confirm country button
  output$conf_cntry<-renderUI({
    validate(
      need(input$country, 'Please select one or more countries')
    )
    actionButton("confirm1", "confirm")

  })

  ## load NUTS 3 based on input country
  map_nuts3<-eventReactive(input$confirm1,{

    nuts3<-giscoR::gisco_get_nuts(
      year = "2016",
      epsg = "4326",
      cache = TRUE,
      update_cache = FALSE,
      cache_dir = NULL,
      verbose = FALSE,
      resolution = "60",
      spatialtype = "RG",
      country = input$country,
      nuts_id = NULL,
      nuts_level = "3"
    )
    # print(nrow(nuts3))
    map_nuts3<- leaflet() %>%
      addProviderTiles(provider= "CartoDB.Positron")%>%
      addFeatures(st_sf(nuts3), layerId = ~NUTS_ID)


  })

  # nuts3_sel<-mapedit::selectMap(map_nuts3)
  nuts3_sel <- callModule(module=selectMod,
                        leafmap=map_nuts3(),
                        id="NUTS3_map")


  # nuts3_sel<-st_sf(nuts3[as.numeric(map_nuts3_sel[which(map_nuts3_sel$selected==TRUE),"id"])])

  ## communities
  observeEvent(input$confirm2,{
    nuts3_sel<-nuts3_sel()
    print(nuts3_sel)
    comm<-giscoR::gisco_get_communes(year = "2016",
                                      epsg = "4326",
                                     cache = TRUE,
                                     update_cache = FALSE,
                                     cache_dir = NULL,
                                     verbose = FALSE,
                                     spatialtype = "RG",
                                     country = input$country
    )%>%filter(NUTS_CODE %in% nuts3_sel$id)
    print(nrow(comm))
  })

  wdpr_path<-shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('', 'txt'))


}
