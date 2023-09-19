#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  ee_Initialize()
  ## create confirm country button
  output$conf_cntry<-renderUI({
    validate(
      need(input$country, 'Please select one or more countries')
    )
    actionButton("confirm1", "confirm")

  })

  ## load NUTS 3 based on input country
  nuts3<-eventReactive(input$confirm1,{
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

  })

  map_nuts3<-eventReactive(input$confirm1,{
    req(nuts3)
    nuts3<-nuts3()
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
  comm<-eventReactive(input$confirm2,{
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
    # print(nrow(comm))

  })

  observeEvent(input$confirm2,{
    comm<-comm()
    print(nrow(comm))
    nuts3<-nuts3()
    print(nrow(nuts3))
    nuts3_df<-nuts3%>%st_drop_geometry()%>%select(NUTS_ID,URBN_TYPE,MOUNT_TYPE,COAST_TYPE,NUTS_NAME)
    output$com_tab<-renderUI(
      tagList(
      withSpinner(DTOutput("DT_com")),
      actionButton('down_pa', label='download PA info')
      )

    )

    com_tab<-left_join(comm, nuts3_df, by = join_by("NUTS_CODE"=="NUTS_ID"))%>%st_drop_geometry()%>%select(COMM_ID,CNTR_CODE,COMM_NAME,NUTS_CODE,NUTS_NAME,URBN_TYPE,MOUNT_TYPE,COAST_TYPE)
    output$DT_com<-renderDT(com_tab)
  })

  comm_all<-eventReactive(input$down_pa,{
    comm<-comm()
    print("----- insert last part-------")
    gee_comm<-comm%>%select(FID)
    gee_comm<-sf_as_ee(gee_comm)

    ## combined mean max min reducer
    reducers <- ee$Reducer$mean()$combine(
      reducer2= ee$Reducer$max(),
      sharedInputs= TRUE
    )$combine(
      reducer2= ee$Reducer$min(),
      sharedInputs= TRUE
    )
    calc_scale<-1000

    #calc mean and stdv height
    DEM <- ee$ImageCollection("COPERNICUS/DEM/GLO30")$select("DEM")
    DEM = DEM$mosaic()
    zone_stats_dem <- DEM$reduceRegions(collection=gee_comm, reducer=reducers,scale = calc_scale)

    zone_stats_dem <- ee_as_sf(zone_stats_dem,via = "getInfo")
    a<-left_join(comm%>%st_drop_geometry(),zone_stats_dem%>%st_drop_geometry(),by="FID")

    names(a)[c((ncol(a)-2),(ncol(a)-1),ncol(a))] <- c("elev_max","elev_mean","elev_min")
    print("---DEM---")
    ## slope
    DEM <- ee$ImageCollection("COPERNICUS/DEM/GLO30")$select("DEM")
    proj = DEM$first()$select(0)$projection()
    slope<-ee$Terrain$slope(DEM$mosaic()
                            $setDefaultProjection(proj))

    zone_stats_slope <- slope$reduceRegions(collection=gee_comm, reducer=reducers,scale = calc_scale)
    a<-left_join(a,ee_as_sf(zone_stats_slope,via = "getInfo")%>%st_drop_geometry(),by="FID")
    names(a)[c((ncol(a)-2),(ncol(a)-1),ncol(a))]<- c('slope_max',"slope_mean","slope_min")
    print("---slope---")

    ## biomass abg already 300m res
    biom_abg <- ee$Image("WCMC/biomass_carbon_density/v1_0/2010")$select("carbon_tonnes_per_ha")
    # biom_abg = biom_abg$mean()
    zone_stats_biom <- biom_abg$reduceRegions(collection=gee_comm, reducer=reducers,scale = calc_scale)
    a<-left_join(a,ee_as_sf(zone_stats_biom,via = "getInfo")%>%st_drop_geometry(),by="FID")
    names(a)[c((ncol(a)-2),(ncol(a)-1),ncol(a))] <- c('abg_co_tha_max',"abg_co_tha_mean","abg_co_tha_min")
    print("---biom---")


    ## population
    pop <- ee$ImageCollection("WorldPop/GP/100m/pop")$select("population")
    pop = pop$mosaic()
    zone_stats_pop <- pop$reduceRegions(collection=gee_comm, reducer=ee$Reducer$mean(),scale = calc_scale)
    a<-left_join(a,ee_as_sf(zone_stats_pop,via = "getInfo")%>%st_drop_geometry(),by="FID")
    names(a)[ncol(a)] <- c("pop_ha")
    print("---pop---")


    ## accessibility
    acc = ee$Image('Oxford/MAP/accessibility_to_cities_2015_v1_0')$select('accessibility')
    zone_stats_acc <- acc$reduceRegions(collection=gee_comm, reducer=ee$Reducer$mean(),scale = calc_scale)
    a<-left_join(a,ee_as_sf(zone_stats_acc,via = "getInfo")%>%st_drop_geometry(),by="FID")
    names(a)[ncol(a)] <- c("min_travTime_cent")
    print("---acc---")

    ### LULC group field area % per community and lulc
    # lulc <- ee$Image("COPERNICUS/CORINE/V20/100m/2018")

    ## ev just for broad classes:artificial, agricultural, seminatural forest, natural forest, wetland, water
    # zone_stats_lulc <- lulc$reduceRegions(collection=gee_comm, reducer=ee$Reducer$frequencyHistogram())
    # a<-ee_as_sf(zone_stats_lulc,via = "getInfo")

    ## overlay PA
    #select polygons that are in input cntr
    # PA <- ee$FeatureCollection('WCMC/WDPA/current/polygons')$filter('PARENT_ISO == "NOR"')
    # PA_rast <- PA$reduceToImage(
    #   properties = list('GIS_AREA'),
    #   reducer = ee$Reducer$first()
    # )
    # PA_rast_repr<-PA_rast$reproject('EPSG:4326', NULL, 250)
    #
    # PA_recl<-ee$Image(0)$where(PA_rast_repr$gt(0),1)$select("constant")

    # zone_stats_pa <- PA_recl$reduceRegions(collection=gee_comm, reducer=ee$Reducer$frequencyHistogram(),scale=250)
    print("------start clustering---------")
    z <- a%>%select(elev_mean,elev_max,elev_min,slope_mean,slope_max,abg_co_tha_mean,abg_co_tha_max,min_travTime_cent,pop_ha)
    print("sel done")
    z <- missForest(z)$ximp
    print("imputation done")
    means <- apply(z,2,mean,na.rm=T)
    sds <- apply(z,2,sd,na.rm=T)
    nor <- scale(z,center=means,scale=sds)
    print("prep done")

    # k means (predefined n clust)
    # library(factoextra)
    k3 <- kmeans(nor, centers = 3, nstart = 25)
    print("clus done")
    comm$comm_clust<-unlist(k3[1])

    print("assign clust done")

    comm_all<-comm
  })


  observeEvent(input$down_pa,{
    req(comm_all)
    comm_all<-comm_all()
    ## table
    # output$fin_tab<-renderUI(
    #   withSpinner(DTOutput("DT_com_fin"))
    # )
    # output$DT_com_fin<-renderDT(comm_all)
    ## map
    pal <- colorNumeric(c("red", "green", "blue"), domain = comm_all$comm_clust)

    map_fin<-leaflet(comm_all)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.3,
                  fillColor = ~pal(comm_all$comm_clust),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
    output$clus_map<-renderLeaflet(map_fin)

  })



}
