
#Get path to GIS data
pathGIS<<- function(){
  wd<- getwd()
  #masterWorkingDirectory<- sessionInfo$masterWorkingDirectory
  
  app_dir <- normalizePath(file.path(wd, ".."), winslash = "/")
  app_dir<-paste0(app_dir,'//GIS//')
  print(app_dir)
}


load_geojson <- function(urls) {
  plan(multisession)  # Use multiple parallel sessions
  
  sf_list <- future_map(urls, function(url) {
    response <- GET(url)
    geojson <- content(response, "text")
    st_read(geojson, quiet = TRUE)
  })
  
 
  return(sf_list)
}

urls <- c(
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/MuleDeerCrucialRange/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/MuleDeerHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/MuleDeerSeasonalRange/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/DeerHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/AntelopeHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/AntelopeHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/BisonHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/BisonHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/ElkHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/ElkHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/MooseHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/MooseHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/BighornSheepHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/BighornSheepHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
"https://services.wygisc.org/HostGIS/rest/services/GeoHub/WGFDRegionWildlifeBiologists/MapServer/0/query?outFields=*&where=1%3D1&f=geojson",
"https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/WildlifeAdministrativeRegions/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

merge_polygons <- function(MuleDeerCrucialRange, MuleDeerHerdUnits, MuleDeerSeasonalRange,
                           DeerHuntAreas, AntelopeHerdUnits, AntelopeHuntAreas) {
  
  MuleDeerCrucialRange <- MuleDeerCrucialRange %>%
    dplyr::rename(id = RANGE) %>%
    dplyr::select(id, geometry) %>%
    dplyr::mutate(layer_name = "MuleDeerCrucialRange")
  
  MuleDeerHerdUnits <- MuleDeerHerdUnits %>%
    dplyr::rename(id = MD_HERDNAME) %>%
    dplyr::select(id, geometry) %>%
    dplyr::mutate(layer_name = "MuleDeerHerdUnits")
  
  MuleDeerSeasonalRange <- MuleDeerSeasonalRange %>%
    dplyr::rename(id = RANGE) %>%
    dplyr::select(id, geometry) %>%
    dplyr::mutate(layer_name = "MuleDeerSeasonalRange")
  
  DeerHuntAreas <- DeerHuntAreas %>%
    dplyr::rename(id = HUNTNAME) %>%
    dplyr::select(id, geometry) %>%
    dplyr::mutate(layer_name = "DeerHuntAreas")
  
  AntelopeHerdUnits <- AntelopeHerdUnits %>%
    dplyr::rename(id = HERDNAME) %>%
    dplyr::select(id, geometry) %>%
    dplyr::mutate(layer_name = "AntelopeHerdUnits")
  
  AntelopeHuntAreas <- AntelopeHuntAreas %>%
    dplyr::rename(id = HUNTNAME) %>%
    dplyr::select(id, geometry) %>%
    dplyr::mutate(layer_name = "AntelopeHuntAreas")
  
  merged_polygons <- rbind(MuleDeerCrucialRange, MuleDeerHerdUnits, MuleDeerSeasonalRange,
                           DeerHuntAreas, AntelopeHerdUnits, AntelopeHuntAreas)
  
  return(merged_polygons)
}





add_or_remove_layer <- function(layer_id, data, map_id, add = TRUE) {
  if (add) {
    mapdeck_update(map_id) %>%
      add_polygon(data = data, layer_id = layer_id, update_view = FALSE)
  } else {
    mapdeck_update(map_id) %>%
      clear_polygon(layer_id = layer_id)
  }
}
