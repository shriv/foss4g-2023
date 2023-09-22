
# custom osmplotr function
return_poche_map <-  function(
    bbox,
    map_title,
    bldg_colour = "white",
    map_bg = "black", 
    street_colour = "gray50") {
  
  
  # get data
  dat_B <- extract_osm_objects (key = "building", bbox = bbox)
  dat_H <- extract_osm_objects (key = "highway", bbox = bbox)
  
  # selected data 
  dat_H_main <- dat_H %>% 
    filter(highway %in% c("primary", "trunk", "primary_link"))
  
  dat_H_impt <- dat_H %>% 
    filter(highway %in% c("secondary", "tertiary"))
  
  dat_H_other <- dat_H %>% 
    filter(!highway %in% c("primary", "trunk", "primary_link", "secondary", "tertiary"))
  
  # make map
  map <- osm_basemap (bbox = bbox, bg = map_bg)
  map <- add_osm_objects (map, dat_B, col = bldg_colour)
  
  # Only output maps if the filtered street types exist
  if (nrow(dat_H_main) != 0){
    map <- add_osm_objects (map, dat_H_main, col = street_colour, size = 2)  
  }
  
  if (nrow(dat_H_impt) != 0){
    map <- add_osm_objects (map, dat_H_impt, col = street_colour, size = 1)  
  }
  
  map <- add_osm_objects (map, dat_H_other, col = street_colour)
  
  plot <- print_osm_map (map) + 
    labs(title = map_title) 
  
  return(plot)
}


# custom osmplotr function with parks, buildings and streets
return_osmplotr_map <-
  function(bbox,
           map_title,
           scale_increment = 250,
           dist_unit = "m",
           x_min_offset = 0.005) {
    # get data
    structs <- c ("highway", "building", "park", "landuse")
    structures <-
      osm_structures (structures = structs, col_scheme = "dark")
    structures$value [4] <- "recreation_ground"
    structures$cols[4] <- "#647864FF"
    
    # make map and add scalebar
    osmplotr_map <- make_osm_map(structures = structures, bbox = bbox)
    plot <- osmplotr_map$map  +
      labs(title = map_title) +
      scalebar(
        x.min = bbox[1] + x_min_offset,
        x.max = bbox[2],
        y.min = bbox[2],
        y.max = bbox[4],
        st.size = 4,
        st.bottom = FALSE,
        box.fill = c("yellow", "white"),
        st.color = "white",
        dist = scale_increment,
        dist_unit = dist_unit,
        transform = TRUE,
        model = 'WGS84'
      )
    
    return(plot)
  }
