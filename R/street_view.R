library(config)
library(googleway)

get_street_view <- function(
    latitude,
    longitude, 
    heading, 
    filepath,
    fov = 90, 
    pitch = 5){
  # Get API key from config file
  key <- config::get("key")
  
  # Get streetview images from Google
  # Latitude and longitude can be obtained from Google URL
  # Use the compass mark to estimate a heading value
  # A heading of 0 / 360 = North; 180 = South; 90 = East; 270 = West
  # FOV notes the zoom level
  # TODO Convert to function
  if (!file.exists(filepath)){
    url <- google_streetview(location = c(latitude,longitude),
                             size = c(400,400),
                             panorama_id = NULL,
                             output = "html",
                             heading = heading,
                             fov = fov,
                             pitch =pitch,
                             response_check = FALSE,
                             key = key)
    
    download.file(url, filepath, model="wb")  
  }
  
}
