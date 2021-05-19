library(tidyverse)
library(leaflet)
library(sf)

#   -------------------------------------------------------
# Custom function for legend  -----------------------------
# adapted from https://stackoverflow.com/a/37482936/2630957
#   ------------------------------------------------------- 

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", 
           sizes, "px; border:1px solid ", borders, 
           "; border-radius:", shapes)
  }
  
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, 
                   labels = legend_labels, 
                   opacity = opacity))
}


# Make the map ----------------------------------------------------------

# some data
library(tmap)
data(NLD_prov, NLD_muni)

# for the polygon background
prov <- st_transform(NLD_prov, 4326)

# reproject and get centroids 
# MULTIPOINT objects from sf are not supported at this time, so we need to 
# extract the coords
muni_centroids <- NLD_muni %>% 
  st_transform(4326) %>% 
  st_point_on_surface() %>% 
  st_coordinates()

# bind coords to muni and make it a data frame
muni <- NLD_muni %>% 
  st_drop_geometry() %>% 
  bind_cols(as.data.frame(muni_centroids)) %>%
  # add settings for legend, derived from data
  pivot_longer(starts_with("origin"), names_to = "origin", values_to = "counts") %>% 
  mutate(label = cut_width(counts, width = 20, center = 10), # equal width, label can be set here too
         size = as.numeric(label) * 3) 

leaflet() %>% 
  # Base map
  addProviderTiles("Esri.WorldGrayCanvas", group = "Esri.WorldGrayCanvas") %>% 
  addPolygons(data = prov, weight = 1) %>% 
  # Groups with bubbles
  addCircleMarkers(data = filter(muni, origin == "origin_native"), 
                   lng = ~X, lat = ~Y, radius = ~size, weight = 1, 
                   group = "origin_native") %>%
  addCircleMarkers(data = filter(muni, origin == "origin_west"), 
                   lng = ~X, lat = ~Y, radius = ~size, weight = 1, 
                   group = "origin_west") %>%
  addCircleMarkers(data = filter(muni, origin == "origin_non_west"), 
                   lng = ~X, lat = ~Y, radius = ~size, weight = 1, 
                   group = "origin_non_west") %>%
  # call the custom function
  addLegendCustom(colors = "white", labels = levels(muni$label), 
                  sizes = sort(unique(muni$size)), 
                  shapes = "circle", borders = "black") %>%
  # add the layers as radio buttons
  addLayersControl(
    baseGroups = c("origin_native", "origin_west", "origin_non_west"),
    options = layersControlOptions(collapsed = FALSE)
  )



