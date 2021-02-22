## SP OBJECTS
seattle_sp <- seattle_sf %>%
  st_transform(., crs=proj) %>%
  as(., "Spatial")
seattle_Owin <- as.owin(seattle_sp)


no_police_action_pts_sf <- st_transform(no_police_action_pts_sf, crs=proj)
no_police_action_pts_coords <- matrix(unlist(no_police_action_pts_sf$geometry),
                                      ncol = 2,
                                      byrow =T)
no_police_action_pts_ppp <- ppp(x = no_police_action_pts_coords[,1],
                                y = no_police_action_pts_coords[,2], 
                                window = seattle_Owin, check = T)
no_police_action_pts_sp <- no_police_action_pts_sf %>%
  st_transform(., crs = proj) %>%
  as(., "Spatial")



## Density plot 1
#dens_no_police_action <- density(no_police_action_pts_ppp)
no_police_action_pts_ppp_jitter <- rjitter(no_police_action_pts_ppp,
                                           retry=TRUE,
                                           nsim=1,
                                           drop=T)

dens_no_police_action <- density.ppp(no_police_action_pts_ppp,
                                     sigma = bw.diggle(no_police_action_pts_ppp), 
                                     edge=T)

#plot(dens_no_police_action, main='Density of Cases Where No Police Action was Taken')

#-----------------------------------
## RASTER KERNEL DENSITY

## Raster Kernel Density
pixelsize = 100
box = round(extent(seattle_sp) / pixelsize) * pixelsize
template = raster(box, crs = proj,
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

no_police_action_pts_sp$PRESENT = 1
raster_no_pol_action = rasterize(no_police_action_pts_sp,
                                 template,
                                 field = 'PRESENT',
                                 fun = sum)
# plot(raster_no_pol_action)
# plot(seattle_sp, border='#00000040', add=T)

kernel = focalWeight(raster_no_pol_action, d = 264, type = 'Gauss')
heat = focal(raster_no_pol_action, kernel, fun = sum, na.rm=T)
threshold = 0.15
polygons = rasterToPolygons(x = heat, n=16, fun = function(x) { x >= threshold })
contours = gBuffer(gUnaryUnion(polygons), width=100)

## UNCOMMENT TO PLOT STATIC DENSITY MAP
# plot(heat, main='Kernel Density of Cases Where No Police Action was Taken')
# plot(seattle_sp, border="gray", add=T)

crs(heat) <- sp::CRS("+init=epsg:4326")
kernel_npapn_pal <- colorNumeric(
  palette = "YlOrRd",
  values(heat),
  na.color = "transparent"
)

leaflet() %>%
  addMapboxTiles(
    style_id = "dark-v9",
    username = "mapbox"
  ) %>%
  addRasterImage(heat, 
                 colors = ~kernel_npapn_pal, 
                 opacity = 0.8
  ) %>%
  addLegend(data = heat,
            pal = kernel_npapn_pal, 
            values = values(heat),
            title = "Density of NPAPN Resolutions<br>per square mile") %>% 
  setView(lng = -122.3321, lat = 47.6062, zoom = 11)


### Point Pattern Density
## RASTER KERNEL DENSITY
pixelsize = 100
box = round(extent(seattle_sp) / pixelsize) * pixelsize
template = raster(box, crs = proj,
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

no_police_action_pts_sp$PRESENT = 1
raster_no_pol_action = rasterize(no_police_action_pts_sp,
                                 template,
                                 field = 'PRESENT',
                                 fun = sum)
# plot(raster_no_pol_action)
# plot(seattle_sp, border='#00000040', add=T)

kernel = focalWeight(raster_no_pol_action, d = 264, type = 'Gauss')
heat = focal(raster_no_pol_action, kernel, fun = sum, na.rm=T)
threshold = 0.15
polygons = rasterToPolygons(x = heat, n=16, fun = function(x) { x >= threshold })
contours = gBuffer(gUnaryUnion(polygons), width=100)

## UNCOMMENT TO PLOT STATIC DENSITY MAP
# plot(heat, main='Kernel Density of Cases Where No Police Action was Taken')
# plot(seattle_sp, border="gray", add=T)

heat_sf <- heat %>% 
  st_as_stars(.) %>%
  st_as_sf(merge=T) %>%
  st_transform(., crs = wgs)

kernel_npapn_pal <- colorNumeric(
  palette = "YlOrRd",
  domain = heat_sf$layer,
  na.color = "transparent"
)

leaflet() %>%
  addMapboxTiles(
    style_id = "dark-v9",
    username = "mapbox"
  ) %>%
  addPolygons(data = heat_sf, 
              color = ~kernel_npapn_pal(layer), 
              stroke = F,
              fillOpacity = 0.5,
              smoothFactor = 0.5
  ) %>%
  addLegend(data = heat_sf,
            pal = kernel_npapn_pal, 
            values = ~layer,
            title = "Density of NPAPN Resolutions<br>per square mile") %>% 
  setView(lng = -122.3321, lat = 47.6062, zoom = 11)


#---------------
# Multitype plot
no_police_action_pts_sf_proj <- st_transform(no_police_action_pts_sf, crs=proj)
no_police_action_pts_coords_matrix <- matrix(unlist(no_police_action_pts_sf_proj$geometry),
                                             ncol = 2,
                                             byrow =T)
no_police_action_pts_ppp <- ppp(x = no_police_action_pts_coords_matrix[, 1],
                                y = no_police_action_pts_coords_matrix[, 2], 
                                window = seattle_Owin,
                                check = T)

six_focal_case_types_npapn <- c("DISTURBANCE",
                                "SUSPICIOUS PERSON",
                                "SUSPICIOUS CIRCUM.",
                                "MISCHIEF OR NUISANCE",
                                "CRISIS COMPLAINT",
                                "PROWLER",
                                "TRESPASS")
no_police_action_pts_case_type_sp <- no_police_action_pts_sf_proj %>%
  select(case_type_final_desc_first) %>% 
  filter(case_type_final_desc_first %in% six_focal_case_types_npapn) %>%
  mutate(case_type_final_desc_first = as.factor(case_type_final_desc_first)) %>%
  as(., "Spatial")

no_police_action_pts_case_type_ppp <- as(no_police_action_pts_case_type_sp, "ppp")
#plot(split(no_police_action_pts_case_type_ppp))

case_types_npapn_kfunction <- alltypes(no_police_action_pts_case_type_ppp,
                                       "Kcross",
                                       envelope = T)  #this takes awhile
plot(case_types_npapn_kfunction)