library(tidyverse)
library(sf)
library(ggmap)
library(osmdata)

setwd("C:\\Users\\think\\Downloads\\R_S8_2024\\R_S8_2024")

assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

# OSM DATA LIBRARY ######

features<-available_features()
amenity<-available_tags("amenity")

# 1. Create a query about bars in the city of Barcelona ######

query <- getbb("Tbilisi, Georgia") %>% # get a bounding box
  opq() %>% #Build an Overpass query
  add_osm_feature("amenity", "bar")

# 2. Convert your query into a sf object ######
bcn_bars <- osmdata_sf(query)

# 3. Extract data from List #####
bcn_bars_coords <- bcn_bars[["osm_points"]] |> 
  select(1, 2, "geometry")

# 4 Get the bbox of data an download a map tile #######

bbox <- st_bbox(bcn_bars_coords)

left<-(as.numeric(bbox[1])-mean(as.numeric(bbox[c(1,3)])))* 2/4+mean(as.numeric(bbox[c(1,3)]))
right <- (as.numeric(bbox[3]) - mean(as.numeric(bbox[c(1, 3)]))) * 2/4 + mean(as.numeric(bbox[c(1, 3)]))
bottom<-(as.numeric(bbox[2])-mean(as.numeric(bbox[c(2,4)])))*1+mean(as.numeric(bbox[c(2,4)]))
top<-(as.numeric(bbox[4])-mean(as.numeric(bbox[c(2,4)])))** 2/4+mean(as.numeric(bbox[c(2,4)]))

register_stadiamaps(key = "76cf97d4-feb6-4a64-bbdc-fd35e0ea4ce4")

BCN_TILE<-get_stadiamap(bbox = c(left = left, 
                                 bottom = bottom, 
                                 right = right,
                                 top = top ), 
                        zoom = 14,  
                        maptype = c("stamen_terrain"), 
                        crop = TRUE,
                        messaging = FALSE)

BCN_TILE <- ggmap(BCN_TILE)
BCN_TILE 

# 5. Create the main map #######
library(sf)
library(ggplot2)

# Assuming you have a single point as an sf object
#your_data <- st_point(c(44.7893, 41.70723))
#your_data <- st_sf(geometry = your_data)

# Assuming BCN_TILE and bcn_bars_coords are already defined

main_map <- BCN_TILE +
  geom_sf(data = bcn_bars_coords,
          colour = "#C52301",
          alpha = 0.75,
          size = 1,
          inherit.aes = FALSE) +
  geom_sf(data = your_data, aes(color = "yellow", size = 2.5), inherit.aes = FALSE) +
  
  labs(x = "", y = "") +
  theme_void() +
  theme(legend.position = "none",
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 2))

ggsave("levanGeorgia_bars.png", plot = main_map, scale = 1, height = 12, width = 12, dpi = 300)


# 6. Add a rectangle in the area you want to zoom in #######
main_map2 <- main_map +
  geom_rect(xmin = 44.7768, ymin = 41.7020,
            xmax = 44.7986,ymax = 41.7112,
            fill = NA, colour = "black", linewidth =1)

main_map2
ggsave("Geo_bar_H2so4.png", scale = 1,height = 12,width=12, dpi = 300)

# 7. Download a map tile for the zoom-in area #######

zoom_area<-get_stadiamap(bbox = c(left = 44.7768, 
                                  bottom =  41.7020,
                                  right = 44.7986,
                                  top = 41.7112), 
                         zoom = 17,  
                         maptype = c("stamen_toner_lite"), 
                         crop = TRUE, 
                         messaging = FALSE)


zoom_area <- ggmap(zoom_area)
zoom_area

# 8. Create the zoom-in map #######
# Assuming BCN_TILE and bcn_bars_coords are already defined

# Create an sf object for a single point
your_data <- st_as_sf(data.frame(longitude = 44.7893, latitude = 41.70723), coords = c("longitude", "latitude"))

# Set the CRS for your_data to WGS84 (EPSG:4326)
your_data <- st_set_crs(your_data, 4326)

# Check the CRS of your_data
st_crs(your_data)

main_map_zoom <- zoom_area +
  geom_sf(data = bcn_bars_coords,
          aes(color = "#C52301"),
          alpha = 1, 
          size = 2.5, 
          inherit.aes = FALSE) +
  geom_sf(data = your_data, aes(color = "blue", size = 2), inherit.aes = FALSE) +
  labs(x = "", y = "") +
  theme_void() +
  theme(legend.position = "none",
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 2))

# Print the map
print(main_map_zoom)

ggsave("Tbilisi111.png", scale = 1,height = 12,width=12, dpi = 300)






# 10.Create an arranged version of the map. ######

# Define custom color palettes
main_map_palette <- c("#4E79A7", "#E15759", "#76B7B2", "#F28E2B", "#59A14F")
zoom_map_palette <- c("#C52301", "blue")

final_map <- main_map2 +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(lineheight = 1, size = 40, face = "bold"),
        plot.caption = element_text(lineheight = 1, size = 32))

main_map_zoom2 <- zoom_area +
  geom_sf(data = bcn_bars_coords,
          colour = zoom_map_palette[1],
          alpha = 1, 
          size = 6.5, 
          inherit.aes = FALSE) +
  geom_sf(data = your_data, color = zoom_map_palette[2], size = 15, inherit.aes = FALSE) +
  labs(x = "", y = "") +
  theme_void() +
  labs(title = "The Bars in Tbilisi",
       caption = "Levan Dalbashvili") +
  theme(legend.position = "none",
        plot.title = element_text(lineheight = 1, size = 20, face = "bold"),
        plot.caption = element_text(lineheight = 1, size = 15),
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 2))

# Combine plots using patchwork instead of gridExtra
combined_plot <- final_map / main_map_zoom2 +
  plot_layout(heights = c(1.4, 2))  # Adjust as needed

# Save the plot
ggsave("bars_tbilisi.png", plot = combined_plot, scale = 1, height = 16, width = 12, dpi = 300)


#### task 2:

library(tidygeocoder)

# List of addresses
poi_names <- c("Harbour Space, Campus Bangkok",
               "Wat Traimit Withayaram Worawihan (Golden Buddha)",
               "Wat Arun Ratchawararam Ratchawaramahawihan",
               "Saket Temple (The Golden Mount)",
               "Wat Prayurawongsawat Worawihan")

poi_addr <- c("126/1 Vibhavadi Rangsit Rd, Khwaeng Din Daeng, Din Daeng, Bangkok 10400", 
              "661 Charoen Krung Road, Talat Noi, Samphanthawong, Bangkok 10100",
              "158 Thanon Wang Doem, Wat Arun, Bangkok Yai, Bangkok 10600",
              "344 Thanon Chakkraphatdi Phong, Ban Bat, Pom Prap Sattru Phai, Bangkok 10100",
              "24 Prajadhipok Rd, Wat Kanlaya, Thon Buri, Bangkok 10600")

lats <- c(13.77897, 
          13.737947190616058,
          13.744104884311124,
          13.754082531692925,
          13.737571303271741)

lons <- c(100.56032,
          100.51344376790456,
          100.48846545441278,
          100.50657808324883,
          100.49548865441257)

# Create a data frame with the addresses
poi_df <- data.frame(name=poi_names, address = poi_addr,
                     lat=lats, lon=lons)

poi_df 

# GET ROUTES  #######
library(osrm)


myhome<- c(13.77897, 100.5603)

bcn_bars_coords2<-poi_df|>
  mutate(lat_o=myhome[1],
         long_o=myhome[2])

# 4. Split your dataframe into a list object ######

names(poi_df)
listbar <- list(bcn_bars_coords2[2, 1:6],
                bcn_bars_coords2[3, 1:6],
                bcn_bars_coords2[4, 1:6],
                bcn_bars_coords2[5, 1:6])

# 5. Get the route by foot from my house to the 10 bars ######
foot_routes <- lapply(listbar, function(df) {
  route = osrmRoute(
    src = c(as.numeric(df$long_o), as.numeric(df$lat_o)) ,
    dst = c(as.numeric(df$lon), as.numeric(df$lat)),
    osrm.profile = "foot"
  )
  route
})

# 5. Get the route by foot from my house to the 10 bars ######
car_routes <- lapply(listbar, function(df) {
  route = osrmRoute(
    src = c(as.numeric(df$long_o), as.numeric(df$lat_o)) ,
    dst = c(as.numeric(df$lon), as.numeric(df$lat)),
    osrm.profile = "car"
  )
  route
})


# 6. Extract elemente from the list and row bind the into a data frame ######
foot_routes <- do.call("rbind", foot_routes)
car_routes <- do.call("rbind", car_routes)

foot_routes$destination <- poi_names[2:5]
car_routes$destination <- poi_names[2:5]



# 7. Plot the routes over a map tile #####
car_routes$id <- as.factor(c(1:4))
foot_routes$id <- as.factor(c(1:4))

car_routes$type <- "car"
foot_routes$type <- "foot"

# Create a combined dataset for plotting
combined_data <- rbind(
  foot_routes %>% mutate(type = "foot"),
  car_routes %>% mutate(type = "car")
)

# Plotting
bbox <- st_bbox(combined_data)

left <- (as.numeric(bbox[1]) - mean(as.numeric(bbox[c(1, 3)]))) * 1 + mean(as.numeric(bbox[c(1, 3)]) - 0.0025)
right <- (as.numeric(bbox[3]) - mean(as.numeric(bbox[c(1, 3)]))) * 1 + mean(as.numeric(bbox[c(1, 3)]) + 0.005)
bottom <- (as.numeric(bbox[2]) - mean(as.numeric(bbox[c(2, 4)]))) * 1 + mean(as.numeric(bbox[c(2, 4)]) - 0.005)
top <- (as.numeric(bbox[4]) - mean(as.numeric(bbox[c(2, 4)]))) * 1 + mean(as.numeric(bbox[c(2, 4)]) + 0.005)

register_stadiamaps(key = "76cf97d4-feb6-4a64-bbdc-fd35e0ea4ce4")

BKK_TILE <- get_stadiamap(
  bbox = c(left = left, bottom = bottom, right = right, top = top),
  zoom = 14,
  maptype = c("stamen_toner_background"),
  crop = TRUE,
  messaging = FALSE
)

BKK_TILE <- ggmap(BKK_TILE)

ggplot_obj <- BKK_TILE +
  geom_sf(data = combined_data, aes(colour = destination), alpha = 1, linewidth = 1.6, inherit.aes = FALSE) +
  facet_grid(type ~ destination, scales = "free_y") +
  theme_light() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "black"),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(size = 14, face = "bold")
  )

# Print the ggplot object
print(ggplot_obj)

# Save the plot
ggsave("HS_to_temple.png", plot = ggplot_obj, scale = 1, height = 10, width = 20, dpi = 300)


