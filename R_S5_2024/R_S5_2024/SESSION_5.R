
#### session 1: Digital cartography ####

# 1. load libraries, set working direction and data in R ####

library(tidyverse)
library(sf)

setwd("C:\\Users\\think\\OneDrive\\Desktop\\coding\\R\\R_S5_2024\\R_S5_2024")
pop_data_thai <- read.csv("pop_data_thai.csv")

# 2. Basic inspection of our data #####

colnames(pop_data_thai)
head(pop_data_thai)
sapply(pop_data_thai, class)

# VERY BASIC DATA MANIPULATION WITH DPLYR FOR ORDERING THE BARS

pop_data_thai <- pop_data_thai |>
  mutate(NAME_1 = fct_reorder(NAME_1, n, .desc = FALSE))

# 3. DIFFERENT IMPLEMENTATIONS OF A BAR PLOT ######
ggplot(pop_data_thai, 
       aes(x=NAME_1, y=n/1000000))+
  coord_flip()+
  expand_limits(y = 6)+
  geom_bar(fill="red",
           stat="identity", 
           linewidth=.25,
           color="black")+
  geom_text(aes(label = format(n,big.mark=","),hjust = -.2))+
  labs(title = "Thailand 2022",
       subtitle="Population by provinces",
       x="",
       y="\nPopulation (in millons)", 
       caption="\nSource: HARBOUR SPACE | DIGITAL CARTOGRAPHY. Data: National Statistical Office (NSO)")+
  theme_light()+
  theme(plot.title = element_text(lineheight=1, size=18, face="bold",hjust = 0.5), #set parameters of title
        plot.subtitle = element_text(lineheight=1, size=15, face="bold",hjust = 0.5), # set parameters of subtitle
        plot.caption = element_text(lineheight=1, size=13,hjust = 0.5),
        axis.text.x  = element_text(vjust=0.5, size=15), #set parameters of x axis text
        axis.title.x = element_text(vjust=0.5, size=15), # set y axis title 
        axis.text.y  = element_text(vjust=0.5, size=15))

ggsave("1_barplot_EDITED.png", 
       scale = 1,
       height = 18,
       width=12, 
       dpi = 300)


ggplot(pop_data_thai, 
       aes(x=NAME_1, y=n/1000000,fill=sub(" Region", "", region)))+
  coord_flip()+
  expand_limits(y = 6)+
  geom_bar(stat="identity", 
           linewidth=.25,
           color="black")+
  geom_text(aes(label = format(n,big.mark=","),hjust = -.25))+
  labs(title = "Thailand 2022",
       subtitle="Population by provinces",
       x="",
       y="\nPopulation (in millons)", 
       caption="\nSource: HARBOUR SPACE | DIGITAL CARTOGRAPHY. Data: National Statistical Office (NSO)")+
  theme_light()+
  theme(plot.title = element_text(lineheight=1, size=18, face="bold",hjust = 0.5), #set parameters of title
        plot.subtitle = element_text(lineheight=1, size=15, face="bold",hjust = 0.5),
        plot.caption = element_text(lineheight=1, size=13, hjust=0.5),
        axis.text.x  = element_text(vjust=0.5, size=15), 
        axis.title.x = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15),
        legend.title  = element_blank(),
        legend.position  = "top",
        legend.text = element_text(vjust=0.5, size=15))

ggsave("2_barplot_EDITED.png", 
       scale = 1,
       height = 18,
       width=12, 
       dpi = 300)

ggplot(pop_data_thai, 
       aes(x=NAME_1, y=n/1000000,fill=sub(" Region", "", region)))+
  coord_flip()+
  expand_limits(y = 6)+
  geom_bar(stat="identity", 
           linewidth=.25,
           color="black")+
  geom_text(aes(label = format(n,big.mark=","),hjust = -.25))+
  labs(title = "Thailand 2022",
       subtitle="Population by provinces",
       x="",
       y="\nPopulation (in millons)", 
       caption="\nSource: HARBOUR SPACE | DIGITAL CARTOGRAPHY. Data: National Statistical Office (NSO)")+
  facet_grid(rows = vars(region), scales = "free_y", switch = "y", space = "free_y") +
  theme_light()+
  theme(plot.title = element_text(lineheight=1, size=18, face="bold",hjust = 0.5), #set parameters of title
        plot.subtitle = element_text(lineheight=1, size=15, face="bold",hjust = 0.5),
        plot.caption = element_text(lineheight=1, size=13, hjust=.5),
        axis.text.x  = element_text(vjust=0.5, size=15), 
        axis.title.x = element_text(vjust=0.5, size=15), 
        axis.text.y  = element_text(vjust=0.5, size=15),
        strip.text = element_text(size=15, face="bold"),
        legend.title  = element_blank(),
        legend.position  = "none",
        legend.text = element_text(vjust=0.5, size=15))

ggsave("3_barplot_EDITED.png", 
       scale = 1,
       height = 18,
       width=12, 
       dpi = 300)


# LET'S GET SPATIAL ######

# 4. Read Shapefile into R: **read_sf and readOGR** ####
library(sf)

shp_thai_dis <- read_sf(dsn = ".", "THA_adm2_s_fix") 

ggplot(data = shp_thai_dis) + 
  geom_sf()

# 5. Add administrative levels: from districts to provinces #####

shp_thai_prov <- shp_thai_dis %>%
  group_by(ID_1) %>%
  summarise(NAME_1 = unique(NAME_1)) %>%
  st_cast() 

ggplot(shp_thai_prov) +
  geom_sf()

# 6. Save sf object into a shapefile ######
st_write(shp_thai_prov, "shp_thai_prov.shp")

# 7. Get centroids of polygons and extract long/lat coordinates. ######
points_thai_prov <- bind_cols(shp_thai_prov$NAME_1,
                              st_coordinates(st_centroid(shp_thai_prov)))

colnames(points_thai_prov)<-c("NAME_1", "lon", "lat")


# 8. Join spatial and statistical data ######

pop_data_thai_lon_lat<-pop_data_thai|>
  left_join(points_thai_prov,by ="NAME_1")

ggplot(pop_data_thai_lon_lat, aes(lon, lat,color=region))+
  geom_point(size=4)+
  labs(title = "Thailand 2021",
       subtitle="Population by provinces",
       x="",
       y="", 
       caption="\nSource: HARBOUR SPACE | DIGITAL CARTOGRAPHY. Data: National Statistical Office (NSO)")+
  theme_light()+
  theme(plot.title = element_text(lineheight=1, size=18, face="bold",hjust = 0.5), #set parameters of title
        plot.subtitle = element_text(lineheight=1, size=15, face="bold",hjust = 0.5),
        plot.caption = element_text(lineheight=1, size=13, hjust=0.5),
        axis.text.x  = element_text(vjust=0.5, size=15), 
        axis.title.x = element_text(vjust=0.5, size=15), 
        axis.text.y  = element_text(vjust=0.5, size=15),
        strip.text = element_text(size=15, face="bold"),
        legend.title  = element_blank(),
        legend.position  = "none",
        legend.text = element_text(vjust=0.5, size=15))

ggsave("4_POINTS_EDITED.png", 
       scale = 1,
       height = 18,
       width=10, 
       dpi = 300)


# 9. Convert a datafram with spatial information into a sf object #####

pop_data_thai_sf<-st_as_sf(pop_data_thai_lon_lat, 
                       coords=c("lon", "lat"),
                       crs=4236)

st_crs(pop_data_thai_sf)

ggplot(pop_data_thai_sf, aes(color=region))+
  geom_sf(size=4)+
  labs(title = "Thailand 2021",
       subtitle="Population by provinces",
       x="",
       y="", 
       caption="\nSource: HARBOUR SPACE | DIGITAL CARTOGRAPHY. Data: National Statistical Office (NSO)")+
  theme_light()+
  theme(plot.title = element_text(lineheight=1, size=18, face="bold",hjust = 0.5), #set parameters of title
        plot.subtitle = element_text(lineheight=1, size=15, face="bold",hjust = 0.5),
        plot.caption = element_text(lineheight=1, size=13, hjust=0.5),
        axis.text.x  = element_text(vjust=0.5, size=15), 
        axis.title.x = element_text(vjust=0.5, size=15), 
        axis.text.y  = element_text(vjust=0.5, size=15),
        strip.text = element_text(size=15, face="bold"),
        legend.title  = element_blank(),
        legend.position  = "none",
        legend.text = element_text(vjust=0.5, size=15))

ggsave("5_POINTS_SF_EDITED.png", 
       scale = 1,
       height = 18,
       width=10, 
       dpi = 300)

# 10. BUBBLE MAP ######

pop_data_thai_sf<-pop_data_thai_sf|>
  mutate(popsize= ifelse(n<250000, "<250",
                         ifelse(n<500000,"[250-500)",
                                ifelse(n<750000,"[500-750)",
                                       ifelse(n<1000000,"[750-1,000)",
                                              ifelse(n<3000000,"[1,000-3,000)",
                                                     "Bangkok"))))),
         popsize=fct_relevel(popsize,
                             "<250",
                             "[250-500)",
                             "[500-750)",
                             "[750-1,000)",
                             "[1,000-3,000)",
                             "Bangkok"),
         region=as.factor(region))

pop_data_thai_sf|>pull(popsize)|>fct_unique()
 levels(pop_data_thai_sf$popsize)

mysizes <-c(.5,1,2,3.5,5,8)*2
names(mysizes)<-levels(pop_data_thai_sf$popsize)

ggplot()+
  geom_sf(data = pop_data_thai_sf,
          aes(color = region, size = popsize)) +
  scale_size_manual(values = mysizes, name = "Population size\nProvinces")+
  labs(title = "Thailand 2022",
       subtitle="Population by provinces (in thousands)",
       x="",
       y="", 
       caption="\nSource: HARBOUR SPACE | DIGITAL CARTOGRAPHY. Data: National Statistical Office (NSO)")+
  theme_light()+
  theme(plot.title = element_text(lineheight=1, size=18, face="bold",hjust = 0.5), #set parameters of title
        plot.subtitle = element_text(lineheight=1, size=15, face="bold",hjust = 0.5),
        plot.caption = element_text(lineheight=1, size=13, hjust=1),
        axis.text.x  = element_text(vjust=0.5, size=15), 
        axis.title.x = element_text(vjust=0.5, size=15), 
        axis.text.y  = element_text(vjust=0.5, size=15),
        strip.text = element_text(size=15, face="bold"),
        legend.title  = element_blank(),
        legend.position  = "right",
        legend.text = element_text(vjust=0.5, size=15))

ggsave("6_POINTS_SF_EDITED.png", 
       scale = 1,
       height = 16,
       width=12, 
       dpi = 300)

# 11. MULTI-LAYER MAP: PROVINCIAL BOUNDARIES + CENTROIDS #####

ggplot()+
  geom_sf(data = shp_thai_dis,fill="#BFBFBF", linewidth=.1)+
  geom_sf(data = shp_thai_prov,fill=NA, linewidth=.5)+
  geom_sf(data= pop_data_thai_sf,aes(color=region,size = popsize))+
  scale_size_manual(values=mysizes,name = "Population size\nProvinces")+
  labs(title = "Thailand 2022",
       subtitle="Population by provinces (in thousands)",
       x="",
       y="", 
       caption="\nSource: HARBOUR SPACE | DIGITAL CARTOGRAPHY. Data: National Statistical Office (NSO)")+
  theme_light()+
  theme(plot.title = element_text(lineheight=1, size=18, face="bold",hjust = 0.5), #set parameters of title
        plot.subtitle = element_text(lineheight=1, size=15, face="bold",hjust = 0.5),
        plot.caption = element_text(lineheight=1, size=13, hjust=1),
        axis.text.x  = element_text(vjust=0.5, size=15), 
        axis.title.x = element_text(vjust=0.5, size=15), 
        axis.text.y  = element_text(vjust=0.5, size=15),
        strip.text = element_text(size=15, face="bold"),
        legend.title  = element_blank(),
        legend.position  = "right",
        legend.text = element_text(vjust=0.5, size=15))

ggsave("7_POINTS_SF_EDITED.png", 
       scale = 1,
       height = 16,
       width=12, 
       dpi = 300)



# 12. CHOROPLETH MAP: JOIN STATISTICAL DATA WITH PROVINCIAL BOUNDARIES #####


pop_data_thai2 <-pop_data_thai_sf|>
  st_drop_geometry()

shp_thai_prov<-shp_thai_prov|>
  left_join(pop_data_thai2,by ="NAME_1")

# 13. CHOROPLETH MAP: PROVINCES BY REGIONS #####

shp_thai_prov<-shp_thai_prov|>
  mutate(region_popsize=as.factor(paste(region,popsize,sep="-")))

levels(shp_thai_prov$region_popsize)

# 13.1. CREATE A COMPLEX PALETTE COLOR #####
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

cols = gg_color_hue(5)

barplot(rep(length(cols),length(cols)), col=c(cols));cols

# 13.2 Create a sequencial color palette for each color #####
fun_color_range <- colorRampPalette(c("#FFFFFF",cols[2]))
CE <- fun_color_range(7)
barplot(rep(length(CE),length(CE)), col=c(CE));CE

fun_color_range <- colorRampPalette(c("#FFFFFF",cols[3]))
NE <- fun_color_range(7)
barplot(rep(length(NE),length(NE)), col=c(NE));NE

fun_color_range <- colorRampPalette(c("#FFFFFF",cols[4]))
NO <- fun_color_range(7)
barplot(rep(length(NO),length(NO)), col=c(NO));NO

fun_color_range <- colorRampPalette(c("#FFFFFF",cols[5]))
ST <- fun_color_range(7)
barplot(rep(length(ST),length(ST)), col=c(ST));ST

fun_color_range <- colorRampPalette(c("#FFFFFF","#BFBFBF"))
LE <- fun_color_range(7)
barplot(rep(length(LE),length(LE)), col=c(LE));LE

# 13.3 Put all colors together into a vector (palette) ######

my_colors<-c(cols[1],#Bangkok
             CE[6],CE[3],CE[4],CE[5],CE[2],
             NE[6],NE[3],NE[4],NE[5],#NE[2],
             NO[6],NO[3],NO[4],NO[5],#NO[2],
             ST[6],ST[3],ST[4],ST[5],ST[2])

# 14. Create a complex choropleth map ######
choro1<-ggplot() + 
  geom_sf(data=shp_thai_prov,
            aes(fill = region_popsize),colour = "black",linewidth=.075) + 
  scale_fill_manual(values=my_colors)+
  
  annotate("text",x = 98, y = 20.25, label = "North", fontface="bold", size=9, color=NO[7])+
  annotate("text", x = 103.5, y = 18.95, label = "North-East", fontface="bold",size=9, color=NE[7])+
  annotate("text", x = 101, y = 12, label = "Central", fontface="bold", size=9, color=CE[7])+
  annotate("text", x = 101.3, y = 8, label = "South",  fontface="bold", size=9, color=ST[7])+
  
  annotate("rect", xmin = 96.5, xmax = 98, ymin = 4.8, ymax = 5.2,fill=my_colors[1],color="black", linewidth=.25)+
  annotate("rect", xmin = 98.2, xmax = 99.7, ymin = 4.8, ymax = 5.2,fill=LE[6],color="black", linewidth=.25)+
  annotate("rect", xmin = 99.9, xmax = 101.4, ymin = 4.8, ymax = 5.2,fill=LE[5],color="black", linewidth=.25)+
  annotate("rect", xmin = 101.6, xmax = 103.1, ymin = 4.8, ymax = 5.2,fill=LE[4],color="black", linewidth=.25)+
  annotate("rect", xmin = 103.3, xmax = 104.8, ymin = 4.8, ymax = 5.2,fill=LE[3],color="black", linewidth=.25)+
  annotate("rect", xmin = 105, xmax = 106.55, ymin = 4.8, ymax = 5.2,fill=LE[2],color="black", linewidth=.25)+
  annotate("text",x = 97.3, y = 5, label = "Bangkok",fontface="bold",size=5.75,color="#28353a")+
  annotate("text",x = 98.95, y = 5, label = "3000-1000",fontface="bold",size=5.5,color="#28353a")+
  annotate("text",x = 100.65, y = 5, label = "1000-750",fontface="bold",size=5.5,color="#28353a")+
  annotate("text",x = 102.35, y = 5, label = "750-500",fontface="bold",size=5.5,color="#28353a")+
  annotate("text",x = 104.05, y = 5, label = "500-250",fontface="bold",size=5.5,color="#28353a")+
  annotate("text",x = 105.75, y = 5, label = "<250",fontface="bold",size=5.5,color="#28353a")+
  
  annotate("rect", xmin = 98.2, xmax = 99.7, ymin = 4.4, ymax = 4.6,fill=NO[6],color="black", linewidth=.25)+
  annotate("rect", xmin = 99.9, xmax = 101.4, ymin = 4.4, ymax = 4.6,fill=NO[5],color="black", linewidth=.25)+
  annotate("rect", xmin = 101.6, xmax = 103.1, ymin = 4.4, ymax = 4.6,fill=NO[4],color="black", linewidth=.25)+
  annotate("rect", xmin = 103.3, xmax = 104.8, ymin = 4.4, ymax = 4.6,fill=NO[3],color="black", linewidth=.25)+
 
  annotate("rect", xmin = 98.2, xmax = 99.7, ymin = 4, ymax = 4.2,fill=NE[6],color="black", linewidth=.25)+
  annotate("rect", xmin = 99.9, xmax = 101.4, ymin = 4, ymax = 4.2,fill=NE[5],color="black", linewidth=.25)+
  annotate("rect", xmin = 101.6, xmax = 103.1, ymin = 4, ymax = 4.2,fill=NE[4],color="black", linewidth=.25)+
  annotate("rect", xmin = 103.3, xmax = 104.8, ymin = 4, ymax = 4.2,fill=NE[3],color="black", linewidth=.25)+
 
  annotate("rect", xmin = 98.2, xmax = 99.7, ymin = 3.6, ymax = 3.8,fill=CE[6],color="black", linewidth=.25)+
  annotate("rect", xmin = 99.9, xmax = 101.4, ymin = 3.6, ymax = 3.8,fill=CE[5],color="black", linewidth=.25)+
  annotate("rect", xmin = 101.6, xmax = 103.1, ymin = 3.6, ymax = 3.8,fill=CE[4],color="black", linewidth=.25)+
  annotate("rect", xmin = 103.3, xmax = 104.8, ymin = 3.6, ymax = 3.8,fill=CE[3],color="black", linewidth=.25)+
  annotate("rect", xmin = 105, xmax = 106.55, ymin = 3.6, ymax = 3.8,fill=CE[2],color="black", linewidth=.25)+
  
  annotate("rect", xmin = 98.2, xmax = 99.7, ymin = 3.2, ymax = 3.4,fill=ST[6],color="black", linewidth=.25)+
  annotate("rect", xmin = 99.9, xmax = 101.4, ymin = 3.2, ymax = 3.4,fill=ST[5],color="black", linewidth=.25)+
  annotate("rect", xmin = 101.6, xmax = 103.1, ymin = 3.2, ymax = 3.4,fill=ST[4],color="black", linewidth=.25)+
  annotate("rect", xmin = 103.3, xmax = 104.8, ymin = 3.2, ymax = 3.4,fill=ST[3],color="black", linewidth=.25)+
  annotate("rect", xmin = 105, xmax = 106.55, ymin = 3.2, ymax = 3.4,fill=ST[2],color="black", linewidth=.25)+
  
  labs(title = "Thailand 2022",
       subtitle="Population by provinces (in thousands)",
       x="",
       y="", 
       caption="\nSource: HARBOUR SPACE | DIGITAL CARTOGRAPHY. Data: National Statistical Office (NSO)")+
  theme_light()+
  theme(plot.title = element_text(lineheight=1, size=18, face="bold",hjust = 0.5), #set parameters of title
        plot.subtitle = element_text(lineheight=1, size=15, face="bold",hjust = 0.5),
        plot.caption = element_text(lineheight=1, size=13, hjust=1),
        axis.text.x  = element_text(vjust=0.5, size=15), 
        axis.title.x = element_text(vjust=0.5, size=15), 
        axis.text.y  = element_text(vjust=0.5, size=15),
        strip.text = element_text(size=15, face="bold"),
        legend.title  = element_blank(),
        legend.position  = "none",
        legend.text = element_text(vjust=0.5, size=15))

choro1
ggsave("8_CHOROPLETH_EDITED.png", 
       scale = 1,
       height = 16,
       width=12, 
       dpi = 300)
# 15. BLACK BACKGROUNDVERSION ####

choro1+
theme(plot.title = element_text(lineheight=.5, size=35, face="bold", hjust=.5,colour="#D8D8D8"),
      plot.subtitle = element_text(lineheight=.5, size=20, face="bold",hjust=.5,colour="#D8D8D8"),
      plot.caption = element_text(lineheight=1, size=17,face="bold", hjust=.5,colour="#D8D8D8"),
      legend.title = element_text(angle = 0,vjust=0.5, size=12,colour="black",face="bold"),
      legend.text = element_text(colour="black", size = 12),
      legend.position="none",
      legend.justification=c(1,0), 
      legend.background = element_rect(fill=NA, colour = NA),
      legend.key.size = unit(1.5, 'lines'),
      legend.key = element_rect(colour = NA, fill = NA),
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks = element_line(colour="#28353a"),
      panel.border = element_rect(colour ="#28353a", fill=NA,linewidth = 2),
      panel.grid.major=element_line(colour="#28353a"),
      panel.grid.minor =element_line(colour="#28353a"),
      plot.background =  element_rect(fill ="#28353a",colour ="#28353a"),
      panel.background =element_rect(fill ="#28353a", colour = "#28353a"))

  ggsave("9_CHOROPLETH_EDITED.png", 
       scale = 1,
       height = 16,
       width=12, 
       dpi = 300)


# VERY BASIC DATA MANIPULATION WITH DPLYR FOR ORDERING THE BARS

pop_data_thai2 <- pop_data_thai2 |>
  mutate(NAME_1 = fct_reorder(NAME_1, n, .desc = FALSE))

pop_data_thai2<-pop_data_thai2|>
  mutate(region_popsize=as.factor(paste(region,popsize,sep="-")))


bars<-ggplot(pop_data_thai2, 
       aes(x=NAME_1, y=n/1000000,fill=region_popsize))+
  coord_flip()+
  expand_limits(y = 6)+
  scale_fill_manual(values=my_colors)+
  geom_bar(stat="identity", 
           linewidth=.25,
           color="black")+
  geom_text(aes(label = format(n,big.mark=","),hjust = -.25))+
  labs(title = "\n",
       subtitle="",
       x="",
       y="\nPopulation (in millons)", 
       caption="\n\n\n")+
  facet_grid(rows = vars(region), scales = "free_y", switch = "y", space = "free_y") +
  theme_light()+
  theme(plot.title = element_text(lineheight=1, size=18, face="bold",hjust = 0.5), #set parameters of title
        plot.subtitle = element_text(lineheight=1, size=15, face="bold",hjust = 0.5),
        plot.caption = element_text(lineheight=1, size=13, hjust=1),
        axis.text.x  = element_text(vjust=0.5, size=15), 
        axis.title.x = element_text(vjust=0.5, size=15), 
        axis.text.y  = element_text(vjust=0.5, size=15),
        strip.text = element_text(size=15, face="bold"),
        legend.title  = element_blank(),
        legend.position  = "none",
        legend.text = element_text(vjust=0.5, size=15))

ggsave("4_barplot_EDITED.png",
       plot=bars,
       scale = 1,
       height = 18,
       width=12, 
       dpi = 300)

library(gridExtra)
arr<-grid.arrange(choro1, bars, nrow = 1,widths = c(1, 1.35))

ggsave("10_ARRANGED.png",
       plot=arr,
       scale = 1,
       height = 18,
       width=18, 
       dpi = 300)

# Consolidation exercise #######


# 1. Create a new sf object with the division by regions of Thailand.
# 2. Save the sf step one as a shapefile. 
# Load necessary libraries
library(sf)
library(dplyr)

# Assuming `shp_thai_prov` is the existing sf object with provincial boundaries
# and `pop_data_thai` is the data frame with statistical data

# 1. Create a new sf object with the division by regions of Thailand
shp_thai_region <- shp_thai_prov %>%
  left_join(pop_data_thai, by = c("region" = "region")) %>%
  group_by(region) %>%
  summarise() %>%
  st_union()

# 2. Save the sf step one as a shapefile
st_write(shp_thai_region, "shp_thai_region.shp")



# 3. Create a discrete choropleth map showing the population by provinces
# of Thailand. Overlay your layer with the division by regions.
# Hints: recall library Rcolorbrewer for creating you color palette. 
# Note: your final map should look similar to png EX1_CHOROPLETH_EDITED.png

# 14. Create a complex choropleth map ######
choro1<-ggplot() + 
  geom_sf(data=shp_thai_prov,
          aes(fill = region_popsize),colour = "black",linewidth=.075) + 
  scale_fill_manual(values=my_colors)+
  
  annotate("text",x = 98, y = 20.25, label = "North", fontface="bold", size=9, color=NO[7])+
  annotate("text", x = 103.5, y = 18.95, label = "North-East", fontface="bold",size=9, color=NE[7])+
  annotate("text", x = 101, y = 12, label = "Central", fontface="bold", size=9, color=CE[7])+
  annotate("text", x = 101.3, y = 8, label = "South",  fontface="bold", size=9, color=ST[7])+
  
  annotate("rect", xmin = 96.5, xmax = 98, ymin = 4.8, ymax = 5.2,fill=my_colors[1],color="black", linewidth=.25)+
  annotate("rect", xmin = 98.2, xmax = 99.7, ymin = 4.8, ymax = 5.2,fill=LE[6],color="black", linewidth=.25)+
  annotate("rect", xmin = 99.9, xmax = 101.4, ymin = 4.8, ymax = 5.2,fill=LE[5],color="black", linewidth=.25)+
  annotate("rect", xmin = 101.6, xmax = 103.1, ymin = 4.8, ymax = 5.2,fill=LE[4],color="black", linewidth=.25)+
  annotate("rect", xmin = 103.3, xmax = 104.8, ymin = 4.8, ymax = 5.2,fill=LE[3],color="black", linewidth=.25)+
  annotate("rect", xmin = 105, xmax = 106.55, ymin = 4.8, ymax = 5.2,fill=LE[2],color="black", linewidth=.25)+
  annotate("text",x = 97.3, y = 5, label = "Bangkok",fontface="bold",size=5.75,color="#28353a")+
  annotate("text",x = 98.95, y = 5, label = "3000-1000",fontface="bold",size=5.5,color="#28353a")+
  annotate("text",x = 100.65, y = 5, label = "1000-750",fontface="bold",size=5.5,color="#28353a")+
  annotate("text",x = 102.35, y = 5, label = "750-500",fontface="bold",size=5.5,color="#28353a")+
  annotate("text",x = 104.05, y = 5, label = "500-250",fontface="bold",size=5.5,color="#28353a")+
  annotate("text",x = 105.75, y = 5, label = "<250",fontface="bold",size=5.5,color="#28353a")+
  
  annotate("rect", xmin = 98.2, xmax = 99.7, ymin = 4.4, ymax = 4.6,fill=NO[6],color="black", linewidth=.25)+
  annotate("rect", xmin = 99.9, xmax = 101.4, ymin = 4.4, ymax = 4.6,fill=NO[5],color="black", linewidth=.25)+
  annotate("rect", xmin = 101.6, xmax = 103.1, ymin = 4.4, ymax = 4.6,fill=NO[4],color="black", linewidth=.25)+
  annotate("rect", xmin = 103.3, xmax = 104.8, ymin = 4.4, ymax = 4.6,fill=NO[3],color="black", linewidth=.25)+
  
  annotate("rect", xmin = 98.2, xmax = 99.7, ymin = 4, ymax = 4.2,fill=NE[6],color="black", linewidth=.25)+
  annotate("rect", xmin = 99.9, xmax = 101.4, ymin = 4, ymax = 4.2,fill=NE[5],color="black", linewidth=.25)+
  annotate("rect", xmin = 101.6, xmax = 103.1, ymin = 4, ymax = 4.2,fill=NE[4],color="black", linewidth=.25)+
  annotate("rect", xmin = 103.3, xmax = 104.8, ymin = 4, ymax = 4.2,fill=NE[3],color="black", linewidth=.25)+
  
  annotate("rect", xmin = 98.2, xmax = 99.7, ymin = 3.6, ymax = 3.8,fill=CE[6],color="black", linewidth=.25)+
  annotate("rect", xmin = 99.9, xmax = 101.4, ymin = 3.6, ymax = 3.8,fill=CE[5],color="black", linewidth=.25)+
  annotate("rect", xmin = 101.6, xmax = 103.1, ymin = 3.6, ymax = 3.8,fill=CE[4],color="black", linewidth=.25)+
  annotate("rect", xmin = 103.3, xmax = 104.8, ymin = 3.6, ymax = 3.8,fill=CE[3],color="black", linewidth=.25)+
  annotate("rect", xmin = 105, xmax = 106.55, ymin = 3.6, ymax = 3.8,fill=CE[2],color="black", linewidth=.25)+
  
  annotate("rect", xmin = 98.2, xmax = 99.7, ymin = 3.2, ymax = 3.4,fill=ST[6],color="black", linewidth=.25)+
  annotate("rect", xmin = 99.9, xmax = 101.4, ymin = 3.2, ymax = 3.4,fill=ST[5],color="black", linewidth=.25)+
  annotate("rect", xmin = 101.6, xmax = 103.1, ymin = 3.2, ymax = 3.4,fill=ST[4],color="black", linewidth=.25)+
  annotate("rect", xmin = 103.3, xmax = 104.8, ymin = 3.2, ymax = 3.4,fill=ST[3],color="black", linewidth=.25)+
  annotate("rect", xmin = 105, xmax = 106.55, ymin = 3.2, ymax = 3.4,fill=ST[2],color="black", linewidth=.25)+
  
  labs(title = "Thailand 2022",
       subtitle="Population by provinces (in thousands)",
       x="",
       y="", 
       caption="\nSource: HARBOUR SPACE | DIGITAL CARTOGRAPHY. Data: National Statistical Office (NSO)")+
  theme_light()+
  theme(plot.title = element_text(lineheight=1, size=18, face="bold",hjust = 0.5), #set parameters of title
        plot.subtitle = element_text(lineheight=1, size=15, face="bold",hjust = 0.5),
        plot.caption = element_text(lineheight=1, size=13, hjust=1),
        axis.text.x  = element_text(vjust=0.5, size=15), 
        axis.title.x = element_text(vjust=0.5, size=15), 
        axis.text.y  = element_text(vjust=0.5, size=15),
        strip.text = element_text(size=15, face="bold"),
        legend.title  = element_blank(),
        legend.position  = "none",
        legend.text = element_text(vjust=0.5, size=15))

choro1
ggsave("8_CHOROPLETH_EDITED.png", 
       scale = 1,
       height = 16,
       width=12, 
       dpi = 300)





##############


# Install and load required packages
install.packages(c("tidyverse", "sf", "viridis"))
library(tidyverse)
library(sf)
library(viridis)

# Read data
pop_data_thai <- read.csv("pop_data_thai.csv")
shp_thai_dis <- read_sf(dsn = ".", "THA_adm2_s_fix") 

# Plot the shapefile
ggplot(data = shp_thai_dis) + 
  geom_sf()

# Create a simplified province-level dataset
shp_thai_prov <- shp_thai_dis %>%
  group_by(ID_1) %>%
  summarise(NAME_1 = unique(NAME_1)) %>%
  st_cast() 

# Join population data to the province-level dataset
pop_data_thai_lon_lat <- shp_thai_prov %>%
  left_join(pop_data_thai, by = "NAME_1")

# Transform to spatial data frame
pop_data_thai_sf <- st_as_sf(pop_data_thai_lon_lat, coords = c("longitude", "latitude"), crs = 4326)

# Define population categories
pop_data_thai_sf <- pop_data_thai_sf %>%
  mutate(popsize = ifelse(n < 250000, "<250",
                          ifelse(n < 500000, "[250-500)",
                                 ifelse(n < 750000, "[500-750)",
                                        ifelse(n < 1000000, "[750-1,000)",
                                               ifelse(n < 3000000, "[1,000-3,000)", "Bangkok"))))),
         popsize = fct_relevel(popsize,
                               "<250",
                               "[250-500)",
                               "[500-750)",
                               "[750-1,000)",
                               "[1,000-3,000)",
                               "Bangkok"),
         region = as.factor(region))

# Define your custom color palette using viridis colors
custom_palette <- viridis_pal(option = "viridis")(6)

# Create a region-level dataset for legend customization
region <- pop_data_thai %>%
  select(NAME_1, region)

region_sf <- shp_thai_prov %>%
  select(NAME_1, geometry)

region_data <- merge(region_sf, region, by = "NAME_1")
region_data <- region_data %>%
  group_by(region) %>%
  summarise(geometry = st_boundary(st_union(geometry)))

# Plot the map with the new color palette
ggplot() + 
  geom_sf(data = pop_data_thai_sf,
          aes(fill = popsize),
          colour = "black", linewidth = 0.025) + 
  geom_sf(data = region_data, colour = "black", linewidth = 0.20) +
  scale_fill_manual(values = custom_palette, 
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         keywidth = unit(2.5, "cm"),
                                         label.position = "bottom")) +
  labs(title = "Thailand 2022",
       subtitle = "Population by provinces (in thousands)",
       x = "",
       y = "", 
       caption = "By Gio Kurtanidze") +
  theme_light() +
  theme(
    plot.title = element_text(lineheight = 1, size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(lineheight = 1, size = 16, face = "bold", hjust = 0.5),
    plot.caption = element_text(lineheight = 1, size = 15, hjust = 0.5),
    axis.text.x = element_text(vjust = 0.9, size = 15),
    axis.title.x = element_text(vjust = 0.9, size = 15),
    axis.text.y = element_text(vjust = 0.9, size = 15),
    strip.text = element_text(size = 5, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(vjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key.size = unit(1.5, 'lines'),
    legend.key.width = unit(3, 'lines'),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt") 
  )  

# Save the map as an image
ggsave("task_map1.png", 
       scale = 0.6,
       height = 16,
       width = 18, 
       dpi = 300)



