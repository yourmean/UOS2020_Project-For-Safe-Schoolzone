library(sf)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)


seoul_shp = read_sf("C:/Users/rxdcxdrnine/Desktop/uos_big_data_2020/test.geojson")
seoul_data = read.csv("C:/Users/rxdcxdrnine/Desktop/uos_big_data_2020/accident_children_total.csv")

head(seoul_shp)
head(seoul_data)

# calculate points at which to plot labels
centroids <- seoul_shp %>% 
    st_centroid() %>% 
    bind_cols(as_data_frame(st_coordinates(.)))    # unpack points to lat/lon columns

seoul_data = seoul_data %>% 
    mutate(지역 = as.character(지역)) %>%
    mutate(발생건수 = 발생건수 ** 2.5)

p = seoul_data %>% 
    left_join(seoul_shp, ., by = c('SIG_KOR_NM' = '지역')) %>% 
    ggplot() + 
    geom_sf(aes(fill = 발생건수), color=alpha("white", 0)) +
    scale_fill_gradientn(colours=c("#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#990000")) +
    guides(fill=FALSE) +
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
p



ggsave("test.png", bg = "transparent")




#### senior

seoul_shp = read_sf("C:/Users/rxdcxdrnine/Desktop/uos_big_data_2020/test.geojson")
seoul_data = read.csv("C:/Users/rxdcxdrnine/Desktop/uos_big_data_2020/accident_senior_total.csv")

head(seoul_shp)
head(seoul_data)

# calculate points at which to plot labels
centroids <- seoul_shp %>% 
    st_centroid() %>% 
    bind_cols(as_data_frame(st_coordinates(.)))    # unpack points to lat/lon columns

seoul_data = seoul_data %>% 
    mutate(지역 = as.character(지역)) %>%
    mutate(발생건수 = 발생건수 ** 0.1)

p = seoul_data %>% 
    left_join(seoul_shp, ., by = c('SIG_KOR_NM' = '지역')) %>% 
    ggplot() + 
    geom_sf(aes(fill = 발생건수), color=alpha("white", 0)) +
    scale_fill_gradientn(colours=c("#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#990000")) +
    guides(fill=FALSE) +
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
p
ggsave("test2.png", bg = "transparent")





