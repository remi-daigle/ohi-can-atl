# require(rgdal)
# require(rgeos)
# # #### This section is commented out because R couldn't handle doing the spatial intersection between biogeo and stlaurent ####
# # 
# # # The regions are based on the CSAS SAR on biogeographic calssification of Canadian marine areas
# # # http://www.dfo-mpo.gc.ca/csas-sccs/publications/SAR-AS/2009/2009_056_e.pdf
# # # I got the actual shapefile from Rodolphe Deviller at MUN
# 
# # biogeo <- readOGR("shapefile/Biogeographicregions/commondata/shapefiles","BiogeographicRegions")
# # stlaurent <- subset(biogeo,NAME=='Gulf')
# # biogeo <- subset(biogeo,NAME=='NL-Labrador Shelves'|
# #                       NAME=='Scotian Shelf')
# # 
# #  
# # # We further subdivided the "Gulf" region based on a georeferenced version of Quebec's "sections" for the St. Lawrence:
# # # http://planstlaurent.qc.ca/fileadmin/site_documents/images/Le_Saint-Laurent/GISL_sections_2014_ANG.jpg
# #
# # stlaurent_div <- readOGR("shapefile/stlaurent_regions","stlaurent")
# # stlaurent_div <- spTransform(stlaurent_div,CRS(proj4string(biogeo)))
# # writeOGR(stlaurent_div,"shapefile/stlaurent_regions","stlaurent_proj","ESRI Shapefile")
# # 
# # #### Used QGIS ####
# # # to generate the intersection between "stlaurent_proj" and "BiogeographicRegions" ("int_sl_bg"). Also the difference between "BiogeographicRegions" and "int_sl_bg" ("diff_bg_sl")
# 
# stlaurent <- readOGR("shapefile/stlaurent_regions","int_sl_bg")
# biogeo <- readOGR("shapefile/stlaurent_regions","diff_bg_sl")
# 
# cols <- rainbow(7)
# # plot(biogeo,col=cols[1:3])
# # plot(stlaurent,col=cols[4:7],add=T)
# 
# # housekeeping add stlaurent variable to biogeo dataframes so it can rbind
# biogeo$stlaurent <- NA
# # remove Fluvial Estuary
# stlaurent <- stlaurent[-4,]
# 
# biogeo <- spChFIDs(biogeo,c("NL-Labrador Shelves","Outer Gulf","Scotian Shelf"))
# stlaurent <- spChFIDs(stlaurent,c("Inner Gulf","Lower Estuary","Upper Estuary"))
# 
# 
# regions_final <- rbind(stlaurent,biogeo)
# gIsValid(biogeo2)
# 
# writeOGR(regions_final,"shapefile","regions_final","ESRI Shapefile",overwrite_layer = TRUE)

require(sf)
require(tidyverse)
proj <- "+proj=aea +lat_1=40 +lat_2=60 +lat_0=40 +lon_0=-60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
biogeo_gulf <- st_read("shapefile/Biogeographicregions/BiogeographicZonesCleanAlbers.shp") %>% 
    filter(Name=="Gulf")%>% 
    st_transform(crs=proj)
biogeo_shelf <- st_read("shapefile/Biogeographicregions/BiogeographicZonesCleanAlbers.shp") %>% 
    filter(Name=="Scotian Shelf"|Name=="NL-Labrador Shelves")%>% 
    st_transform(crs=proj)
stlaurent_div <- st_read("shapefile/stlaurent_regions/stlaurent.shp") %>% 
    # filter(stlaurent!="A") %>% 
    st_transform(crs=proj)

int_sl_bg <- st_intersection(stlaurent_div,biogeo_gulf) %>% 
    mutate(ID=paste0(ID,stlaurent)) %>% 
    select(-stlaurent)
colnames(int_sl_bg)[11] <- "geometry"
attr(int_sl_bg,"sf_column") <- "geometry"
int_sl_bg <- st_cast(int_sl_bg, "MULTIPOLYGON")

# st_geometry(int_sl_bg)
# plot(int_sl_bg$geometry)

diff_bg_sl <- st_difference(biogeo_gulf,st_union(stlaurent_div))
diff_bg_sl <- st_difference(diff_bg_sl,st_convex_hull(st_union(stlaurent_div[2:5,]))) # remove slivers

colnames(diff_bg_sl)[11] <- "geometry"
attr(diff_bg_sl,"sf_column") <- "geometry"
# plot(diff_bg_sl$geometry)

biogeo_gulf <- rbind(diff_bg_sl,int_sl_bg)
plot(biogeo_gulf$geometry)

# for(i in 1:5){
#     plot(biogeo_gulf$geoms,main=i,col="white")
#     plot(biogeo_gulf$geoms[i],add=T,col="blue")
# }
### remove fresh water sections ###
biogeo_gulf <- biogeo_gulf[1:3,]

# ### fix data table ###
# names(biogeo_gulf)
# # biogeo_gulf$ID
# # biogeo_gulf$REGION
biogeo_gulf$AREA_KM2 <- as.integer(round(st_area(biogeo_gulf)/1000000))
biogeo_gulf$AREA_HA <- as.integer(round(st_area(biogeo_gulf)/10000))
# # biogeo_gulf$X_CENTROID
# # biogeo_gulf$Y_CENTROID
biogeo_gulf$Label <- c("Gulf - Outer","Gulf - Inner", "Gulf - Estuary")
biogeo_gulf$Name <- c("Gulf - Outer","Gulf - Inner", "Gulf - Estuary")
biogeo_gulf$French_Nam <- c("Golfe - Externe","Golfe - Interne", "Golfe - Estuaire")
# # biogeo_gulf$Legend

biogeo <- rbind(biogeo_gulf,biogeo_shelf)
biogeo <- st_cast(biogeo, "MULTIPOLYGON")

plot(biogeo$geometry)
# st_geometry(biogeo)
# st_geometry(biogeo_shelf)
# st_geometry(biogeo_gulf)
# st_geometry(diff_bg_sl)
# st_geometry(int_sl_bg)

unlink(list.files(path="shapefile/",pattern='regions_final.',full.names = TRUE))
st_write(biogeo,"shapefile/regions_final.shp")
require(rgdal)
test <- readOGR("shapefile","regions_final")
plot(test)
