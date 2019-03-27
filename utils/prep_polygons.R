library(rmapshaper)
# data from https://gadm.org/download_country_v3.html
FRA_dep=readRDS("data/FRA_adm2.rds")
FRA_dep_10pct<-ms_simplify(FRA_dep,keep=.1)
FRA_dep_1pct<-ms_simplify(FRA_dep,keep=.01)
# save(list=c("FRA_dep_1pct","FRA_dep_10pct","FRA_dep"),file="data/FR_gadm.RData")
# leaflet(simplified_pol) %>% 
#   addTiles() %>%
#   addPolygons()

corres_noms_reg=fread("data/noms_regions.csv")
View(corres_noms_reg)
FRA_reg=readRDS("data/FRA_adm1.rds")
data2=FRA_reg@data
data2$ordre=1:nrow(data2)
data2=merge(data2,corres_noms_reg,by.x="NAME_1",by.y="New",all.x=T)
sum(is.na(data2$Old))
data2=dplyr::arrange(data2)
FRA_reg@data=data2
FRA_reg_10pct<-ms_simplify(FRA_reg,keep=.1)
FRA_reg_1pct<-ms_simplify(FRA_reg,keep=.01)
save(list=c("FRA_reg_1pct","FRA_reg_10pct","FRA_reg",
            "FRA_dep_1pct","FRA_dep_10pct","FRA_dep"),file="data/FR_gadm.RData")


leaflet(FRA_reg) %>%
  addTiles() %>%
  addPolygons(label = ~NAME_1)
