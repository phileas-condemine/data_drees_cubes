library(data.table)
library(DT)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(leaflet)
library(shinyWidgets)
# library(shinyBS)
library(bsplus)
library(reactlog)
# tell shiny to log reactivity
options("shiny.reactlog" = TRUE)
# library(shinyloadtest)
load("data/files_infos.RData")
# on mélange l'ordre des tables pour ne pas avantager certaines
files_infos$random_order=sample(nrow(files_infos))
setorder(files_infos,random_order)

load("../metadata_IVT.RData")
load("../dimension_lib2.RData")
# utils/prep_prolygons.R
load("data/FR_gadm.RData")
# source("utils/find_readable_files.R")
FRA_dep_1pct@data$ordre=1:nrow(FRA_dep_1pct@data)


tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
                                 transform: translate(-50%,20%);
                                 position: fixed !important;
                                 left: 50%;
                                 text-align: center;
                                 padding-left: 10px; 
                                 padding-right: 10px; 
                                 background: rgba(255,255,255,0.75);
                                 font-weight: bold;
                                 font-size: 28px;
                                 }
                                 "))

fix_noms_duplicated=function(headers){
  dup_dt=data.table(noms=headers,dup=duplicated(headers))
  dup_dt[,dup_nb:=cumsum(dup),by="noms"]
  dup_dt$dup_nb=as.character(dup_dt$dup_nb)
  dup_dt[dup_nb=="0",dup_nb:=""]
  paste0(dup_dt$noms,dup_dt$dup_nb)
}

# do.call mean vs sum ne se comporte pas de la même manière
# https://www.reddit.com/r/rstats/comments/6igdrr/for_some_reason_docallmean_x_does_not_work/
mean2=function (..., na.rm = FALSE){
  mean(x=c(...),na.rm=na.rm)
}
  
  