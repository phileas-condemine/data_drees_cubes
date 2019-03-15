library(data.table)
files=list.files("../downloads")
files=files[grep("csv$",files)]
files_infos=pbapply::pblapply(files,function(x)file.info(paste0("../downloads/",x)))
files_infos=rbindlist(files_infos)
files_infos=cbind(files_infos,files)
names(files_infos)
files_infos=files_infos[,c("files","mtime","size")]
files_infos=data.table(files_infos)
files_infos$file_clean=gsub("_([0-9]+).csv","",files_infos$files)
files_infos$file_clean=gsub('-',' ',files_infos$file_clean)
files_infos$file_clean=gsub('_',' ',files_infos$file_clean)
files_infos$file_clean=gsub('(^| |v)([0-9]+)($| )','',files_infos$file_clean)
setorder(files_infos,-mtime)
# ON SUPPRIME LES DOUBLONS EN GARDANT LES PLUS RECENTS
nrow(files_infos)
files_infos=files_infos[,.SD[1],by="file_clean"]
nrow(files_infos)


load("../folders_with_IVT.RData")
folders_with_IVT$title=gsub("Rapport en graphique : ","",folders_with_IVT$title)
folders_with_IVT$title=gsub("Rapport en tableau : ","",folders_with_IVT$title)


files_infos=merge(files_infos,folders_with_IVT,by.x="files",by.y="file_nm")


save(files_infos,file="files_infos.RData")
