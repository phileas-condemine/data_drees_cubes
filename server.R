function(input,output,session){
  # path=function(){paste0("../downloads/","adeli-audioprothesiste18-tab1_80896366051273.csv")}#files_infos$files[2])}
  path=reactive({
    if(length(input$list_fichiers_cell_clicked)>0){
      clicked=input$list_fichiers_cell_clicked
      row_clicked=clicked$row
      content=files_infos[row_clicked]
      path=paste0("../downloads/",content$files)
      path
    } else {NULL}
  })
  metadata=reactive({
    if(length(input$list_fichiers_cell_clicked)>0){
      clicked=input$list_fichiers_cell_clicked
      row_clicked=clicked$row
      content=files_infos[row_clicked]
      href=content$href
      metadata=metadata_IVT[[href]]
      # print(metadata)
      metadata
    } else {NULL}
  })
  variables_a_croiser=reactive({
    req(path())
    mydata=fread(path(),header = F,stringsAsFactors = F)
    # print(head(mydata))
    data_names1=mydata[1]%>%unname%>%unlist
    if(sum(is.na(as.numeric(data_names1)))==length(data_names1)){
      #il y a des headers DONNEES en 1ere ligne, le reste en 2ème ligne, il suffit de les récupérer
      data_names2=mydata[2]%>%unname%>%unlist
      variables_a_croiser=data_names2
      variables_a_croiser=variables_a_croiser[!variables_a_croiser==""]
      variables_a_croiser
    } else  {#pas de headers
      metadata=metadata()
      vars=rbind(metadata$vars_in_cols,metadata$vars_in_rows,metadata$vars_in_others)
      # vars=vars[order(as.numeric(vars$value))]#faut-il trier ???
      variables_a_croiser=vars$txt[!vars$txt=="DONNEES"]
      variables_a_croiser
    }
  })
  fulldata=reactive({
    req(path())
    fulldata=fread(path(),header = F,stringsAsFactors = F)
    # print(head(fulldata))
    data_names1=fulldata[1]%>%unname%>%unlist
    data_names2=fulldata[2]%>%unname%>%unlist
    if(sum(is.na(as.numeric(data_names1)))==length(data_names1)){
    #il y a des headers DONNEES en 1ere ligne, le reste en 2ème ligne, il suffit de les récupérer
    data_names2[data_names2==""] <- data_names1[data_names2==""]
    fulldata=fread(path(),header = F,stringsAsFactors = F,skip = 3)
    names(fulldata) <- as.character(data_names2)
    } else  {#pas de headers
      metadata=metadata()
      vars=rbind(metadata$vars_in_cols,metadata$vars_in_rows,metadata$vars_in_others)
      # vars=vars[order(as.numeric(vars$value))]#faut-il trier ???

      # noms_donnees=metadata$hierarchies[[vars[vars$txt=="DONNEES",]$value]]
      # noms_donnees=noms_donnees[,ncol(noms_donnees),with=F]
      # check_err<<-noms_donnees
      # noms_donnees=noms_donnees%>%unlist%>%unname
      # noms_donnees=noms_donnees[!is.na(noms_donnees)]
      # noms_donnees=noms_donnees[!noms_donnees==""]      
      
      headers<<-readLines(path(),n=2)[2]
      headers=readLines(path(),n=2)[2]
      headers=gsub('"','',headers)
      headers=strsplit(headers,',')[[1]]
      headers=headers[-c(1,2)]
      noms_donnees=headers
      # IL FAUT GERER LES NOMS DOUBLONNES
      if (sum(duplicated(headers))>0){
        print("fix noms dupliqués")
        noms_donnees=fix_noms_duplicated(headers)
        print(noms_donnees)
      }
      
      # print("nom_donnees")
      # print(noms_donnees)
      # print("donnees de croisement")
      # print(vars$txt[!vars$txt=="DONNEES"])
      names(fulldata)<-c(vars$txt[!vars$txt=="DONNEES"],noms_donnees)
      
      
      }
    fulldata=fulldata%>%data.table
  })

  
  mydata=reactive({
    req(fulldata())
    mydata=fulldata()
    mydata2<<-mydata
    #On filtre parmi les stats pré-calculées pour obtenir le niveau hiérarchique souhaité
    if (!(is.null(input$operation)|is.null(input$join_vars))){
      metadata=metadata()
      vars=rbind(metadata$vars_in_cols,metadata$vars_in_rows,metadata$vars_in_others)
      vars_hc<<-vars
      vars$value=as.character(vars$value)
      print(vars)
      vars=vars[!grepl("DONNEES",vars$txt),]
      for (ind in 1:nrow(vars)){
        var_ind=vars$value[ind]
        var_nm=vars$txt[ind]
        print(var_nm)
        var_nm=gsub(' ','_',var_nm)
        hierarchie=metadata$hierarchies[[var_ind]]
        hierarchie=data.table(hierarchie)
        names(hierarchie) <- as.character(1:ncol(hierarchie))
        if(ncol(hierarchie)>1){
          input_nm=paste0("cols",var_nm)
          req(input[[input_nm]])
          if(input[[input_nm]]>1){
            redudants_cols=as.character(1:(as.numeric(input[[input_nm]])-1))
            redundants_vals=unique(unname(unlist(hierarchie[,redudants_cols,with=F])))
            vals_chosen_level=unique(unname(unlist(hierarchie[,as.character(input[[input_nm]]),with=F])))
            vals_chosen_level=setdiff(vals_chosen_level,redundants_vals)
          } else{
            vals_chosen_level=unique(unname(unlist(hierarchie[,as.character(input[[input_nm]]),with=F])))
          }
          if (!vars$txt[ind]%in%names(mydata)){
            var_nm_check=stringdist::amatch(vars$txt[ind],names(mydata),maxDist = 1000)
            var_nm_check=names(mydata)[var_nm_check]
          } else {
            var_nm_check=vars$txt[ind]
            }
          print(nrow(mydata))
          vals_chosen_level<<-vals_chosen_level
          hierarchie<<-hierarchie
          var_nm_check<<-var_nm_check
          mydata=mydata[mydata[[var_nm_check]]%in%vals_chosen_level]
          print(nrow(mydata))
          
        }
      }
      
    # changeCols=metadata$hierarchies[[vars[vars$txt=="DONNEES",]$value]]%>%unlist
    # changeCols=fix_noms_duplicated(changeCols)
    changeCols=setdiff(names(mydata),variables_a_croiser())
    # print(changeCols)
    # changeCols=changeCols[changeCols%in%names(mydata)]
    mydata[,(changeCols):= lapply(.SD, function(x)as.numeric(as.character(x))), .SDcols = changeCols] 
    
    
      if(length(setdiff(variables_a_croiser(),input$join_vars))>0){
        mydata=mydata[,setdiff(names(mydata),setdiff(variables_a_croiser(),input$join_vars)),with=F][
        ,do.call(input$operation,.SD),by=eval(input$join_vars)]
        setnames(mydata,paste0("V",1:length(changeCols)),changeCols)
      print("grouping done !")
      }
 
    }

    mydata
  })
  
  output$list_fichiers=renderDT({
    datatable(files_infos[,c("title","folder_name","files","size"),with=F]
              ,filter=list(position = 'top'),
              extensions = c('FixedHeader'),
              class = "display",selection = 'single',rownames=F,              
              options = list(
                searchHighlight = TRUE,
                fixedHeader = TRUE,
                language = list(
                  info = 'Résultats _START_ à _END_ sur une liste de _TOTAL_.',
                  paginate = list(previous = 'Précédent', `next` = 'Suivant')),
                dom = "Bfrtip",
                scrollX=F,
                pageLength = 50
              ))
    })
  output$menu <- renderMenu(sidebarMenu(list(menuItem("Choix du fichier", 
                                                      tabName = "choix_du_fichier", 
                                                      icon = icon("book-reader"))),
                                        id = "tabs"))
  
  observeEvent(c(input$list_fichiers_cell_clicked),{
    req(length(input$list_fichiers_cell_clicked)>0)
    # print(input$list_fichiers_cell_clicked)
    print(path())
    # path=function(){return("../downloads//adeli-dieteticien18-tab1_80902373448054.csv")}
    
    my_list = list(menuItem("Choix du fichier", tabName = "choix_du_fichier", icon = icon("book-reader")))
    sub_items=list()
    #### CONSTRUCTION DU CHOIX HIERARCHIQUE SUR LES VARIABLES
    input_hierarchies=tagList()
    metadata=metadata()
    vars=rbind(metadata$vars_in_cols,metadata$vars_in_rows,metadata$vars_in_others)
    vars$value=as.character(vars$value)
    i=1
    for (ind in 1:nrow(vars)){
      var_ind=vars$value[ind]
      var_nm=vars$txt[ind]
      var_nm=gsub(" ","_",var_nm)
      hierarchie=metadata$hierarchies[[var_ind]]
      if(ncol(hierarchie)>1){
        input_hierarchies[[i]] = tagList()
        input_hierarchies[[i]][[1]] = selectizeInput(paste0("cols",var_nm), 
                                       paste("Hierarchie",vars$txt[ind]), 
                                       choices = 1:ncol(hierarchie))
        i=i+1
      }
    }
    #### CREATION DES MENUS
    sub_items[[1]]=menuSubItem(icon = NULL,tabName = "overview",
                               div(id="sheet_selection_div",
                                   selectizeInput("join_vars", "Groupement par variables",
                                           choices = variables_a_croiser(),selected = variables_a_croiser(),multiple=T,
                                           width = '95%'),
                                   selectizeInput("operation", "Fonction de regroupement",
                                          choices = c("sum","mean"),selected = variables_a_croiser(),
                                          width = '95%'),
                                   input_hierarchies
                                   
                                   ))
    sub_items[[2]]=menuSubItem(icon = NULL,
                               div(id="choix_graph_div",
                                   selectInput("choix_graph", "Quel graphique utiliser ?",selected = F,multiple=F,
                                            selectize=F,size=3,# https://stackoverflow.com/questions/24175997/force-no-default-selection-in-selectinput/26132906#26132906
                                            choices = c("Carte","Série temporelle","Barres"),
                                            width = '95%')))
    my_list[[2]] = menuItem(text = "Aperçu des données", 
                            icon = icon("table"),startExpanded = T,sub_items)    
    my_list[[3]]=menuItem(text = "Affichage de la visualisation", 
                               tabName = "Visualisation",icon = icon("chart-area"))
    

    output$menu <- renderMenu(sidebarMenu(my_list,id = "tabs"))
    updateTabItems(session = session,inputId = "tabs",selected = "overview")
  })
  output$overview=renderDT({
    
    mydata=mydata()
    vars_char=sapply(mydata,is.character)
    vars_char=names(vars_char)[vars_char]
    mydata[,(vars_char):=lapply(.SD,factor),.SDcols=vars_char]
    datatable(mydata   #Pour que les filtres soient plus pratiques
               ,filter=list(position = 'top'),
               extensions = c('FixedHeader'),
               options = list(
                 searchHighlight = TRUE,
                 fixedHeader = TRUE,
                 language = list(
                   info = 'Résultats _START_ à _END_ sur une liste de _TOTAL_.',
                   paginate = list(previous = 'Précédent', `next` = 'Suivant')),
                 dom = "Bfrtip",
                 scrollX=F,
                 pageLength = 50
               ),
               class = "display",selection = 'single',rownames=F
)})
  
  
  observeEvent(input$choix_graph=="Carte",{
    req(input$choix_graph=="Carte")
    # CHECK SI UNE SEULE VARIABLE DE CROISEMENT EST SELECTIONNEE, ELLE DEVRAIT ETRE UNE VAR GEO
    print(variables_a_croiser())
    print("do map")
    if(length(input$join_vars)==1){
      
      tryCatch({
      print("ok une seule variable")
      # metadata=metadata_IVT[[files_infos[files==gsub("../downloads/","",path())]$href]]
      # metadata$hierarchies$`4`
      mydata=mydata()
      metadata=metadata()
      vars=rbind(metadata$vars_in_cols,metadata$vars_in_rows,metadata$vars_in_others)
            unique(mydata[[input$join_vars]])
      mydata$dep_num=stringr::str_extract(pattern = "^([0-9]|A|B)+ -",
                              string = mydata[[input$join_vars]])%>%
        gsub(pattern = " -",replacement = "" )
      unique(mydata$dep_num)
      data_for_map=merge(mydata,FRA_dep_1pct@data,by.x="dep_num",by.y="CCA_2",all.y=T)
      data_for_map=data_for_map[,.SD[1],by="dep_num"]
      setorder(data_for_map,ordre)
      FRA_dep_1pct@data=data_for_map
      
      stats_to_display=metadata$hierarchies[[vars[grep("DONNEES",vars$txt),]$value]]%>%unlist%>%unname
      # var=input$indName
      var=stats_to_display[1]
      var%in%names(FRA_dep_1pct@data)
      steps=quantile(FRA_dep_1pct@data[[var]],0:8/8,na.rm=T)
      pal <- colorBin("YlOrRd", domain = var,bins=steps)
      
      title <- tags$div(
        tag.map.title, HTML(var)
      )  
      
      output$map=renderLeaflet({leaflet(FRA_dep_1pct) %>% 
        addTiles() %>%
        addPolygons(fillColor = as.formula(paste0("~pal(`",var,"`)")),fillOpacity = .5,
                    color= as.formula(paste0("~pal(`",var,"`)")),opacity = .2,
                    label=as.formula(paste0("~paste(NAME_2,`",var,"`,sep='\n')")))%>%
          addControl(title, position = "topleft", className="map-title")
          
      })
      removeUI(selector = "#map",immediate = T)
      insertUI(selector = "#dataviz_div",where = "afterBegin",ui = 
                 tags$div(id="map",leafletOutput("map",height=800)))
      updateTabItems(session = session,inputId = "tabs",selected = "Visualisation")
      },error=function(e){
        print(e)
        showModal(modalDialog(easyClose = T,size = "s",
                              title = "Erreur",
                              e
        ))
      })
      
      
    } else {
      showModal(modalDialog(easyClose = T,size = "s",
                            title = "Attention",
                            "Pour afficher une carte il faut sélectionner une seule variable de groupement et cette variable doit être une variable géographique (département, région...)"
      ))
      updateSelectInput(session = session,inputId = "choix_graph",selected = F)
    }
    
  })


}