function(input,output,session){
  # path=function(){paste0("../downloads/","2014-01-31-ehpa-cube3-basereduite-v1-3_80951515546379.csv")}#files_infos$files[2])}
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
    data_names2=mydata[2]%>%unname%>%unlist
    variables_a_croiser=data_names2
    variables_a_croiser=variables_a_croiser[!variables_a_croiser==""]
    variables_a_croiser
  })
  fulldata=reactive({
    req(path())
    mydata=fread(path(),header = F,stringsAsFactors = F)
    # print(head(mydata))
    data_names1=mydata[1]%>%unname%>%unlist
    data_names2=mydata[2]%>%unname%>%unlist
    data_names2[data_names2==""] <- data_names1[data_names2==""]
    mydata=fread(path(),header = F,stringsAsFactors = F,skip = 3)
    names(mydata) <- as.character(data_names2)
    mydata=mydata%>%data.table
  })

  
  mydata=reactive({
    req(fulldata())
    mydata=fulldata()
    mydata2<<-fulldata()
    if (!(is.null(input$operation)|is.null(input$join_vars))){
      metadata=metadata()
      vars=rbind(metadata$vars_in_cols,metadata$vars_in_rows,metadata$vars_in_others)
      vars_hc<<-vars
      vars$value=as.character(vars$value)
      for (ind in 1:nrow(vars)){
        var_ind=vars$value[ind]
        var_nm=vars$txt[ind]
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
          mydata=mydata[mydata[[var_nm_check]]%in%vals_chosen_level]

          print(nrow(mydata))
          
        }
      }
      
    changeCols=metadata$hierarchies[[vars[vars$txt=="DONNEES",]$value]]%>%unlist
    print(changeCols)
    changeCols=changeCols[changeCols%in%names(mydata)]
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
  
  output$list_fichiers=renderDT(datatable(files_infos[,c("title","folder_name","files","size")]
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
  ))
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
                               div(id="choix_graph_div",selectInput("choix_graph", "Quel graphique utiliser ?",selected = F,multiple=F,
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
  


}