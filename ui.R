dashboardPage(
  tags$header(class = "main-header", span(class = "logo", "Data.DREES_viz"),
              tags$nav(class = "navbar navbar-static-top",
                       role = "navigation", span(shiny::icon("bars"), style = "display:none;"),
                       a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas",
                         role = "button", span(class = "sr-only", "Toggle navigation")),
                       div(class = "navbar-custom-menu",
                           tags$ul(class = "nav navbar-nav",
                                   tags$li(id="doc_click",
                                           a(tags$i(class = "fa fa-book text-success"), "Le projet")),
                                   
                                   tags$li(id="Github",
                                           a(tags$i(class = "fa fa-github text-success"),"Code Source", 
                                             href="https://github.com/phileas-condemine/")
                                   )))
              )),
  dashboardSidebar(
    sidebarMenuOutput("menu")
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "choix_du_fichier",
              fluidRow(
                div(id="fichiers_dispos",style="width:95%;margin-left:20px; margin-right:20px",
                    withSpinner(dataTableOutput("list_fichiers"),size = 2)
                ))),
      tabItem(tabName = "overview",
              fluidRow(
                div(id="apercu_data",style="width:95%;margin-left:20px; margin-right:20px",
                    withSpinner(dataTableOutput("overview"),size = 2)
                )))
    )
  ),
  title = "CUBES VIZ"
)