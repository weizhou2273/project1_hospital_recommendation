library(shiny)
library(leaflet)
library(shinydashboard)
library(rgdal)
library(ggplot2)
library(dplyr)
library(scales)


header <- dashboardHeader(
  title = "Medicare Explorer"
)

menu <-sidebarMenu(
  menuItem("Finds your hospital", tabName = "Finds_your_hospital", icon = icon("th")),
  menuItem("Explore Medicare", tabName = "Explore_Medicare", icon = icon("dashboard"))
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  tabItems(
    
    # First tab content
    tabItem(tabName = "Explore_Medicare",
            fluidRow(  
             column(width = 10,
              tabBox(width = NULL,
                title = "Analysis",
                tabPanel("Map",
                         fluidRow(width ="100%",
                                  box(selectInput("color","Color",color)),
                                  box(selectInput("size","size",size))
                                  ),
                         leafletOutput("map",width="100%",height="600px"),width = NULL),
                
                tabPanel("Medicare payment analytics",width = NULL,
                    fluidRow(width="100%",
                             box(selectInput("x_axis","X Axis",x_axis)),
                             box(selectInput("y_axis","Y Axis",y_axis))),
                    plotOutput("scatterplot",height="650px")
                    ),
                    
                tabPanel("Boxplot",width = NULL,
                    fluidRow(width="100%",
                             box(selectInput("x_axis_bp","X Axis",x_axis_bp)),
                             box(selectInput("y_axis_bp","Y Axis",x_axis))  ),
                    plotOutput("boxplot",width="100%",height="650px"))
            ) 
            )
                ,
              
              box(
                title = "Controls",width = 2,
                selectizeInput("DRG1","Chooose your Diagnosis related Group",DRG)
                )
              
              )
            ),

    # Second tab content
    tabItem(tabName = "Finds_your_hospital",
            fluidRow(
              column(width =4, 
                     box(
                       title = "Preference control",
                       background="navy",
                       collapsible = TRUE,
                       width = NULL,
                       selectizeInput("DRG","Chooose your Diagnosis related Group",DRG),
                       textInput("ur_location","Your location", value ="500 8th Ave, New York, NY 10018", placeholder=NULL),
                       numericInput("radius_mile","Search hospitals within X miles around your", value = 10, min=0, max=10000,step=5),
                       checkboxGroupInput("quality_concern","I want hospitals with X stars of quality care", quality_concern,selected = c("3","4","5"),inline = TRUE),
                       actionButton("refresh", "Refresh now")
                       ),
                     box(title = "Search results",
                         # background="blue",
                         collapsible = TRUE,
                         DT::dataTableOutput('hospital_in_cycle'),
                         width = NULL)
              ),
              
              column(title = 'Quality V.S. Cost',width = 8,
                     box(infoBoxOutput("out_of_pocket_info"),
                         infoBoxOutput("quality_index"),
                         width = NULL),
                      box(title="Hospital locator", status ="primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                          height = 12,
                          leafletOutput("map2",height="520px")
                          )
              )
              
              )
           
              
    )
  ))

##############Overall structure################
dashboardPage(
  header,
  dashboardSidebar(menu),
  body
)
