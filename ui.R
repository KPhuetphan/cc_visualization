library(shiny)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)


library(data.table)
library(DT)
library(plyr)
library(dplyr)

library(edeaR)
library(eventdataR)
library(bupaR)
library(processmapR)
library(processmonitR)
library(xesreadR)
library(pm4py)
library(reticulate)
library(DiagrammeR)

#library(bpmn)

library(DiagrammeRsvg)
library(svgPanZoom)
library(ggplot2)
library(svglite)
library(ggiraph)
library(plotly)

buttonWidth <- 220
sideBarWidth <- 250

sessionInfo()
# load the required packages
#options(shiny.maxRequestSize = 50*1024^2)

######################
## Helper functions ##
######################
js <- paste(
  "$(document).on('keydown', function(event){",
  "  var key = event.which;",
  "  if(key === 37){",
  "    Shiny.setInputValue('arrowLeft', true, {priority: 'event'});",
  "  } else if(key === 39){",
  "    Shiny.setInputValue('arrowRight', true, {priority: 'event'});",
  "  }",
  "});"
)

jsCode <- "
shinyjs.init = function(){
          $('#analysis li a[data-value=analysis_setup_tab]').hide();
}
"

header <- dashboardHeader(title = "CC Vistualization")
siderbar <- dashboardSidebar(
             wellPanel( uiOutput(outputId="logImport"),
                        uiOutput(outputId="variable_selection_box"),
                        uiOutput(outputId="modelImport"),
                        div(style='position:relative;left:30px;',
                            uiOutput(outputId="inputBut"))
                        ),
                        div(style='position:absolute;left:30px; bottom:2px;',
                            p("CC Vistualization by Krongkran P")
                        )
                        
)
 
###################################
####### Conformance Process ##
###################################

body <-  dashboardBody(
  tags$head(tags$link(rel = "stylesheet",type = "text/css",href = "styleDefinitions.css")),
  fluidRow(
          shinydashboard::valueBoxOutput(outputId = "progressBox1",width = 3),
          shinydashboard::valueBoxOutput(outputId = "progressBox2",width = 3),
          shinydashboard::valueBoxOutput(outputId = "progressBox3",width = 3),
          shinydashboard::valueBoxOutput(outputId = "progressBox5",width = 3)
          ),
  fluidRow(
    column(width=12,tags$div(class = 'main',style='margin-top:4em',
      tabsetPanel(id = "Main",
        tabPanel(value = "tab1",title="Conformance Process",icon = icon("chart-pie"),
          fluidRow(width=12, 
              column(width=12, offset = 0, style='padding:0px; margin: 0; margin-right: 0',
                box(title = "Conformance Map",status = "success",solidHeader = TRUE,width = 12,
                closable = FALSE,height = 550,shinycssloaders::withSpinner(svgPanZoomOutput(outputId ="Cc_map")))),
              column(width=12,offset = 0, style='padding:0px; margin-left: 0; margin-right: 0; margin-bottom: 0; ',
                box(title = "Conformance Table",status="primary",solidHeader = TRUE,width = 12,side = "right",dataTableOutput(outputId = "cc_table")))
                  )
                ),
###################################
####### Root Cause Proces #########
###################################
  tabPanel(value = "tab2",title="Root Cause Process",icon = icon("search"),
        fluidRow(width=12, 
           column(width=12,offset = 0, style='padding:0px; margin-left: 0; margin-right: 0; margin-bottom: 0;',
                        box(title = "Root Cause Trace",width = 12,solidHeader = TRUE,status = "primary",
                            svgPanZoomOutput(outputId = "rc_table"))
                 ),
           column(width=12, offset = 0, style='padding:0px; margin: 0; margin-right: 0;',
                           box(title = "Non-Conformance Table ",status = "primary",solidHeader = TRUE,width = 12,
                               closable = FALSE,shinycssloaders::withSpinner(dataTableOutput(outputId ="Rc_map")))
                 )
           
           
           )
        ),
  tabPanel(value = "tab4",title="Conformance Graph",icon = icon("chart-bar"),
           fluidRow(width=12,
                    column(width=12, 
                    tabBox(
                      title = "Conformance Graph",width = 12,height = 650,side = "right",
                      tabPanel(title = "Timeline",icon = icon("hourglass-half"),shinycssloaders::withSpinner(plotlyOutput("cc_graph"))),
                      tabPanel(title = "Performance",icon = icon("file-signature"),shinycssloaders::withSpinner(plotlyOutput("cc_graph_2")))
                    ))
                    
                    )
  ),

           
  tabPanel(value = "tab3",title="Raw input",icon = icon("database"),
           fluidRow(width=12, 
            box(title = "Dataset (input1)",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 6,
                uiOutput(outputId = "dataset_input")),
           box(title = "Petri Net model (input2)", status = "primary", solidHeader = TRUE,collapsible = TRUE,width = 6,
              (svgPanZoomOutput("Pe_map",height = 580)))
           )
           )
      ))
    )

))
    
ui <- dashboardPage(header, siderbar, body , skin='purple' )