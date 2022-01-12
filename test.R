library(shiny)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)


library(data.table)
library(dplyr)
library(DT)
library(plyr)

library(edeaR)
library(eventdataR)
library(bupaR)
library(processmapR)
library(processmonitR)
library(xesreadR)
library(pm4py)
library(reticulate)
library(DiagrammeR)

library(DiagrammeRsvg)
library(svgPanZoom)
library(ggplot2)
library(svglite)
library(ggiraph)
library(plotly)


sessionInfo()
# load the required packages
options(shiny.maxRequestSize = 50*1024^2)
# Add 'time_since_start' column to eventlog
add_time_since_case_start <- function(eventlog, units = "days") {
  # Determine timestamp variable from eventlog
  timestamp_var <- sym(bupaR::timestamp(eventlog))
  # Add time_since_start per event, since case start
  eventlog_extended <- eventlog %>% 
    bupaR::group_by_case() %>% 
    mutate(time_since_start = difftime(!!timestamp_var, min(!!timestamp_var, na.rm = TRUE), units = units)) %>% 
    bupaR::ungroup_eventlog()
  
  return(eventlog_extended)
}

server <- function(input, output,session) {
  ns <- session$ns
  eventlog <- reactiveValues(eventlog = NULL)
  
  ###################################
  ########### data tyoe #############
  ###################################  
  output$inputType <- renderUI({
    div(selectInput("inputType", "Select type import dataset",
                    choices =  c("File", "Example Dataset"),selected = "File"))
  })
  
  
  rawData <- reactive({
    if(input$inputType == 'File'){
      inFile <- read_xes(input$xesfile$datapath) 
      print("step1:readinputxes")
      write_xes(inFile, "./input/input_python.xes")
      print("step2:writeinputxes")
      rawData<-inFile
      
    }
    else if (input$inputType == 'Example Dataset'){
      data<- read_xes("./input/input_example.xes")
      print("step:readinputxesexample")
      rawData<-data
    }
    return(rawData)
  })
  
  
  rawModel <- reactive({
    observeEvent(input$inputBut, {
      inModel <- read_xes(input$modelfile$datapath) 
      rawData(inModel)
      
    })
    
  })
  
  
  ###################################
  ## log ##
  ###################################  
  output$logImport<-renderUI({
    div(fileInput(inputId="xesfile",
                  label = "Dataset to be uploaded",
                  multiple=FALSE, 
                  accept=c("xes",".xes")),
        id="xesFile")
  })
  
  ###################################
  ## model ##
  ###################################  
  
  output$modelImport<-renderUI({
    div(fileInput(inputId="modelfile",
                  label = "Model to be uploaded",
                  multiple=FALSE, 
                  accept=c("xes",".xes")),
        id="modelfile")
  })
  
  
  #####################
  ## Eventlog observers ##
  #####################
  observeEvent(input$xesfile,{
    uiOutput(outputId = ns("variable_selection_box"))
    print("step3:writeinputxes")
    
  })
  
  
  output$variable_selection_box <- renderUI({
    available_variables <- c("Please select...", colnames(rawData()))
    box(title = "Wrapping eventlog",status = "primary",solidHeader = TRUE,width = 12,
        style='  color: black;  font-size: 10px;',
        selectInput(inputId = "case_id_var", label = "Select case_id column", choices = available_variables, selected = "<none>"),
        selectInput(inputId = "timestamp_var", label = "Select timestamp column", choices = available_variables, selected = "<none>"),
        selectInput(inputId = "activity_var", label = "Select activity column", choices = available_variables, selected = "<none>"),
        selectInput(inputId = "resorce_id_var", label = "Select resource_id column", choices = available_variables, selected = "<none>")
    )
  })
  
  
  observeEvent(input$logButton,{
    showNotification("load eventlog done!", type = "message")
    req(rawData())
    req(input$case_id_var %in% colnames(rawData()))
    req(input$timestamp_var %in% colnames(rawData()))
    req(input$activity_var %in% colnames(rawData()))
    req(input$resorce_id_var %in% colnames(rawData()))
    print("DEBUG: generate_eventlog_from_upload_button event!")
    
    # Create eventlog from data
    eventlog <- rawData() %>%
      simple_eventlog(
        case_id = input$case_id_var,
        activity_id = input$activity_var,
        timestamp = input$timestamp_var,
        resource_id = input$resorce_id_var) %>%
      # Add time_since_start column to eventlog
      add_time_since_case_start()
    
    print(paste0("INFO: eventlog generated from data upload (containing ", nrow(eventlog), " lines)"))
  })
  
  
  
  
  observeEvent(input$inputBut_example,{
    
    # Create eventlog from data
    eventlog <- rawData() %>%
      simple_eventlog(
        case_id = input$case_id_var,
        activity_id = input$activity_var,
        timestamp = input$timestamp_var,
        resource_id = input$resorce_id_var) %>%
      # Add time_since_start column to eventlog
      add_time_since_case_start()
    
    print(paste0("INFO: eventlog example (containing ", nrow(eventlog), " lines)"))
  })
  
  
  
  
  
  
  
  ########################
  ####### observeEvent ###
  ########################
  
  # observeEvent(input$logButton, {
  #   #removeUI(selector="#xesFile", immediate=TRUE)
  #   removeUI(selector="#modelfile", immediate=TRUE)
  #   #removeUI(selector="#logButton", immediate=TRUE)
  #   #removeUI(selector="#variable_selection_box", immediate=TRUE)
  #   #removeUI(selector="#inputType", immediate=TRUE)
  #   
  #   #insertUI(selector="#cc_viewer", immediate=TRUE)
  #   
  #   
  # })
  # 
  # observeEvent(input$inputBut_example, {
  #   removeUI(selector="#inputBut_example", immediate=TRUE)
  #   #removeUI(selector="#inputType", immediate=TRUE)
  # })
  # 
  
  ########################
  ####### button#########
  ########################
  
  output$inputBut<-renderUI({
    actionButton(inputId = ns("logButton"),  class = "btn-warning",label = ">> Generate CC Vis <<")
  })
  
  output$inputBut_example<-renderUI({
    actionButton(inputId = ns("ExButton"),label = ">> Generate CC Vis <<")
  })  
  
  
  ########################
  ## CC_overview_pie_chart##
  ########################
  
  
  
  
  output$distPie_1 <- renderGirafe({
    
    donut_data <- data.frame(type = c("Blue", "Pink"), value = c(44, 73)) %>%
      mutate( percentage = value / sum(value),
              hover_text = paste0(type, ": ", value)
      ) %>%
      mutate(percentage_label = paste0(round(100 * percentage, 1), "%"))
    
    donut_plot <- ggplot(donut_data, aes(y = value, fill = type)) +
      geom_bar_interactive(
        aes(x = 1, tooltip = hover_text),
        width = 0.4,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = donut_data[["percentage_label"]][donut_data[["type"]] == "Pink"],
        size = 20,
        color = "BLACK"
      ) +
      scale_fill_brewer(palette="PuBuGn")+
      coord_polar(theta = "y") +
      theme_void()
    
    ggiraph(ggobj = donut_plot)
    
  })
  
  output$distPie_2 <- renderGirafe({
    
    donut_data <- data.frame(type = c("Blue", "Pink"), value = c(44, 73)) %>%
      mutate( percentage = value / sum(value),
              hover_text = paste0(type, ": ", value)
      ) %>%
      mutate(percentage_label = paste0(round(100 * percentage, 1), "%"))
    
    donut_plot <- ggplot(donut_data, aes(y = value, fill = type)) +
      geom_bar_interactive(
        aes(x = 1, tooltip = hover_text),
        width = 0.4,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = donut_data[["percentage_label"]][donut_data[["type"]] == "Pink"],
        size = 20,
        color = "BLACK"
      ) +
      scale_fill_brewer(palette="PuBuGn")+
      coord_polar(theta = "y") +
      theme_void()
    
    ggiraph(ggobj = donut_plot)
    
  })
  
  output$distPie_3 <- renderPlot({
    
    df = data.frame("Total" = c("Samsung","Huawei","Apple","Xiaomi","OPPO","Other"),
                    "share" = c(.2090,.1580,.1210,.0930,.0860,.3320))
    # Create a basic bar
    ggplot(df, aes(x="", y=share, fill=Total)) + 
      geom_bar(stat="identity", width=1)+
      scale_fill_brewer(palette="PuBuGn")+
      coord_polar(theta = "y")+
      theme_void()
  })
  
  
  
  ########################
  ########################
  ######## cc_map#########
  ########################
  ########################
  
  
  output$cc_map <- renderSvgPanZoom({
    
    
    py_input <- reactiveValues({
      
      if(input$inputType == 'Example Dataset'){
        xes_importer <- reticulate::import("pm4py.objects.log.importer.xes")$importer
        log <- xes_importer$apply('./input/input_example.xes')
        py_input  <- log
      }
      else if (input$inputType == 'File') {
        xes_importer <- reticulate::import("pm4py.objects.log.importer.xes")$importer
        log <- xes_importer$apply('./input/input_python.xes')
        py_input  <- log
      }
      return(py_input)
    })
    
    require(py_input)
    pm4py_inductive <- reticulate::import("pm4py.algo.discovery.inductive.algorithm")
    pn <- pm4py_inductive$apply(r_to_py(py_input))
    
    str(pn)
    
    viz <- reticulate::import("pm4py.visualization.petri_net")$visualizer
    
    # Convert back to Python
    py_pn <- r_to_py(pn[[1]])
    py_im <- r_to_py(pn[[2]])
    py_fm <- r_to_py(pn[[3]])
    
    # Render to DOT with PMP4Y
    dot <- viz$apply(py_pn,py_im,py_fm,variant=viz$Variants$FREQUENCY, log=r_to_py(log))$source
    
    svgPanZoom(export_svg(grViz(diagram = dot)),controlIconsEnabled = TRUE,minZoom = 0.5,viewBox = FALSE)
    
  })
  
  
  
  
  
  ########################
  ########################
  ######## cc_graph ######
  ########################
  ########################
  
  output$cc_graph <- renderPlotly({
    
    processmapR::plotly_dotted_chart(eventlog)
    
    
    
    
    #reticulate::source_python("timeline.py")
    #list(src = "myplot.png")
  })
  
  
  ########################
  ########################
  ######## cc_table ######
  ########################
  ########################
  output$cc_table <- renderUI({
    output$datatable <- renderDT(options = list(scrollX=TRUE),{rawData()})
    dataTableOutput(outputId = "datatable")
  })
}