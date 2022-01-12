library(shiny)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(flexdashboard)

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
library(petrinetR)

library(XML)
library(xml2)

library(DiagrammeRsvg)
library(svgPanZoom)
library(ggplot2)
library(svglite)
library(ggiraph)
library(plotly)

library(purrr)



######################
## Helper functions ##
######################

sessionInfo()
# load the required packages
options(shiny.maxRequestSize = 50*1024^2)

# Add 'time_since_start' column to eventlog
add_time_since_case_start <- function(event_log, units = "days") {
  # Determine timestamp variable from Event_log
  timestamp_var <- sym(bupaR::timestamp(event_log))
  # Add time_since_start per event, since case start
  Event_log_extended <- event_log %>%
    bupaR::group_by_case() %>%
    mutate(time_since_start = difftime(!!timestamp_var, min(!!timestamp_var, na.rm = TRUE), units = units)) %>%
    bupaR::ungroup_eventlog()

  return(Event_log_extended)
}

######################
## server functions ##
######################

server <- function(input, output,session) {
  ns <- session$ns

###################################
############  Input data   ########
###################################  
  
### Input Data ###
rawData <- eventReactive(input$inputFile,{
        tryCatch(
          {

            endsWith(input$inputFile$name, '.xes')
            ### R ####
            path <- input$inputFile$datapath
        
            
            ### python ####
            xes_importer <- reticulate::import("pm4py.objects.log.importer.xes")$importer
            variant = xes_importer$Variants$ITERPARSE

            log <- xes_importer$apply(path,variant=variant)
            
            tryCatch({
              attributes_filter <- reticulate::import("pm4py.algo.filtering.log.attributes")$attributes_filter
              log <- attributes_filter$apply_events(log, list("complete"), parameters=list("pm4py:param:attribute_key"="lifecycle:transition", positive=TRUE))
            }, error=function(cond) {
              
            })
            
            
            convert_to_dataframe <- reticulate::import("pm4py")$convert_to_dataframe
            rawData_r_input_raw <<- convert_to_dataframe(log)
            tryCatch({
              rawData_r_input_raw[["time:timestamp"]] <<- as.POSIXct(unlist(map(rawData_r_input_raw[["time:timestamp"]], ~ py_to_r(.))), origin = "1960-01-01")
            }, error=function(e) {})
            rawData_r_input <<- rename_with(rawData_r_input_raw, ~ gsub(":", "_", .x, fixed = TRUE))
            logg <<- log
            
            #logg2 <<- log
            rawData_py_input <<- log#[log$lifecycle:transition == "complete", ]
            #print("Python read file .xes")
                
            },
          error = function(e) {
          stop(safeError(e))
          }
        )
        return(rawData_r_input)
        #return(rawData_py_input)
      })
observeEvent(rawData(), {
    updateTabsetPanel(session,"Main",selected = "tab3")
  })

### Input Model  ###
rawModel <- reactive({
  req(input$modelFile)
  tryCatch(
    {
      #endsWith(input$modelFile$name, '.pnml')
      ### R ####
      mpath <- input$modelFile$datapath
      #model <- xml2::read_xml(mpath)
      #print("R read.pnml")
      #write_xml(model, "./input/model.pnml")
      #print("R write model .pnml")
      
      pnml_importer <- reticulate::import("pm4py.objects.petri_net.importer")$importer
      #rawPnml<- pnml$apply({"./input/model.pnml"})
      rawPnml<- pnml_importer$apply(mpath)
      print("Create .pnml")
      
      rawModel_r_input <<-rawPnml
      pn <<-rawPnml
      
      
      str(pn)
      
      py_pn <<- r_to_py(pn[[1]])
      py_im <<- r_to_py(pn[[2]])
      py_fm <<- r_to_py(pn[[3]])
      print("import .pnml to pn")
      
     
    },
    error = function(e) {
      stop(safeError(e))
    }
  )
  return(pn)
})

aligned_traces <- eventReactive(input$logButton,{
  req(rawData())
  req(rawModel())
  tryCatch(
    {
      alignments <- reticulate::import("pm4py.algo.conformance.alignments.petri_net")$algorithm
      
      
      cc<<-alignments$apply_log(r_to_py(rawData_py_input),py_pn,py_im,py_fm)
    },
    error = function(e) {
      stop(safeError(e))
    }
  )
  return(cc)
})

log_fitness <- reactive({
  req(aligned_traces())
  tryCatch(
    {
      trc <- aligned_traces()
      
      replay_fitness <- reticulate::import("pm4py.algo.evaluation.replay_fitness")$algorithm
      log_fitness_result <- replay_fitness$evaluate(trc, variant=replay_fitness$Variants$ALIGNMENT_BASED)
    },
    error = function(e) {
      stop(safeError(e))
    }
  )
  return(log_fitness_result)
})

#root_cause <- reactive({
#  req(aligned_traces())
#  source_python("source.py")
#  rca <<- do_root_cause_analysis(r_to_py(rawData_py_input), py_pn, py_im, py_fm)
#})


###################################
############ plot panel ###########
###################################  
  
output$logImport<-renderUI({
    div(fileInput(inputId="inputFile",
                  label = "Dataset to be uploaded(.xes)",
                  multiple=FALSE,
                  accept=c("xes",".xes")))
})
  
output$inputBut<-renderUI({
    actionButton(inputId = ns("logButton"),  class = "btn-gen", label = div("Generate Visual  ", icon("magic")))
    
  })

output$modelImport<-renderUI({
  div(fileInput(inputId="modelFile",
                label = "Model to be uploaded(.pnml)",
                multiple=FALSE,
                accept=c("pnml",".pnml")))
})
output$variable_selection_box <- renderUI({
    available_variables <- c("Please select...", colnames(rawData()))
      box(title = "Wrapping eventlog",status = "primary",solidHeader = TRUE,collapsible = T,width = 12, 
          style='  color: black;  font-size: 12px;margin-bottom: 0; margin-left: 0; margin-right: 0;',
          selectInput(inputId = "colSelectActivity", label = "Select activity ", choices = available_variables, selected = "<none>"),
          selectInput(inputId = "colSelectCase", label = "Select case id ", choices = available_variables, selected = "<none>"),
          selectInput(inputId = "colSelectTimestamp", label = "Select timestamp ", choices = available_variables, selected = "<none>"),
          selectInput(inputId = "colSelectResourceID", label = "Select resource id ", choices = available_variables, selected = "<none>")
        )
    })
  
###################################
######## wrapping Eventlog  #######
###################################  

event_log <- eventReactive(input$logButton, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Computing plot values", value = 0)
    n <- 10
    for (i in 1:n) {
      progress$inc(1/n, detail = paste(i*10,"% completed"))
      Sys.sleep(0.5)
    }
    progress$set(message = "Plot values computed successfully!")

    req(rawData())
    req(input$colSelectCase %in% colnames(rawData()))
    req(input$colSelectActivity %in% colnames(rawData()))
    req(input$colSelectTimestamp %in% colnames(rawData()))
    req(input$colSelectResourceID %in% colnames(rawData()))
    print("DEBUG: generate_eventlog_from_upload_button event!")
    
    rawDataForEventLog <<- rawData()
    
    Event_log <- rawDataForEventLog %>%
      simple_eventlog(
        case_id = rlang::as_string(input$colSelectCase),
        activity_id = rlang::as_string(input$colSelectActivity),
        timestamp = rlang::as_string(input$colSelectTimestamp),
        resource_id = rlang::as_string(input$colSelectResourceID)) %>% 
      # Add time_since_start column to eventlog
      add_time_since_case_start()
    
    print(paste0("INFO: eventlog generated from data upload (containing ", nrow(Event_log), " lines)"))
    return(Event_log)
  })
  
#######################################################################################################################
######################################## Raw input View ############################################################### 
#######################################################################################################################

###############################
######## Raw input table ######
###############################
  
output$dataset_input <- renderUI({

  req(rawData())
  output$datatable <<- renderDT(options = list(scrollX=TRUE),{sapply(rawData(), as.character) })
  dataTableOutput(outputId = ns("datatable"))
})
  
#######################################################################################################################
######################################## Conformance View ############################################################# 
#######################################################################################################################
  
########################
##### CC__pie_chart#####
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

  df = data.frame("Total" = c("a","b","c","d","e","f"),
                  "share" = c(.2090,.1580,.1210,.0930,.0860,.3320))
  
  # Create a basic bar
  ggplot(df, aes(x="", y=share, fill=Total)) + 
             geom_bar(stat="identity", width=1)+
             scale_fill_brewer(palette="PuBuGn")+
             coord_polar(theta = "y")+
             theme_void()
})

########################
##### progressBox######
########################

output$progressBox1 <- shinydashboard::renderValueBox({
  req(aligned_traces())
  trc <- aligned_traces()
  val <- length(Filter(function(t) t$fitness == 1, trc))
  shinydashboard::valueBox( 
    val,
    "Conforming case",
    icon = icon("thumbs-up"),
    color = "olive"
  )
})
output$progressBox2 <- shinydashboard::renderValueBox({
  trc <- aligned_traces()
  val <- as.integer(length(Filter(function(t) t$fitness == 1, trc)) / length(trc) * 100)
  shinydashboard::valueBox(
    paste0(val,"%"),
    "Total Conforming case",
    icon = icon("balance-scale"),
    color = "yellow"
    )
  
})

output$progressBox3 <- shinydashboard::renderValueBox({
  req(aligned_traces())
  trc <- aligned_traces()
  val <- length(Filter(function(t) t$fitness < 1, trc))
  div(shinydashboard::valueBox(
    val,
    "NON-Conforming case",
    icon = icon("thumbs-down"),
    color = "red"
    ,width = NULL
    ))
  
})
output$progressBox4 <- shinydashboard::renderValueBox({
  req(aligned_traces())
  trc <- aligned_traces()
  val <- length(Filter(function(t) t$fitness < 1, trc))
  div(shinydashboard::valueBox(
    val,
    "Total case",
    icon = icon("thumbs-down"),
    color = "blue"
    #,width = 7
    ))
  
})
output$progressBox5 <- shinydashboard::renderValueBox({
  req(log_fitness())
  #val <- as.character(log_fitness)
  shinydashboard::valueBox(
    textOutput("log_fitness", inline = TRUE),
    "Fitness",
    icon = icon("cat"),
    color = "blue"
    ,width = NULL
    )

})


output$percFitTraces <- renderText({
  req(log_fitness())
  lf <- log_fitness()
  lf$percFitTraces
})

output$averageFitness <- renderText({
  req(log_fitness())
  lf <- log_fitness()
  lf$averageFitness
})

output$percFitTraces <- renderText({
  req(log_fitness())
  lf <- log_fitness()
  lf$percFitTraces
})

output$percentage_of_fitting_traces <- renderText({
  req(log_fitness())
  lf <- log_fitness()
  lf$percentage_of_fitting_traces
})

output$average_trace_fitness <- renderText({
  req(log_fitness())
  lf <- log_fitness()
  lf$average_trace_fitness
})

output$log_fitness <- renderText({
  req(log_fitness())
  lf <- log_fitness()
  lf$log_fitness
})

########################
######## cc_graph ######
########################
output$cc_graph <- renderPlotly({
req(event_log())
processmapR::plotly_dotted_chart(event_log())
})

output$cc_graph_2 <- renderPlotly({
req(event_log())
  
#activity_var <- sym(bupaR::activity_id(event_log()))
#  event_log() %>% 
#    bupaR::group_by_activity() %>% 
#    bupaR::n_cases() %>% 
#    {ggplot(., aes(x = !!activity_var, y = n_cases)) +
#        geom_col(fill = "skyblue2") +
#        ylab('Number of cases') +
#        coord_flip()} %>% 
#    ggplotly()

  event_log() %>%
    activity_frequency("activity") %>%
    plot()
}) 
  
########################
######## cc_table ######
########################
output$cc_table <- renderDataTable({
    req(aligned_traces())
    
    trc <- aligned_traces()
  
    max_len <<- max(unlist(map(trc, function(t) length(t$alignment))), 0)
    
    if(max_len == 0) {
      return(datatable(options = list(scrollX=TRUE, pageLength = 8),{sapply(as.data.frame(list(Case=c("-","-"),Type=c("-","-"),Fitness=c("-","-"))), as.character)}))
    }
    
    tab <<- list(Case=c(),Type=c(),Fitness=c())
    for(i in 1:max_len) {
      tab[[as.character(i)]] <<- c()
    }
    
    for(i in 1:length(trc)) {
      trcc <- trc[[i]]
      alig <- trcc$alignment
      
      tab$Case <<- c(tab$Case, as.character(i))
      tab$Case <<- c(tab$Case, as.character(i))
      tab$Type <<- c(tab$Type, "Actual")
      tab$Type <<- c(tab$Type, "Expected")
      tab$Fitness <<- c(tab$Fitness, trcc$fitness)
      tab$Fitness <<- c(tab$Fitness, "")
      
      for(j in 1:max_len) {
        if(j <= length(alig)) {
          tab[[as.character(j)]] <<- c(tab[[as.character(j)]], ifelse(is.null(alig[[j]][[1]]), "", alig[[j]][[1]]))
          tab[[as.character(j)]] <<- c(tab[[as.character(j)]], ifelse(is.null(alig[[j]][[2]]), "", alig[[j]][[2]]))
        } else {
          tab[[as.character(j)]] <<- c(tab[[as.character(j)]], "")
          tab[[as.character(j)]] <<- c(tab[[as.character(j)]], "")
        }
      }
      #tab$Actual <<- c(tab$Actual, ifelse(is.null(alig[[i]][[1]]), "", alig[[i]][[1]]))
      #tab$Expected <<- c(tab$Expected, ifelse(is.null(alig[[i]][[2]]), "", alig[[i]][[2]]))
      #tab[[as.character(i)]] <<- c(alig[[i]][[1]])
    }
    tab_df <- as.data.frame(tab)
    datatable(options = list(scrollX=TRUE, pageLength = 8),{sapply(tab_df, as.character) }) %>%
      formatStyle(names(tab_df),"white-space"="nowrap") %>%
      formatStyle(
        'Fitness',
        color = 'white',
        backgroundColor = styleInterval(0.999, c('red', 'green'))
      )
    
    #list()
    
    ##cc[[1]]$alignment
    #head(cc)
    #cc[[1]]$alignment
    
    #df <- head(cc)
    # df %>% relocate(f)head(as.matrix(cc))
    
    
    #evaluation_all <- reticulate::import("pm4py.algo.evaluation.replay_fitness")$algorithm
    
    #quality <<- evaluation_all$apply(r_to_py(rawData_py_input),py_pn,py_im,py_fm)
    #print(quality)
  
  
  
  
})

output$Rc_map <- renderDataTable({
  req(aligned_traces())
  
  trc <- aligned_traces()
  trc <- Filter(function(t) t$fitness != 1, trc)
  
  max_len <<- max(unlist(map(trc, function(t) length(t$alignment))), 0)
  
  if(max_len == 0) {
    return(datatable(options = list(scrollX=TRUE, pageLength = 8),{sapply(as.data.frame(list(Case=c("-","-"),Type=c("-","-"),Fitness=c("-","-"))), as.character)}))
  }
  
  tab <<- list(Case=c(),Type=c(),Fitness=c())
  for(i in 1:max_len) {
    tab[[as.character(i)]] <<- c()
  }
  
  for(i in 1:length(trc)) {
    trcc <- trc[[i]]
    alig <- trcc$alignment
    
    tab$Case <<- c(tab$Case, as.character(i))
    tab$Case <<- c(tab$Case, as.character(i))
    tab$Type <<- c(tab$Type, "Actual")
    tab$Type <<- c(tab$Type, "Expected")
    tab$Fitness <<- c(tab$Fitness, trcc$fitness)
    tab$Fitness <<- c(tab$Fitness, "")
    
    for(j in 1:max_len) {
      if(j <= length(alig)) {
        tab[[as.character(j)]] <<- c(tab[[as.character(j)]], ifelse(is.null(alig[[j]][[1]]), "", alig[[j]][[1]]))
        tab[[as.character(j)]] <<- c(tab[[as.character(j)]], ifelse(is.null(alig[[j]][[2]]), "", alig[[j]][[2]]))
      } else {
        tab[[as.character(j)]] <<- c(tab[[as.character(j)]], "")
        tab[[as.character(j)]] <<- c(tab[[as.character(j)]], "")
      }
    }
    #tab$Actual <<- c(tab$Actual, ifelse(is.null(alig[[i]][[1]]), "", alig[[i]][[1]]))
    #tab$Expected <<- c(tab$Expected, ifelse(is.null(alig[[i]][[2]]), "", alig[[i]][[2]]))
    #tab[[as.character(i)]] <<- c(alig[[i]][[1]])
  }
  tab_df <- as.data.frame(tab)
  datatable(options = list(scrollX=TRUE, pageLength = 8),{sapply(tab_df, as.character) }) %>% formatStyle(names(tab_df),"white-space"="nowrap")
  
  #list()
  
  ##cc[[1]]$alignment
  #head(cc)
  #cc[[1]]$alignment
  
  #df <- head(cc)
  # df %>% relocate(f)head(as.matrix(cc))
  
  
  #evaluation_all <- reticulate::import("pm4py.algo.evaluation.replay_fitness")$algorithm
  
  #quality <<- evaluation_all$apply(r_to_py(rawData_py_input),py_pn,py_im,py_fm)
  #print(quality)
  
  
  
  
})

#renderSvgPanZoom({
  #req(rawData())
  #req(rawModel())
  
  #token_based_replay <- reticulate::import("pm4py.algo.conformance.tokenreplay")$algorithm
  #parameters_tbr <- r_to_py(list(disable_variants= TRUE, enable_pltr_fitness= TRUE))
  #res <<- token_based_replay$apply(rawData_py_input, py_pn, py_im, py_fm, parameters=parameters_tbr)
  #replayed_traces <<- res[[1]]
  #place_fitness <<- res[[2]]
  #trans_fitness <<- res[[3]]
  #unwanted_activities <<- res[[4]]
  #print(1)
  #string_attributes <- list("org:group")
  #numeric_attributes <- list()
  #parameters <<- r_to_py(list(string_attributes= string_attributes, numeric_attributes= numeric_attributes))
  #root_cause_analysis <- reticulate::import("pm4py.algo.conformance.tokenreplay.diagnostics")$root_cause_analysis
  #trans_root_cause <<- root_cause_analysis$diagnose_from_trans_fitness(rawData_py_input, r_to_py(trans_fitness), parameters=parameters)
  #print(3)
  #act_root_cause <<- root_cause_analysis$diagnose_from_notexisting_activities(rawData_py_input, r_to_py(unwanted_activities), parameters=parameters)
  
  #req(root_cause())
  #res <- root_cause()
  #trans_root_cause <<- res$trans
  #act_root_cause <<- res$act
  
#  list()
  
  #dot <<- trans_root_cause[["reinitiate request"]]$source
  #dot <<- act_root_cause[["pay compensation2"]]$source
  #dot <<- trans_root_cause[["examine casually"]]$source
  
  #svgPanZoom(export_svg(grViz(diagram = dot)),controlIconsEnabled = TRUE,minZoom = 0.5,viewBox = FALSE)
  
  
  #dt_vis <- reticulate::import("pm4py.visualization.decisiontree")$visualizer
  #trans <<- trans_root_cause[[1]]
  #clf <- trans_root_cause[[trans]][["clf"]]
  #print(5)
  #feature_names <- trans_root_cause[[trans]][["feature_names"]]
  #classes <- trans_root_cause[[trans]][["classes"]]
  # visualization could be called
  #gviz <<- dt_vis$apply(clf, feature_names, classes)
  #dt_vis$view(gviz)

  
  #log_to_features <- reticulate::import("pm4py.algo.transformation.log_to_features")$algorithm
  #res <<- log_to_features$apply(rawData_py_input)
  #fdata <<- res[[1]]
  #feature_names <<- res[[2]]
  #get_class_representation <- reticulate::import("pm4py.objects.log.util")$get_class_representation
  #res <- get_class_representation$get_class_representation_by_trace_duration(rawData_py_input, 2 * 8640000)
  #target <<- res[[1]]
  #classes <<- res[[2]]
  #tree <- reticulate::import("sklearn")$tree
  #clf <- tree$DecisionTreeClassifier()
  #pd <- reticulate::import("pandas")
  #print(1)
  #dataframe = pd$DataFrame(fdata, columns=feature_names)
  #print(2)
  #clf$fit(fdata, target)
  #print(3)
  #dectree_visualizer <- reticulate::import("pm4py.visualization.decisiontree")$visualizer
  #gviz <<- dectree_visualizer$apply(clf, feature_names, classes)
#})

########################################################################## python ############################################################################################################

###################################
######## Raw input View ###########
###################################
################################################################################################
##################################### Raw map pe_map ###########################################
################################################################################################

output$Pe_map <- renderSvgPanZoom({
  print("IN_PE_MAP")
  req(rawModel())
  print("AVAIL_PE_MAP")
  
  viz <- reticulate::import("pm4py.visualization.petri_net")$visualizer
   #Render to DOT with PMP4Y 
  dot <- viz$apply(py_pn,py_im,py_fm,log=r_to_py(rawData_py_input))$source
  svgPanZoom(export_svg(grViz(diagram = dot)),controlIconsEnabled = TRUE,minZoom = 0.5,viewBox = FALSE)
  
  

  
})

#####################################
######## Conformance View ###########
#####################################



################################################################################################
##################################### cc_map ###################################################
################################################################################################
observeEvent(event_log(), {
  updateTabsetPanel(session,"Main",selected = "tab1")
})

observeEvent(aligned_traces(), {
  updateTabsetPanel(session,"Main",selected = "tab1")
})





output$Cc_map <- renderSvgPanZoom({
  print("IN_CCMAP")
  req(rawModel())
  req(rawData())
  print("INPUT_AVAIL")
  viz <- reticulate::import("pm4py.visualization.petri_net")$visualizer
  #Render to DOT with PMP4Y 
  dot <- viz$apply(py_pn,py_im,py_fm,variant=viz$Variants$ALIGNMENTS, log=r_to_py(rawData_py_input))$source
  
  #grVizToPNG(diagram = dot,"/output/cc.png")
  
  cc_g = grViz(diagram = dot)
  cc_g = DiagrammeRsvg::export_svg(cc_g)
  cc_g = charToRaw(cc_g)
  rsvg::rsvg_pdf(cc_g,"./output/cc.pdf")
  
  svgPanZoom(export_svg(grViz(diagram = dot)),controlIconsEnabled = TRUE,minZoom = 0.5,viewBox = FALSE)
  
  
  # req(rawData_py_input)
  # 
  # #req(rawModel)
  # 
  # # pm4py_inductive <- reticulate::import("pm4py.algo.discovery.inductive.algorithm")
  # # pn <- pm4py_inductive$apply(r_to_py(rawData_py_input))
  # # 
  # # viz <- reticulate::import("pm4py.visualization.petri_net")$visualizer
  # # 
  # # 
  # # #Convert back to Python
  # # py_pn <<- r_to_py(pn[[1]])
  # # py_im <<- r_to_py(pn[[2]])
  # # py_fm <<- r_to_py(pn[[3]])
  # 
  # 
  # viz <- reticulate::import("pm4py.visualization.petri_net")$visualizer
  # # Render to DOT with PMP4Y aligement
  # dot <- viz$apply(py_pn,py_im,py_fm,variant=viz$Variants$FREQUENCY, log=r_to_py(rawData_py_input))$source
  # svgPanZoom(export_svg(grViz(diagram = dot)),controlIconsEnabled = TRUE,minZoom = 0.5,viewBox = FALSE)
  
  # Compute alignment
  #alignment <<- conformance_alignment(py_pn,py_im,py_fm,log=r_to_py(log))
  # # Alignment is returned in long format as data frame
  #head(alignment)
  
  
  
})

###################################
####### Root Cause Process ########
###################################


output$rc_table <- renderSvgPanZoom({
  req(aligned_traces())
  req(rawData())
  
  source_python("source.py")
  rca <<- render_align_table(r_to_py(rawData_py_input), aligned_traces())
  
  dot <- rca$source
  
  rc_g = grViz(diagram = dot)
  rc_g = DiagrammeRsvg::export_svg(rc_g)
  rc_g = charToRaw(rc_g)
  rsvg::rsvg_pdf(rc_g,"./output/rc.pdf")
  
  svgPanZoom(export_svg(grViz(diagram = dot)),controlIconsEnabled = TRUE,minZoom = 0.5,viewBox = FALSE)
})






}  
