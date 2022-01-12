install.packages("shiny")
install.packages("shinyjs")
install.packages("shinythemes")
install.packages("shinycssloaders")
install.packages("shinydashboard")
install.packages("shinyWidgets")
install.packages("data.table")
install.packages("DT")
install.packages("plyr")
install.packages("dplyr")

install.packages("edeaR")
install.packages("eventdataR")
install.packages("bupaR")
install.packages("processmapR")
install.packages("processmonitR")
install.packages("xesreadR")
install.packages("reticulate")
install.packages("DiagrammeR")

install.packages("DiagrammeRsvg")
install.packages("svgPanZoom")
install.packages("ggplot2")
install.packages("svglite")
install.packages("ggiraph")
install.packages("plotly")

install.packages("flexdashboard")
install.packages("petrinetR")

install.packages("tidyverse")

install.packages("remotes")
remotes::install_github("bupaverse/pm4py@v1.2.7")

library(reticulate)

if (FALSE) {
  py_config()
  use_python("C:/ProgramData/Miniconda3/python.exe")
  cenvs=conda_list()
  
  use_condaenv('Miniconda3')
  
  reticulate::use_condaenv(condaenv = 'ccvis', required = TRUE, conda = "C:/ProgramData/Miniconda3/python.exe")
  
  Sys.setenv(RETICULATE_PYTHON = "C:/ProgramData/Miniconda3/python.exe")
  cenvs=conda_list()
  use_python("C:/ProgramData/Miniconda3/python.exe")
  #use_python("C:/Python39/python.exe")
  
  use_condaenv('ccvis', required = TRUE)
  
  use_condaenv('Miniconda3')
  venvs = virtualenv_list()
  #use_virtualenv("ccvis")
  
  py_install(pip = TRUE)
}
py_install("pm4py==2.2.16")

#conda create -n ccvis
#conda activate ccvis
#pip install pm4py==2.2.16

### https://fanwangecon.github.io/R4Econ/development/python/htmlpdfr/fs_python_reticulate.html
