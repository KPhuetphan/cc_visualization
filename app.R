
options(shiny.maxRequestSize = 50*1024^2)


### Start app

shinyApp(ui, server, options = list(height = 500))
