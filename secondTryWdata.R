library(shiny)
library(DAPAR)
library(DAPARdata)

# boxplot quanti c vs D
# display tab 2 first column of feature

ui <- fluidPage(
  
  titlePanel("Test avec donnees Exp1_R25_prot"), 
  
  mainPanel(
    
    plotOutput("plot"), 
    
    column(3, sliderInput("row", "Choose row",
                       min = 1, max = nrow(feature), value = c(0, 10))),
    column(3, sliderInput("col", "Choose column",
                min = 1, max = ncol(feature), value = c(0, 10))),
    
    column(6,textOutput("selected_var")),
    
    tableOutput('table')
  )

)


server <- function(input, output) {
  
  #rv <- reactiveValues( # when value modified, modified everywhere
  #  data = NULL
  #)
  
  data("Exp1_R25_prot")
  #rv$obj <- Exp1_R25_prot
  obj <- Exp1_R25_prot
  quanti <- Biobase::exprs(obj)
  dim(quanti) # 2384 6
  feature <- Biobase::fData(obj)
  dim(feature) # 2384 86
  
  output$plot <- renderPlot({
    
    #conds <- Biobase::pData(obj)[,"Condition"]
    #DAPAR::boxPlotD(obj)
    boxplot(quanti, main = "Condition C vs D in quantification data")
    
    })
  
  
  output$selected_var <- renderText({ 
    
    paste("You choose rows", input$row[1], "to", input$row[2], "and columns", 
          input$col[1], "to", input$col[2], "of feature tab", sep = " ")
    
  })
  
  output$table <- renderTable({
    
    feature[input$row[1]:input$row[2],input$col[1]:input$col[2]]
    #feature[1:3,1:3]
    
    })
  
}

shinyApp(ui = ui, server = server)