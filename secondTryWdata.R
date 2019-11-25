library(shiny)
library(DAPAR)

# boxplot quanti c vs D
# display tab 2 first column of feature

utils::data(Exp1_R25_prot, package='DAPARdata') #load dataset Exp1_R25_prot from package DAPARdata
obj <- Exp1_R25_prot
feature <- Biobase::fData(obj)

ui <- fluidPage(
  
  titlePanel("Test avec donnees Exp1_R25_prot"), 
  
  mainPanel(
    actionButton('btn', 'toto'), # to update what's inside isolate()
    
    plotOutput("plot"), # plot = boxplot
    
    column(3, sliderInput("row", "Choose row",
                       min = 1, max = nrow(feature), value = c(1, 5))),
    column(3, sliderInput("col", "Choose column",
                min = 1, max = ncol(feature), value = c(0, 5))), # two slider bars to choose boundaries of row and column
    
    column(6,textOutput("selected_var")), # text output
    
    tableOutput('table') # tab output
  )

)


server <- function(input, output) {
  
  
  rv <- reactiveValues( # when value is modified, saved in rv$data and changed everywhere
    data = NULL
  )
  
  toto <- 0
  
  
  output$plot <- renderPlot({ 
    input$btn # display button toto to update the boxplot

    isolate({ # all inside isolate() is unchaged until input$btn toto pushed
      quanti <- Biobase::exprs(obj)[c(input$row[1]:input$row[2]),]
      boxplot(quanti, main = "Condition C vs D in quantification data")
    })
    
    })
  
  
  observe({ # observe when input$row varies then only update functions which use input$row
    input$row
    
    rv$data <- input$row[1] # overwrite rv$data with new input$row[1] value
    
  })
  
  
  output$selected_var <- renderText({ 
    
    print(input$btn) # display in console
    print(rv$data) # display in console
    
      paste("You choose rows", rv$data, "to", input$row[2], "and columns", 
          input$col[1], "to", input$col[2], "of feature tab", sep = " ") # here, rv$data is input$row[1]
    
  })
  
  output$table <- renderTable({
    
    feature[input$row[1]:input$row[2],input$col[1]:input$col[2]] # display tab "feature" with boundaries of sliderInput
    #feature[1:3,1:3]
    
    })
  
}

shinyApp(ui = ui, server = server)