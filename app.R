# Source code
# hatgame 
# sompob@tropmedres.ac
# develop by Tanaphum Wichaita

library(shiny)
library(shinyjs)
library(DT)

source("hatgame.R")

ui <- fluidPage(
  includeCSS("www/style.css"),
  shinyjs::useShinyjs(),
  div(
  column(8,
   h1("Simulating SIR model"),
   ),
   column(4,
       tags$img(class = "logo",src="MORU_FS_RGB.png")
       ),
  ),
   sidebarLayout(
      sidebarPanel(
         p(class = "warning","*Try number below or equal 100"),
         numericInput("npop", "Population size", value = 30),
         numericInput("R0", "R0", value = 3),
        
         
         actionButton("load.button", "load data"),
         actionButton("next.button", "Next"),
         actionButton("back.button", "Back")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
          tabPanel("Network",plotOutput("network", width = "100%", height = "800px")),
          tabPanel("Plot", plotOutput("plot.infected")),
          tabPanel("Table", dataTableOutput("table.infected"))
         )
      )
   )
)

server <- function(input, output) {
  
  values <- reactiveValues(df.infected = NULL)
  values$loaded <- FALSE

  shinyjs::disable("next.button")
  shinyjs::disable("back.button")
  npop <- eventReactive(input$npop,{as.numeric(input$npop)})
  R0 <- eventReactive(input$R0,{as.numeric(input$R0)})
  # 
  observe({
    toggleState(id = "load.button", condition = as.numeric(input$npop)  <= 100)
  })
  

  
  # click load button
  observeEvent(input$load.button,{
    req(input$npop <=100)
    shinyjs::enable("next.button")
    shinyjs::enable("back.button")
    values$loaded <- TRUE
    g <- randomNetwork(npop(),R0())
    state <- addInfectedNode(g)
    values$g <- g
    values$state <- state
    values$m.state <- matrix(state,nrow = 1) 
    values$time <- 0 #init time
    values$layout <- layout_nicely(g)
    values$df.infected <- data.frame(time = c(0),susceptible =c(stateCount(values$state)[1]), infected = c(1),recovered =c(0))
    output$network <- renderPlot(plotNetwork(g,values$state,values$layout))
    
  })
  
  # click next button
  observeEvent(input$next.button,{
    if(values$loaded==TRUE){
      values$state <- updateState(values$g, values$state)
      
      state.susceptible <- stateCount(values$state)[1]
      state.infected <- stateCount(values$state)[2]
      state.recovered  <- stateCount(values$state)[3]
      values$time <- values$time + 1
      values$m.state <- rbind(values$m.state,values$state)
      values$df.infected <- rbind(values$df.infected, data.frame(time=c(values$time),susceptible = c(state.susceptible),infected=c(state.infected),recovered=state.recovered))
      
      output$network <- renderPlot(plotNetwork(values$g,values$state,values$layout))
      
      output$plot.infected <- renderPlot(plotInfected(values$df.infected[,c(1,3)]))
      
      if(state.infected <= 0){
        shinyjs::disable("next.button")
      }
    }
  })
  
  #click back button
  observeEvent(input$back.button,{
    if(values$loaded==TRUE){
      nrow.df.infected <- nrow(values$df.infected)
      nrow.m.state <- nrow(values$m.state)
      
      if(nrow.m.state > 2){
        values$time <- values$time - 1
        values$df.infected <- values$df.infected[1:(nrow.df.infected - 1), ]
        values$m.state <- values$m.state[1:(nrow.m.state - 1), ]
        values$state <- values$m.state[nrow(values$m.state),]
      
      }
      
      output$network <- renderPlot(plotNetwork(values$g,values$state,values$layout))
      output$plot.infected <- renderPlot(plotInfected(values$df.infected[,c(1,3)]))
      state.infected <- stateCount(values$state)[2]
      if(state.infected > 0){
        shinyjs::enable("next.button")
      }
    }
    
  })
  
  #susceptible	infected	recovered
  output$table.infected <- DT::renderDataTable({
    req(!is.null(values$df.infected))
    df <- values$df.infected
    datatable(df) %>% 
      formatStyle(columns = "susceptible", target = "cell", backgroundColor = "#B7FFBF") %>% 
      formatStyle(columns = "infected", target = "cell", backgroundColor = "#F7080880") %>% 
      formatStyle(columns = "recovered", target = "cell", backgroundColor = "#cbe9f4")
   })

  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

