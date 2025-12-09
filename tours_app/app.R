library(tidyverse)
library(readxl)
library(shinydashboard)
library(shiny)
library(xtable)
library(DT)
library(bslib)



rm(list=ls())

tourism <- read_excel("data/tourism.xlsx")

tourism$accommodation <-  factor( tourism$accommodation , 
                                  labels =   c("*", "**","***","****","*****" )
)

tourism$price <- factor( tourism$price , 
                         labels =   c("Expensive", "Fair", "Cheap" ))

tourism$recommendation <-  factor( tourism$recommendation , 
                                   labels =   c("Yes", "Rather yes","Rather no","No" ))

tourism$skiholiday <- factor( tourism$skiholiday , 
                              labels = c("No", "Yes" ))

tourism$sex <- factor( tourism$sex , 
                       labels = c("Male", "Female" ))

tourism$country  <- factor( tourism$country , 
                            labels = c("Switzerland", "Germany","Austria","Other" ))

tourism$education  <- factor( tourism$education , 
                              labels = c("Secondary", "A-level","Bachelor","Master" ))

ui <- page_fluid(
  layout_sidebar(
    sidebar = sidebar("Sidebar"),
    
    fluidRow(
      # Card de data
      column(
        width = 6,
        card(
          "data",
          infoBoxOutput("guest"),
          infoBoxOutput("satisfaction"),
          tableOutput("accommodation")
        )
      ),
      
      # Card de expenses
      column(
        width = 6,
        card(
          "Expenses",
          style = "width: 300px;",
          plotOutput("expenses", width = "200px", height = "250px")
        )
      )
    ),
    
        width = 6,
        card(
          "Visitantes",
          dataTableOutput("table")
      
    )
  )
)

#Define server logic
server <- function(input, output){
 
  output$guest <- renderInfoBox({
    infoBox("Visitantes", nrow(tourism), icon = icon("droplet", class = "fa-2x"), color = "teal", fill = TRUE
)
  })
  
  output$accommodation <- renderTable({
    tabla <- tourism |> summarise (n = n(), .by  = accommodation) |> arrange(accommodation)
    xtable(tabla)
  }, striped = TRUE)
  
  output$expenses <- renderPlot({
    ggplot(tourism, aes(y = expenses)) +
      geom_boxplot() +
      labs(title = "gastos")
  })
  
  output$table <- renderDataTable({
    df <- filter(tourism, country=="Germany")
    
    datatable(df)
  })
   
}
shinyApp(ui, server)















