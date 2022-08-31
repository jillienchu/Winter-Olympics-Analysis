library(shinydashboard)
library(shiny)
library(pheatmap)
library(ggplot2)
library(DataExplorer)
library(scales)
library(dplyr)
library(plotly)
library(leaflet)
             
medal <- read.csv("/Users/jillienchu/Desktop/ALY 6070/Week 1/archive (3)/medals.csv")
medal_total <- read.csv("/Users/jillienchu/Desktop/ALY 6070/Week 1/archive (3)/medals_total.csv")                  

medal1 <- medal %>% group_by(medal_type, athlete_sex) %>% 
  summarise(Count = table(medal_type))

ui <- dashboardPage(
  dashboardHeader(title = "Medals"),
  dashboardSidebar(selectInput("typeInput", "MEDALS",
                               choices = c("", "Gold", "Silver", "Bronze"))),
  
  dashboardBody(fluidRow(
    valueBoxOutput("value1")
    ,valueBoxOutput("value2"),
    valueBoxOutput("value3"),
    
    box(
      title = "Total Medals",
      subtitle = " "
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput("plot1", height = "300px", width = "400px"))),
    
    box(
      title = "Medals By Gender",
      Subtitle = " ",
      status= "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      plotOutput("plot2",height="300px", width = "400px")
    )
    
  ))

server <- function(input, output) {
  
  total_gold <- sum(medal_total$Gold)
  total_silver <-sum(medal_total$Silver)
  total_bronze <- sum(medal_total$Bronze)
  #Creating box output
  output$value1 <- renderValueBox({
    valueBox(total_gold
             ,'GOld MEdals', icon = icon("fa-solid fa-medal")
             ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(total_silver
             ,'SILVER MEDALS', icon= icon("fa-solid fa-medal")
             ,color = "green")  
  })
  
  output$value3 <- renderValueBox({ 
    valueBox(total_bronze
             ,'BRONZE MEDALS', icon = icon("fa-solid fa-medal")
             ,color = "blue")  
  })
  
  output$plot1 <- renderPlot(
    ggplot(data = medal_total,   
           aes(x= Country , y= Order.by.Total, fill=factor(Country))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Total Medals") + 
      xlab("Countries") + theme(legend.position="None" 
                                ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Medal by Countries")+theme(axis.text.x = element_text(angle = 90))
  )
  output$plot2 <- renderPlot(
    ggplot(data = medal1,
           aes(x= athlete_sex, y = Count, fill = medal_type))+
      labs(colour = medal1$medal_type)+
      geom_bar(stat="identity", position = "stack")+
      xlab("Gender")+ theme(legend.position="None" 
                            ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Medal by Gender")+theme(axis.text.x = element_text(angle = 90))
    
  ) }

shinyApp(ui = ui, server = server)
