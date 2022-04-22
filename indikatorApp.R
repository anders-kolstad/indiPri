library(shiny)
library(shinydashboard)
library(ggplot2)
library(dashboardthemes)
library(tidyverse)
library(readxl)
library(shinyWidgets)
library(DT)

dat <- read_excel("data/indikatorer.xlsx", 
                       sheet = "Indikatoroversikt")

#dat <- read_excel("data/indikatorer.xlsx", sheet = "Indikatoroversikt")


# UI ----------------------------------------------------------------------


# some strings
eco <- c("Våtmark", "Semi-naturlig mark", "Åpne områder")
dat$ecoSum <- rowSums(dat[,eco], na.rm = T)  
kjerne <- c("1 Økosystem" = 1, "2 Økosystemer" = 2, "3 Økosystemer" = 3, "Ingen" = 0)
ui <- 
  dashboardPage(
    
    title = "Indikatoroversikt",
    
    dashboardHeader(
      title = textOutput('Hei Hopp'),
      titleWidth = 400
    ),
    
    dashboardSidebar(
      
      pickerInput(inputId = "økosystem", 
                  label = "Økosystem", 
                  choices =eco,
                  selected = "Våtmark",
                  multiple = F),
      
      pickerInput(inputId = "pilot", 
                  label = "Uttesting", 
                  choices =unique(dat$`Testet i pilot`),
                  selected =c("PAEC", "IBECA"),
                  multiple = T),
      
      pickerInput(inputId = "kjerne", 
                  label = "Kjerneindikatorer", 
                  choices =kjerne,
                  selected =kjerne[!kjerne %in% c(0)],
                  multiple = T)
    ),
    
    dashboardBody(
      shinyDashboardThemes(
        theme = "purple_gradient"
      ),
      
      fluidRow(
       column(width = 6,
            plotOutput('myBarplot')
       ),
        column(width = 6,
            plotOutput('myBarplotECT')  
        )
        ),
        
        fluidRow(
          box(width=NULL,
              DTOutput('myTable'))
        )
    )
)





server <- function(input, output) {
  
  
  
  # filtrert datasett
  datR <- reactive({
    
    temp <- dat
    temp$incl <- NA
    if("Våtmark" %in% input$økosystem)            temp$incl[temp$Våtmark==1]              <- 1
    if("Semi-naturlig mark" %in% input$økosystem) temp$incl[temp$`Semi-naturlig mark`==1] <- 1
    if("Åpne områder" %in% input$økosystem)       temp$incl[temp$`Åpne områder`==1]       <- 1
    temp <- temp[temp$incl==1,]

    temp[
        temp$`Testet i pilot` %in% input$pilot &
        temp$ecoSum %in% input$kjerne 
        ,
      ]
  })
  
  
  # filtrert datasett kun etter økosystemvalg
  datREco <- reactive({
    
    ecotemp <- dat
    
    if("Våtmark"            %in% input$økosystem)  ecotemp <- ecotemp[ecotemp$Våtmark==1,]             
    if("Semi-naturlig mark" %in% input$økosystem)  ecotemp <- ecotemp[ecotemp$`Semi-naturlig mark`==1,] 
    if("Åpne områder"       %in% input$økosystem)  ecotemp <- ecotemp[ecotemp$`Åpne områder`==1,]     
    ecotemp
    
  })
  
  
  output$myBarplot <- renderPlot({
    
    ggplot(datREco(), aes( x = `Økologisk egenskap`
    ))+
      geom_bar(stat = "count",
               fill = "grey80",
               colour = "grey30",
               size=1.5,
               alpha = 0.3,
               width=.1)+
      geom_bar(data = datR(),
               stat = "count",
               fill = "grey80",
               colour = "grey30",
               size=1.5)+
      theme_bw(base_size = 12)+
      coord_flip()+
      labs(x = "", y = "Antall indikatorer")+
      ggtitle(paste("Egenskaper - ", input$økosystem))
    
  })
 
  
  
  output$myBarplotECT <- renderPlot({
    
    ggplot(datREco(), aes( x = `ECT-klasse`
    ))+
      geom_bar(stat = "count",
               fill = "grey80",
               colour = "grey30",
               size=1.5,
               alpha=.3,
               width=.1)+
      geom_bar(data = datR(),
               stat = "count",
               fill = "grey80",
               colour = "grey30",
               size=1.5)+
      theme_bw()+
      coord_flip()+
      labs(x = "", y = "Antall indikatorer")+
      ggtitle(paste("ECT-klasser - ", input$økosystem))
    
  })  
  
  
  output$myTable <- 
    renderDataTable({
      
      DT::datatable(datR(), 
                    options = list(
                      scrollX = TRUE,
                      columnDefs = list(list(
                        targets = "_all",
                        render = JS(
                          "function(data, type, row, meta) {",
                          "return type === 'display' && data != null && data.length > 30 ?",
                          "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                          "}")
                      ))),
                    class = "display")
    })
  
}






shinyApp(ui = ui, server = server)