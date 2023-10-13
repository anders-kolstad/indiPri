source("libraries.R")

dat <- read_excel("data/indikatorer.xlsx", 
                       sheet = "Indikatoroversikt")

#dat <- read_excel("data/indikatorer.xlsx", sheet = "Indikatoroversikt")
statuser <- c(
  "ferdig",
  "ferdig utkast",
  "under utvikling",
  "vil utvikles",
  "vurderes",
  "forkastet"
)

# UI ----------------------------------------------------------------------


# some strings
eco <- c("Våtmark", "Semi-naturlig mark", "Åpne områder")
dat$ecoSum <- rowSums(dat[,eco], na.rm = T)  
kjerne <- c("1 Økosystem" = 1, "2 Økosystemer" = 2, "3 Økosystemer" = 3, "Ingen" = 0)
met <- c("Tilnærmet klare til bruk", "Delvis utviklet")



# Start App
ui <- 
  dashboardPage(
 
    dashboardHeader(
      title = 'indiPri'
     
    ),
    
    dashboardSidebar(
      
      h2("Fitrer data"),
      br(),
      
  # INN: Økosystem ----
      pickerInput(inputId = "økosystem", 
                  label = "Økosystem", 
                  choices =eco,
                  selected = eco,
                  multiple = T,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Ingen",
                    `select-all-text` = "Velg alle",
                    `none-selected-text` = "Velg fra listen"
                  )),
  
  # INN: Indikatorprosjekt ----
  pickerInput(inputId = "prosjekt", 
              label = "Indikatorprosjekt", 
              choices =unique(dat$indikatorprosjekt),
              selected =unique(dat$indikatorprosjekt),
              multiple = T,
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "Ingen",
                `select-all-text` = "Velg alle",
                `none-selected-text` = "Venligst velg en eller flere prosjekter"
              )),
  
  # INN: Pilot ----    
      pickerInput(inputId = "pilot", 
                  label = "Uttesting", 
                  choices =unique(dat$`Testet i pilot`),
                  selected =unique(dat$`Testet i pilot`),
                  multiple = T,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Ingen",
                    `select-all-text` = "Velg alle",
                    `none-selected-text` = "Velg fra listen"
                  )),
  
  # INN: Kjerneindikatorer ----    
      pickerInput(inputId = "kjerne", 
                  label = "Kjerneindikatorer", 
                  choices =kjerne,
                  selected =kjerne[!kjerne %in% c(0)],
                  multiple = T,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Ingen",
                    `select-all-text` = "Velg alle",
                    `none-selected-text` = "Velg fra listen"
                  )),
  
  # INN: Utviklingsbehov ----
      pickerInput(inputId = "metode", 
                  label = "Utviklingsbehov", 
                  choices =unique(dat$Metodeutvikling),
                  selected =unique(dat$Metodeutvikling),
                  multiple = T,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Ingen",
                    `select-all-text` = "Velg alle",
                    `none-selected-text` = "Velg fra listen"
                  )),
  
  # INN: Øko-egenskap ----
      pickerInput(inputId = "egenskap", 
                  label = "Egenskap", 
                  choices =unique(dat$`Økologisk egenskap`),
                  selected =unique(dat$`Økologisk egenskap`),
                  multiple = T,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Ingen",
                    `select-all-text` = "Velg alle",
                    `none-selected-text` = "Venligst velg en eller flere egenskaper"
                  )),
  
  
  
      br(),
  
  # UTVALG:  Metodeutvikling ----
      h4("Skru av eller på utheving:"),
      pickerInput(inputId = "metodevalg", 
                  label = "Metodeutvikling", 
                  choices =met,
                  selected = NULL,
                  multiple = T,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Ingen",
                    `select-all-text` = "Velg alle",
                    `none-selected-text` = "Ingen valg er gjort"
                  )),
  
  # UTVALG:  Prosjekt ----
  pickerInput(inputId = "prosjektUtheving", 
              label = "Indikatorprosjekt", 
              choices =unique(dat$indikatorprosjekt),
              selected =NULL,
              multiple = T,
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "Ingen",
                `select-all-text` = "Velg alle",
                `none-selected-text` = "Ingen valg er gjort"
              )),
  
  # UTVALG:  Status ----
      pickerInput(inputId = "status", 
                  label = "Marker indikatorer basert på status", 
                  choices = statuser,
                  selected = NULL,
                  multiple = T,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Ingen",
                    `select-all-text` = "Velg alle",
                    `none-selected-text` = "Ingen valg er gjort"
                  ))
    ),
    
    dashboardBody(
      shinyDashboardThemes(
        theme = "blue_gradient"
      ),
      
      fluidRow(
       column(width=10,
            plotOutput('myBarplot'),
            plotOutput('myBarplotECT')
       ),
       column(width=2,
              br(),
              h4('Info:', strong('indiPri'), 'er et verktøy for å kartlegge utviklingsbehov for indikatorer for økologisk tilstand. 
                 Bruk filtrerne til venstre for å filtrere tabellen og for å bestemme hvilke indikatorer som skal vises i figurene. 
                 Filteret for økosystem vil uansett kun endre utvalget i tabellen.  Deretter kan du velge å markere rader i tabellen,
                 enten i menyen til ventre, og/eller ved å trykke på radene i tabellen under. Målet er å finne et utvalg indikatorer
                 som resulterer i at alle egenskapene og ECT-klassene i figuren øvert har en eller flere indikatore (vist i grønt).')
        )
       ),
        
        fluidRow(
          h2("Tabell over indikatorer"),
          h4("Utvalget endrer seg basert på filtrene til høyre."),
          h4("Trykk på rader for å velge eller avvelge de. Utvalget vises som grønne søyler i figurene over."),
          br(),
          box(width=NULL,
              DTOutput('myTable'))
        )
    )
)





server <- function(input, output) {
  
## OPTIONS ######################################################
  # set page length for tables
  options(DT.options = list(pageLength = 20))
  
  
## REACTIVE DATASETS ##########################################
  
  # Melt data and filter by egenskap
  datRMelt <- reactive({
    
    meltDat <- data.table::melt(setDT(dat),
                                measure.vars = eco,
                                variable.name = "Økosystem_long",
                                na.rm=T)
    meltDat <- meltDat[meltDat$`Økologisk egenskap` %in% input$egenskap,]
    meltDat <- meltDat[meltDat$indikatorprosjekt %in% input$prosjekt,]
    meltDat
    
  })
  

  # filtrer datasett ytterligere (til figurene, ikke til tabellen)
  datRfig <- reactive({
    
    temp <- datRMelt()
    
    temp[
      temp$`Testet i pilot` %in% input$pilot &
      temp$ecoSum %in% input$kjerne          &
      temp$Metodeutvikling %in% input$metode
      ,
    ]
  })
  
  # filtrert datasett (til tabellen, ikke til figurene)
  datRtab <- reactive({
    
    temp <- dat
    temp$incl <- NA
    if("Våtmark" %in% input$økosystem)            temp$incl[temp$Våtmark==1]              <- 1
    if("Semi-naturlig mark" %in% input$økosystem) temp$incl[temp$`Semi-naturlig mark`==1] <- 1
    if("Åpne områder" %in% input$økosystem)       temp$incl[temp$`Åpne områder`==1]       <- 1
    temp <- temp[temp$incl==1,]

    temp[
        temp$`Testet i pilot`         %in% input$pilot    &
        temp$ecoSum                   %in% input$kjerne   &
        temp$`Økologisk egenskap`     %in% input$egenskap &
        temp$indikatorprosjekt        %in% input$prosjekt &  
        temp$Metodeutvikling          %in% input$metode  
        ,
      ]
  })
  
 
  
 
  # filtrert datasett basert på valg i tabellen. 
  # Disse dataene blir smeltet og ser likt ut som datRfig
  datRvalg <- reactive({
    
    temp <- datRtab()[input$myTable_rows_selected,]
    temp <- data.table::melt(setDT(temp),
                             measure.vars = eco,
                             variable.name = "Økosystem_long",
                             na.rm=T)
    temp
  })
  
  
  
## BARPLOTS ########################################
  
  ##  First Plot #############################################
  
  output$myBarplot <- renderPlot({
    
    
    ifelse(is.null(input$myTable_rows_selected), 
           t <- datRfig()[0,],
           t <- datRvalg())
    
    ggplot(datRMelt(), 
           aes( x = `Økologisk egenskap`))+
      geom_bar(stat = "count",
               fill = "grey80",
               colour = "grey30",
               size=1.5,
               alpha = 0.3,
               width=.01)+
      geom_bar(data = datRfig(),
               stat = "count",
               fill = "cornsilk",
               colour = "grey30",
               size=1.5)+
      geom_bar(data = t,
               stat = "count",
               fill = "green",
               colour = "grey30",
               size=1.5)+
      #geom_hline(yintercept = 0.5, size = 2, alpha = .5, colour="red")+
      #geom_hline(yintercept = 1.5, size = 2, alpha = .5, colour="orange")+
      #geom_hline(yintercept = 2.5, size = 2, alpha = .5, colour="green")+
      facet_wrap(.~Økosystem_long)+
      theme_bw(base_size = 16)+
      coord_flip()+
      labs(x = "", y = "Antall indikatorer")+
      ggtitle("Egenskaper - ")
    
  })
 

  ##  Second Plot #############################################
  
  
  output$myBarplotECT <- renderPlot({
    
    ifelse(is.null(input$myTable_rows_selected), 
           t <- datRfig()[0,],
           t <- datRvalg())
    
    ggplot(datRMelt(), 
      aes( x = `ECT-klasse`))+
       geom_bar(
               stat = "count",
               fill = "grey80",
               colour = "grey30",
               size=1.5,
               alpha=.3,
               width=.01
               )+
      geom_bar(data = datRfig(),
               stat = "count",
               fill = "burlywood",
               colour = "grey30",
               size=1.5)+
      geom_bar(data = t,
               stat = "count",
               fill = "green",
               colour = "grey30",
               size=1.5)+
      #geom_hline(yintercept = 0.5, size = 2, alpha= .5, colour="red")+
      #geom_hline(yintercept = 1.5, size = 2, alpha= .5, colour="orange")+
      #geom_hline(yintercept = 2.5, size = 2, alpha= .5, colour="green")+
      facet_wrap(.~Økosystem_long)+
      theme_bw(base_size = 16)+
      coord_flip()+
      labs(x = "", y = "Antall indikatorer")+
      ggtitle("ECT-klasser - ")
    
  })  
  
  
  
## TABLE ##########################################

  pre <- reactive({
    # find preselected rows
    
      temp <- as.numeric(NULL)
      t <- datRtab()
      if("Tilnærmet klare til bruk" %in% input$metodevalg) temp <- c(temp, which(t$Metodeutvikling == "klart"))
      if("Delvis utviklet" %in% input$metodevalg)          temp <- c(temp, which(t$Metodeutvikling == "delvis klart"))
      #if("ja" %in% input$utvalgt)                          temp <- c(temp, which(t$Utvalgt == "ja"))
      temp <- c(temp, which(t$indikatorprosjekt %in% input$prosjektUtheving))
      temp <- c(temp, which(t$Status %in% input$status))
      temp
    })
  
  output$myTable <- 
    renderDataTable(
      DT::datatable(
        dplyr::select(datRtab(),
                      -PAEC,
                      -IBECA,
                      -ecoSum,
                      -incl,
                      -'Endring ift piloten',
                      -'Knyttet til flere egenskaper'), 
        
          selection  = list(selected = pre()),
          options = list(scrollX = TRUE)
         
        )
    )
  
}






shinyApp(ui = ui, server = server)