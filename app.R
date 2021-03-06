#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(shinythemes)
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(hms)
library(zoo)
library(DT)


ui <- fluidPage(
    
    # Application title
    tags$head(
        tags$style(HTML("
            code {
                display:block;
                padding:9.5px;
                margin:0 0 10px;
                margin-top:10px;
                font-size:13px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:4px; 
                font-family:monospace;
            }"))),
    titlePanel("Tiktijden naar gepresteerde uren"),
    sidebarLayout(
        sidebarPanel(        
            selectInput(
            "maandloon",
            "Opleidingsjaar",
            choices = c(
                "Jaar 1 (3111.92 EUR)" = "3111.92",
                "Jaar 2 (3211.92 EUR)" = "3211.92",
                "Jaar 3 (3336.92  EUR)" = "3336.92", #
                "Jaar 4 (3461.92 EUR)" = "3461.92",
                "Jaar 5 (3611.92 EUR)" = "3611.92",
                "Jaar 6 (3761.92 EUR)" = "3761.92"
            ),
            selected = "Jaar 1 (3111.92)"
        ), 
        selectInput(
            "optingout",
            "Opting Out?",
            choices = c(
                "Ja" = "TRUE",
                "Nee" = "FALSE"
            ),
            selected = "TRUE"
        ),      
            fileInput(
                inputId = "csvFile",
                label = "Upload Excel bestand",
                accept = c(".xlsx")),
        downloadButton('download',"Download the data")),
        mainPanel(
            p("Deze tool zet een Excel bestand met tiktijden om in een nieuwe Excel tabel waar de
               comfortabele en oncomfortabele uren worden berekend op basis van tiktijden, samen met het brutoloon."),
            p("Een voorbeeld over hoe de Excel er moet uitzien is te vinden via onderstaande link. 
                Het is belangrijk dat de Excel er exact hetzelfde uit ziet (titels van kolommen, formaat van dagen en uren (dus niet bv. 20-01-2021 8u maar 20/01/2021 08:00)).
              In de 'type' kolom zijn er 3 mogelijkheden om in te vullen: R voor recup, V voor verlof of W voor wacht."),
            tags$a(href="https://github.com/rmvpaeme/tiktijden_ASO/raw/main/uurschema_voorbeeld.xlsx", "download voorbeeld"),
            p(),
            p(strong("opgelet"), "Deze tool is nog volop in ontwikkeling, bevat een aantal bugs en ontbreekt een aantal features."),
            HTML("<ul><li>Met opting out worden de uren > 48u nog niet aan 110% berekend.</li>
                 <li>Extramurale wachten kunnen nog niet worden geregistreerd. Het type 'wacht' wordt nog niet gebruikt in de berekeningen </li>
                 <li>Ploegensystemen en meerdere keren in en uittikken per dag zijn nog niet getest.</li>
                 <li>Feestdagen worden automatisch herkend en worden berekend aan 150%. </li>
                 <li>Bij verlof wordt er gerekend dat er wordt ingetikt om 8u en uitgetikt om 18u (omdat die uren meetellen voor 'gewerkte' uren om het gemiddelde te berekenen) </li>
                 <li>Bij een 24 uurswacht die start op zaterdag om 8u wordt er 16u gerekend aan 125% en 8 uur aan 150% (want is op zondagmorgen) (en zondagwacht/maandagmorgen analoog berekend)</li>
                 <li>Het uurloon wordt berekend op de volgende manier: maandloon * 3 / 13 weken / 48 uur.</li></ul>"),
            #strong("strong() makes bold text."),
            #em("em() creates italicized (i.e, emphasized) text."),
        )
    ),    mainPanel(
        
        DT::dataTableOutput("modifiedData"),
        hr(),
        p("Deze tool is puur indicatief en exacte berekeningen kunnen verschillen. De gebruiker is zelf verantwoordelijk om de accuraatheid van de berekeningen te verifiëren. De volledige broncode met berekeningen is te vinden op https://github.com/rmvpaeme/tiktijden/"),
        
    ),
    # WHERE YOUR FOOTER GOES
    # Show a plot of the generated distribution

)

server <- function(input, output) {
    
    
    userData <- eventReactive(list(input$csvFile),
                              {
                                  
                                  req(input$csvFile)
                                  
                                  #raw_df <- read.csv(input$csvFile$datapath)
                                  df <- read_excel(input$csvFile$datapath)
                                  
                                  maandloon <- as.numeric(input$maandloon)
                                  uurloon <- maandloon*3/13/48
                                  df$verlof <- ifelse(df$type == "V", TRUE, FALSE)
                                  df$verlof <- ifelse(is.na(df$verlof), FALSE, df$verlof)
                                  #
                                  
                                  df$ingetikt <- as_datetime(ifelse(df$verlof == TRUE, 
                                                                    as_datetime(df$dag + hours(8)), 
                                                                    as_datetime(df$ingetikt)))
                                  df$uitgetikt <- as_datetime(ifelse(df$verlof == TRUE, 
                                                                     as_datetime(df$dag + hours(20)), 
                                                                     as_datetime(df$uitgetikt)))
                                  
                                  df$wacht <- ifelse(df$type == "W", TRUE, FALSE)
                                  df$wacht <- ifelse(is.na(df$wacht), FALSE, df$wacht)
                                  df$recup <- ifelse(df$type == "R", TRUE, FALSE)
                                  df$recup <- ifelse(is.na(df$recup), FALSE, df$recup)
                                  
                                  df$in_date <- as.Date(df$ingetikt)
                                  df$in_time <- format(df$ingetikt,"%H:%M:%S")
                                  df$out_date <- as.Date(df$uitgetikt)
                                  df$out_time <- format(df$uitgetikt,"%H:%M:%S")
                                  dates <- tibble(fulldate = seq(ymd_hms(df$dag[1] + hours(1)),ymd_hms(df$dag[nrow(df)] + days(1) + hours(23) + minutes(59)),by='hours'))
                                  dates$fulldate <- as.character(dates$fulldate)
                                  dates <- separate(data = dates, col = fulldate, into = c("dag", "uur"), sep = " ", remove = FALSE)
                                  dates$fulldate <- ymd_hms(dates$fulldate)
                                  
                                  df$dag <- ymd(df$dag)
                                  df$dag <- as.character(df$dag)
                                  df <- merge(dates, df, by.x = "dag", by.y = "dag", all.x = TRUE)
                                  df <- tibble(df)
                                  df <- dplyr::arrange(df, fulldate)
                                  df$ingetikt <- na.locf(df$ingetikt)
                                  df$uitgetikt <- na.locf(df$uitgetikt)
                                  df$in_date <- na.locf(df$in_date)
                                  df$in_time <- na.locf(df$in_time)
                                  df$out_date <- na.locf(df$out_date)
                                  df$out_time <- na.locf(df$out_time)
                                  #optingout weekuren berekenen
                                  
                                  #op 48u
                                  # opleidingsjaar 1 = 3.111,92 EUR
                                  # jaar 2 = 3.211,92 EUR
                                  # jaar 3 =3.336,92 EUR
                                  # jaar 4 = 3.461,92 EUR
                                  # jaar 5 = 3.611,92 EUR
                                  # jaar 6 = 3.761,92 EUR
                                  
                                  ##  Vergoeding voor de oncomfortabele uren
                                  # De arbeidstijd die plaatsvindt tussen 20u ’s avonds en 8u ’s morgens 
                                  # en op zaterdagen wordt vergoed aan 125% van de basisvergoeding 
                                  # uit artikel 6, puntje a, subartikel 6.1. van deze overeenkomst.
                                  
                                  # De arbeidstijd die plaatsvindt op zon- en feestdagen wordt vergoed aan 150% 
                                  # van de basisvergoeding uit artikel 6 , puntje a, subartikel 6.1. van deze overeenkomst.
                                  
                                  # 6.5. De artsen-specialisten in opleiding die bij artikel 4.3, de optie “ja” hebben aangeduid, 
                                  # krijgen een vergoeding voor deze uren die 110% van de basisvergoeding bedraagt. #
                                  # Deze vergoeding is enkel verschuldigd voor de uren die de gemiddelde wekelijkse arbeidsduur van 48 uren 
                                  # overschrijden en niet het voorwerp hebben uitgemaakt van een vergoeding voor oncomfortabele uren bedoeld onder b). 
                                  # Deze vergoeding is met andere woorden niet cumuleerbaar met de vergoeding onder b).
                                  
                                  # d) Oproepbare extramurale wachtdiensten
                                  
                                  # 6.6. Oproepbare extramurale wachtdiensten worden tijdens de weekdagen tussen 
                                  # 8 uur ’s morgens en 20 uur ’s avonds vergoed met een forfaitair bedrag van 50 EUR per 
                                  # begonnen periode van 12 uren.
                                  
                                  
                                  # 6.7. Oproepbare extramurale wachtdiensten worden tijdens het weekend en tussen 20 uur ’s avonds en 8 uur ’s morgens 
                                  # vergoed met een forfaitair bedrag van 75 EUR per begonnen periode van 12 uren.
                                  
                                  # 6.8. Wanneer er tijdens de extramurale oproepbare wachtdiensten effectief wordt gepresteerd in het ziekenhuis, 
                                  # wordt dit vergoed overeenkomstig artikel 6, puntje a tot en met c, 
                                  # subartikel 6.1. t.e.m. 6.4.
                                  
                                  # feestdagen
                                  
                                  Nieuwjaar <- c("01/01/2022", "01/01/2023","01/01/2024","01/01/2025")
                                  Paasmaandag	<- c("18/04/2022", "10/04/2023", "01/04/2024", "21/04/2025")
                                  DvdA <- c("01/05/2022", "01/05/2023","01/05/2024","01/05/2025")
                                  OH_Hemelvaart <- c("26/05/2022", "18/05/2023", "09/05/2024", "29/05/2025")
                                  Pinkstermaandag <-	c("06/06/2022", "29/05/2023",  "20/05/2024","09/05/2025" )
                                  Nationale_feestdag <-	c("21/07/2022", "21/07/2023", "21/07/2024", "21/07/2025")
                                  OLV_hemelvaart <- c("15/08/2022", "15/08/2023", "15/08/2024", "15/08/2025")
                                  Allerheiligen	<-  c("01/11/2021", "01/11/2022", "01/11/2023","01/11/2024","01/11/2025")
                                  Wapenstilstand	<- c("11/11/2021", "11/11/2022", "11/11/2023","11/11/2024","11/11/2025")
                                  Kerstmis	<- c("25/12/2021", "25/12/2022", "25/12/2023", "25/12/2024", "25/12/2025")
                                  feestdagen <- c(Nieuwjaar, Paasmaandag, DvdA, OH_Hemelvaart, Pinkstermaandag, Nationale_feestdag, 
                                                  OLV_hemelvaart, Allerheiligen, Wapenstilstand, Kerstmis)
                                  feestdagen <- as.Date(dmy(feestdagen))
                                  
                                  df$weekdag <- weekdays(as.Date(df$dag))
                                  
                                  df$feestdag <- ifelse(as.Date(df$dag) %in% feestdagen, TRUE, FALSE)
                                  
                                  df$weekdag <- ifelse(df$feestdag == TRUE , "Sunday", df$weekdag)
                                  
                                  row <- apply(df, 1, function(row){
                                      return(row)})
                                  row <- row[,72]
                                  
                                  #df <- df %>% filter(!is.na(ingetikt))
                                  
                                  borders <- function(fulldate, ingetikt, uitgetikt){
                                      if (ingetikt %within% interval(fulldate, fulldate - hours(1))){
                                          a = as.double(as_hms(fulldate - ingetikt)/60/60)
                                      } else if (uitgetikt %within% interval( fulldate - hours(1), fulldate)){
                                          a = 1 - as.double(as_hms(fulldate - uitgetikt)/60/60)
                                      } else if (ingetikt %within% interval( fulldate - hours(1), fulldate)){
                                          a = as.double(as_hms(fulldate - ingetikt)/60/60)
                                      } else if (uitgetikt %within% interval(fulldate,  fulldate - hours(1))){
                                          a = 1 - as.double(as_hms(fulldate - uitgetikt)/60/60)
                                      } else {
                                          a = 1
                                      }
                                      return(round(a, 2))
                                  }
                                  df$fulldate <- df$fulldate + hours(1)
                                  calc <- apply(df,1,function(row){
                                      fulldate = as_datetime(row["fulldate"])
                                      uitgetikt <- as_datetime(row["uitgetikt"]) #%>% pull(uitgetikt)
                                      ingetikt <- as_datetime(row["ingetikt"]) #%>% pull(ingetikt)
                                      weekdag <- as.character(row["weekdag"])# %>% pull(dag)
                                      in_date <- as_date(row["in_date"]) #%>% pull(in_date)
                                      out_date <- as_date(row["out_date"])
                                      
                                      comf_uur_start <- as_datetime(in_date + hours(8))
                                      comf_uur_stop <- as_datetime(in_date + hours(20))
                                      
                                      if(fulldate %within% interval(ingetikt,uitgetikt + hours(1))){
                                          if (fulldate %within% interval(comf_uur_start + seconds(1),comf_uur_stop + seconds(1))  & !(weekdag %in% c("Saturday", "Sunday"))){
                                              row["comfortabele_uren"] = borders(fulldate, ingetikt, uitgetikt)
                                              row["bruto_comfortabele_uren_euro"] <- round(as.double(row["comfortabele_uren"])*uurloon,2)
                                              row["oncomfortabele_uren"] = 0
                                              row["bruto_oncomfortabele_uren_euro"] <- 0
                                          } else if (!(fulldate %within% interval(comf_uur_start + seconds(1),comf_uur_stop + seconds(1)))  & !(weekdag %in% c("Saturday", "Sunday"))){
                                              row["comfortabele_uren"] = 0
                                              row["bruto_comfortabele_uren_euro"] <- 0
                                              row["oncomfortabele_uren"] = borders(fulldate, ingetikt, uitgetikt)
                                              row["bruto_oncomfortabele_uren_euro"] <- round(as.double(row["oncomfortabele_uren"])*uurloon*1.25,2)
                                          } else if ((weekdag %in% c("Saturday"))) {
                                              row["comfortabele_uren"] = 0
                                              row["bruto_comfortabele_uren_euro"] <- 0
                                              row["oncomfortabele_uren"] = borders(fulldate, ingetikt, uitgetikt)
                                              row["bruto_oncomfortabele_uren_euro"] <- round(as.double(row["oncomfortabele_uren"])*uurloon*1.25,2)
                                          } else if ((weekdag %in% c("Sunday"))) {
                                              row["comfortabele_uren"] = 0
                                              row["bruto_comfortabele_uren_euro"] <- 0
                                              row["oncomfortabele_uren"] = borders(fulldate, ingetikt, uitgetikt)
                                              row["bruto_oncomfortabele_uren_euro"] <- round(as.double(row["oncomfortabele_uren"])*uurloon*1.5,2)  } 
                                      } else{
                                          row["comfortabele_uren"] = 0
                                          row["bruto_comfortabele_uren_euro"] <- 0
                                          row["oncomfortabele_uren"] = 0
                                          row["bruto_oncomfortabele_uren_euro"] <-0
                                          
                                      }
                                      return(row)
                                  })
                                  
                                  totaal <- calc %>% t()
                                  totaal <- data.frame(totaal)
                                  
                                  #df$uren_gewerkt_pd <- as.double(as_hms((df$uitgetikt-df$ingetikt)))/60/60
                                  
                                  totaal <- totaal %>% group_by(dag_grpby = day(dag), maand_grpby = month(dag)) %>% mutate(comf_uren_pd = sum(as.numeric(comfortabele_uren)), 
                                                                                                                           comf_uren_bruto = sum(as.numeric(bruto_comfortabele_uren_euro)),
                                                                                                                           oncomf_uren_pd = sum(as.numeric(oncomfortabele_uren)),
                                                                                                                           oncomf_uren_bruto = sum(as.numeric(bruto_oncomfortabele_uren_euro))) %>% ungroup()
                                  totaal <- totaal %>% distinct(dag, .keep_all = TRUE) %>% select(-c(comfortabele_uren, bruto_comfortabele_uren_euro, oncomfortabele_uren , bruto_oncomfortabele_uren_euro))
                                  totaal$sum_bruto <- totaal$oncomf_uren_bruto + totaal$comf_uren_bruto
                                  totaal$sum_uren <- totaal$oncomf_uren_pd + totaal$comf_uren_pd
                                  
                                  totaal <- totaal %>% group_by(week_grpby = week(dag), maand_grpby = month(dag)) %>% mutate(uren_gewerkt_per_week = sum(sum_uren)) %>% 
                                      ungroup()
                                  
                                  totaal <- totaal %>% select(dag, ingetikt, uitgetikt, weekdag, type, feestdag,
                                                              comf_uren_pd, comf_uren_bruto, oncomf_uren_pd, oncomf_uren_bruto,
                                                              sum_bruto, sum_uren, uren_gewerkt_per_week)
                                  
                                  totaal
                              }) 
    
    output$modifiedData <-  DT::renderDataTable({userData()})
    
    output$download <- downloadHandler(
        filename = function(){"tiktijden.xlsx"}, 
        content = function(fname){
            write_xlsx(userData(), fname)
        }
    )
    
}

shinyApp(ui = ui, server = server)