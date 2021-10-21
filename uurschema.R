library(tidyverse)
library(readxl)
library(lubridate)
library(hms)

maandloon <- 3611
uurloon <- maandloon*3/13/48

df <- read_excel("/Users/rmvpaeme/Downloads/uurschema_voorbeeld.xlsx")
df <- read_excel("/Users/rmvpaeme/uurtracker/urenlijst_test.xlsx")

df$verlof <- ifelse(is.na(df$verlof), FALSE, TRUE)
df$recup <- ifelse(is.na(df$recup), FALSE, TRUE)

df$ingetikt <- as_datetime(ifelse(df$verlof == TRUE, 
                                  as_datetime(df$dag + hours(8)), 
                                  ifelse(df$recup == TRUE, as_datetime(df$dag + hours(8)), as_datetime(df$ingetikt))))
df$uitgetikt <- as_datetime(ifelse(df$verlof == TRUE, 
                                  as_datetime(df$dag + hours(20)), 
                                  ifelse(df$recup == TRUE, as_datetime(df$dag + hours(8)), as_datetime(df$uitgetikt))))

df$in_date <- as.Date(df$ingetikt)
df$in_time <- format(df$ingetikt,"%H:%M:%S")
df$out_date <- as.Date(df$uitgetikt)
df$out_time <- format(df$uitgetikt,"%H:%M:%S")


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


df$hours_worked <- as.double(as_hms((df$uitgetikt-df$ingetikt)))/60/60
df <- df %>% group_by(week = week(dag)) %>% mutate(uren_gewerkt_per_week = sum(hours_worked)) %>% 
  ungroup()

df %>% group_by(week = week(dag)) %>% summarize(uren_gewerkt_per_week = sum(hours_worked))

row <- apply(df, 1, function(row){
  return(row)})
row <- row[,5]

calc <- apply(df, 1, function(row){
  uitgetikt <- as_datetime(row["uitgetikt"]) #%>% pull(uitgetikt)
  ingetikt <- as_datetime(row["ingetikt"]) #%>% pull(ingetikt)
  in_date <- as_date(row["in_date"]) #%>% pull(in_date)
  out_date <- as_date(row["out_date"])
  weekdag <- as.character(row["weekdag"])# %>% pull(dag)
  
  comf_uur_start <- as_datetime(in_date + hours(8))
  comf_uur_stop <- as_datetime(in_date + hours(20))

  if (ingetikt >= comf_uur_start & uitgetikt <= comf_uur_stop  & !(weekdag %in% c("Saturday", "Sunday"))){
    #intikken en uittikken tussen 8u sochtends en 20u savonds
    row["comfortabele_uren"] = as.double(as_hms((uitgetikt-ingetikt)))/60/60
    row["bruto_comfortabele_uren_euro"] <- round(as.double(row["comfortabele_uren"])*uurloon,2)
    row["oncomfortabele_uren"] = 0
    row["bruto_oncomfortabele_uren_euro"] <- 0
    #intikken na 8u sochtends uittikken na 20u savonds
    } else if (ingetikt <= comf_uur_start & uitgetikt >= comf_uur_stop & !(weekdag %in% c("Saturday", "Sunday"))) {
    row["comfortabele_uren"] = as.double(as_hms((comf_uur_stop-comf_uur_start)))/60/60
    row["bruto_comfortabele_uren_euro"] <- round(as.double(row["comfortabele_uren"])*uurloon,2)
    
    row["oncomfortabele_uren"] = as.double(as_hms((uitgetikt-comf_uur_stop)))/60/60
    row["bruto_oncomfortabele_uren_euro"] <- round(as.double(row["oncomfortabele_uren"])*uurloon*1.25,2)
  
    # intikken voor 8u sochtends en uittikken voor 20u savonds
    }  else if (ingetikt <= comf_uur_start & uitgetikt <= comf_uur_stop & !(weekdag %in% c("Saturday", "Sunday"))) {
      row["comfortabele_uren"] = as.double(as_hms((uitgetikt-comf_uur_start)))/60/60
      row["bruto_comfortabele_uren_euro"] <- round(as.double(row["comfortabele_uren"])*uurloon,2)
      
      row["oncomfortabele_uren"] = as.double(as_hms((comf_uur_start-ingetikt)))/60/60
      row["bruto_oncomfortabele_uren_euro"] <- round(as.double(row["oncomfortabele_uren"])*uurloon*1.25,2)
    # intikken voor 8u sochtends en uittikken na 20u savonds   
    } else if (ingetikt <= comf_uur_start & uitgetikt >= comf_uur_stop & !(weekdag %in% c("Saturday", "Sunday"))) {
      row["comfortabele_uren"] = as.double(as_hms((comf_uur_stop-comf_uur_start)))/60/60
      row["bruto_comfortabele_uren_euro"] <- round(as.double(row["comfortabele_uren"])*uurloon,2)
      
      row["oncomfortabele_uren"] = as.double(as_hms((comf_uur_start-ingetikt)))/60/60 + as.double(as_hms((uitgetikt + comf_uur_stop)))/60/60 
      row["bruto_oncomfortabele_uren_euro"] <- round(as.double(row["oncomfortabele_uren"])*uurloon*1.25,2)
      
    }else if ((weekdag %in% c("Saturday"))) {
      if (out_date > in_date){
        row["comfortabele_uren"] <- 0
        row["bruto_comfortabele_uren_euro"] <- 0
        # shift duurt van zaterdag tot zondagochtend (uren na middernacht vergoeden als zondag)
        voor_middernacht = as.double(as_hms((as_datetime(in_date + hours(24)) - ingetikt)))/60/60
        bruto_voor_middernacht <- round(as.double(voor_middernacht)*uurloon*1.25,2)
        na_middernacht = as.double(as_hms((uitgetikt - as_datetime(in_date + hours(24)))))/60/60
        bruto_na_middernacht <- round(as.double(na_middernacht)*uurloon*1.5,2)
        row["oncomfortabele_uren"] = voor_middernacht + na_middernacht
        row["bruto_oncomfortabele_uren_euro"] <- bruto_voor_middernacht+bruto_na_middernacht
      } else {
        row["comfortabele_uren"] <- 0
        row["bruto_comfortabele_uren_euro"] <- 0
        row["oncomfortabele_uren"] = as.double(as_hms((uitgetikt-ingetikt)))/60/60
        row["bruto_oncomfortabele_uren_euro"] <- round(as.double(row["oncomfortabele_uren"])*uurloon*1.25,2)
      }
      
    } else if ((weekdag %in% c("Sunday"))) {
      if (out_date > in_date){
        row["comfortabele_uren"] <- 0
        row["bruto_comfortabele_uren_euro"] <- 0
        # shift duurt van zondag tot maandagochtend (uren na middernacht vergoeden als oncomf uur)
        voor_middernacht = as.double(as_hms((as_datetime(in_date + hours(24)) - ingetikt)))/60/60
        bruto_voor_middernacht <- round(as.double(voor_middernacht)*uurloon*1.5,2)
        na_middernacht = as.double(as_hms((uitgetikt - as_datetime(in_date + hours(24)))))/60/60
        bruto_na_middernacht <- round(as.double(na_middernacht)*uurloon*1.25,2)
        row["oncomfortabele_uren"] = voor_middernacht + na_middernacht
        row["bruto_oncomfortabele_uren_euro"] <- bruto_voor_middernacht+bruto_na_middernacht
      } else {
        row["comfortabele_uren"] <- 0
        row["bruto_comfortabele_uren_euro"] <- 0
        row["oncomfortabele_uren"] = as.double(as_hms((uitgetikt-ingetikt)))/60/60
        row["bruto_oncomfortabele_uren_euro"] <- round(as.double(row["oncomfortabele_uren"])*uurloon*1.5,2)
      }
    }
  
  return(row)
})

totaal <- calc %>% t()
totaal <- data.frame(totaal)
#totaal$bruto_comfortabele_uren_euro <- ifelse(df$verlof == TRUE, 0, totaal$bruto_comfortabele_uren_euro )
#totaal$bruto_oncomfortabele_uren_euro <- ifelse(df$verlof == TRUE, 0, totaal$bruto_oncomfortabele_uren_euro )
totaal$som_bruto <- as.double(totaal$bruto_comfortabele_uren_euro) + as.double(totaal$bruto_oncomfortabele_uren_euro)
totaal <- totaal %>% select(dag, ingetikt, uitgetikt, weekdag, verlof, recup, feestdag, comfortabele_uren,
                  bruto_comfortabele_uren_euro,
                  oncomfortabele_uren, bruto_oncomfortabele_uren_euro, som_bruto, uren_gewerkt_per_week)

totaal

  