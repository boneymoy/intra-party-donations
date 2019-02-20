
# Untersuchen von Spenden von Politiker*innen an die eigene Partei 
# liefert Liste (sumOfSelfDonations) mit allen Politiker*innen welche an ihre eigene Partei gespendet haben der größe nach sortiert
# Bisheriger Stand der Daten:
#   Spenden natürlicher Personen von 2000 bis 2019 -> 8864 Spenden
#   aber nur aktuell in Land- oder Bundestag sitzende Politiker*innen (abgeordnetenwatch API nicht ganz durschaut) -> 2698 (doppelte Personen in Liste aus xml)
#     Anzahl Personen in Landesparlamenten: 1.873
#     Anzahl Personen in BundesTag:         706
#     Gesamt:                               2.579
#   (Stand 14.02.2019) 

library(rjson)
library(stringr)
require(XML)

#TODO Pfad automatisch auslesen?
#Pfad angeben
getwd
setwd("...\\intra-party-donations")


read.Lobbypedia <- function(lobbyCSV = "parteispenden.csv"){
  # Parteispenden aller natuerlichen Personen von:
  # https://lobbypedia.de/wiki/Spezial:Abfrage_ausf%C3%BChren/Parteispenden
  
  # TODO: abfrage automatisch ausführen und csv speichern
  data <- read.csv(lobbyCSV, encoding = "UTF-8")
  
  # Datenvorverarbeitung der Parteispenden 
  #  Nicht benoetigte Spalten entfernen
  #  Noch enthaltene Spalten: data$Geldgeber/Betrag/Empfaenger/Jahr/Ort
  data$X <- NULL
  data$Kategorie <- NULL
  data$Bundesland <- NULL
  data$Schlagworte <- NULL
  data$Branche <- NULL
  return(data)
}
# Datenvorverarbeitung der Politiker*innen 
# XML von abgeordnetenwatch.de API herunterladen
#  Einzeln alle Politiker*innen pro Landtag und Bundestag ausgeben

#   nur ueber Bundeslaender moeglich ???

#   Baden WÃ¼rttemberg https://www.abgeordnetenwatch.de/api/parliament/baden-w%C3%BCrttemberg/deputies.xml 
#   Bayern https://www.abgeordnetenwatch.de/api/parliament/bayern/deputies.xml
#   Berlin https://www.abgeordnetenwatch.de/api/parliament/berlin/deputies.xml
#   Brandenburg https://www.abgeordnetenwatch.de/api/parliament/brandenburg/deputies.xml
#   Bremen https://www.abgeordnetenwatch.de/api/parliament/bremen/deputies.xml
#   Hamburg https://www.abgeordnetenwatch.de/api/parliament/hamburg/deputies.xml
#   Hessen https://www.abgeordnetenwatch.de/api/parliament/hessen/deputies.xml
#   Mecklenburg-Vorpommern https://www.abgeordnetenwatch.de/api/parliament/mecklenburg-vorpommern/deputies.xml
#   Niedersachsen https://www.abgeordnetenwatch.de/api/parliament/niedersachsen/deputies.xml
#   Nordrhein-Westfalen https://www.abgeordnetenwatch.de/api/parliament/nordrhein-westfalen/deputies.xml
#   Rheinland-Pfalz https://www.abgeordnetenwatch.de/api/parliament/rheinland-pfalz/deputies.xml
#   Saarland https://www.abgeordnetenwatch.de/api/parliament/saarland/deputies.xml
#   Sachsen https://www.abgeordnetenwatch.de/api/parliament/sachsen/deputies.xml
#   Sachsen-Anhalt https://www.abgeordnetenwatch.de/api/parliament/sachsen-anhalt/deputies.xml
#   Schleswig-Holstein https://www.abgeordnetenwatch.de/api/parliament/schleswig-holstein/deputies.xml
#   ThÃ¼ringen https://www.abgeordnetenwatch.de/api/parliament/th%C3%BCringen/deputies.xml

read.abgeordnetenwatch <- function(XML){
  
  # TODO: auch hier xml automatisch herunterladen und speichern
  # Erstellen einer Matrix mit allen Mitarbeiter*innen + jeweilige Partei
  # Alle Landtage (+ Bundestag) xmls durchlaufen
  # und benoetigte Informationen extrahieren
  # 
  # input: XML Datei
  # return: Zweispalten matrix mit Name + Partei 
  
  # parliament enthält infos aus xml für alle personen eines parlamentes
  # parliament in Liste umwandeln 
  # benoetigte informationen in parliament[i]$profile$personal$first_name/last_name/degree
  counter <- 1
  temp <- xmlParse(XML)
  parliament <- xmlToList(temp)
  namelist <- matrix(nrow = length(parliament), ncol = 2)
  for (i in 2:length(parliament)){
    #  singlenamelist mit Namensstring "last_name,[degree] first_name" und 
    #  Partei party erstellen
    singlenamelist <- convert.xmlEntry(parliament[i]$profile$personal$first_name, parliament[i]$profile$personal$last_name, parliament[i]$profile$party, parliament[i]$profile$personal$degree)
    # erstellte Liste singlenamelist in gesamte Matrix einfuegen
    namelist[counter,1] <- singlenamelist[[1]]
    namelist[counter,2] <- singlenamelist[[2]]
    counter <- counter + 1
  }
  
  return(namelist) 
}

convert.xmlEntry <- function(first_name, last_name, party, degree){
  #  Name der Person aus xml-Liste extrahieren
  #
  #  input: Vor-, Nachname, Partei, Grad. Als XML-Listeneintrag 
  #  return: singlenamelist 2 Elemente 
  #          String(Nachname, Grad Vorname),
  #          String Partei
  #
  #  unterscheide ob Dr./Prof. oder nicht ( falls ja zusaetzliches Leerzeichen)
  if (is.null(degree)){
    singlenamelist <- list(paste0(last_name,", ", first_name), party) 
  } else {
    singlenamelist <- list(paste0(last_name,", ", degree, " ", first_name), party) 
  }
  return(singlenamelist)
}

read.multipleXML <- function(xml_list){
  # Auslesen von Dateiname von XML Datei aus liste und weitergabe an read.abgeordnetenwatch
  #
  # input: Liste von XML Dateinamen
  # return: Liste mit Namen aus Abgeordnetenwach von Bundestag und allen Landtagen
  names <- matrix(ncol = 2)
  for (i in 1:length(xml_list)){
    print(paste0("Read in: ",xml_list[i], "..."))
    print(xml_list[i])
    names <- rbind(names,read.abgeordnetenwatch(xml_list[i])) 
    print("Done!")
  }
  print("Finished")
  return(names)
}

extract.Integer <- function(value){
  # extract number out of data.frame format ??? bessere LÃ¶sung suchen
  # String aus donationList$Betrag ziehen, Format "$ 11[..]11,11" 
  # -> ersten zwei Zeichen ignorieren und dann Zeichen 3 - (n-3) bis komma + Stellen nach dem Komma*0,01 
  value <- substr(as.character(value),3,str_length(value))
  value <- strtoi(substr(value,1,str_length(value)-3)) +
           strtoi(substr(value,str_length(value)-1,str_length(value)))*0.01
  return(value)
}

filter.pennielessness <- function(sumOfDonations){
  # Entferne Personen welche nicht an ihre eigene Partei gespendet haben
  counter <- 1
  filtered_donation_list <- matrix(nrow = nrow(sumOfDonations), ncol = 3)
  for (i in 1:nrow(sumOfDonations)){
    if (is.na(sumOfDonations[i,3])) {
      next
    }
    if (sumOfDonations[i,3] != 0){
      filtered_donation_list[counter,] <- sumOfDonations[i,]
      counter <- counter + 1
    }  
  }
  donations.df <- data.frame(filtered_donation_list)
  # leere Einträge aus Liste entfernen
  donations.df <- donations.df[!is.na(donations.df$X1),]
  # Nach gespendetem Betrag sortieren
  donations.df <- donations.df[order(donations.df$X3),]
  donations.df<- donations.df[seq(dim(donations.df)[1],1),]
  # Mehrere Namen doppelt in xml von abgeordnetenwatch
  # Duplikate finden und Zeilen entfernen
  donations.df <- donations.df[!duplicated(donations.df,incomparables = FALSE),]
  
}

search.selfDonations <- function(donationList, namelist){
  
  # In selfdonate summe der spenden an eigene partei fuer jede person (Spalte 1:Name,Spalte2:Betrag,Spalte3:Partei)
  
  sumOfDonations <- matrix(nrow = nrow(namelist), ncol = 3)
  sumOfDonations[,1:2] <- namelist
  sumOfDonations[1:nrow(sumOfDonations),3] <- 0
  
  # hat Person von abgeortnetenwatch (namelist) gespendet ?
  # spender, liste mit Booleanwert ob Person gespendet hat
  
  spender <- namelist[ ,1] %in% donationList$Geldgeber
  
  # ist Spende an eigene partei? (namelist[i+1]==data$EmpfÃ¤nger)
  # Durlaufe alle Spenden
  # wenn Person gespendet hat (spender == true) 
  #   ueberpruefe fuer jede spende geldgeber und vergleiche mit namelist[i,1] Name, wenn gleich
  #     ueberpruefe fuer jede spende empfaenger und vergleiche mit namelist[i,2] Partei
  #     bei Treffer Betrag in sumOfDonations summieren
  
  for (j in 1:length(donationList$Geldgeber)){
    print(paste0(j, " von ", length(donationList$Geldgeber)))
    for (i in 1:nrow(namelist)){
      if (spender[i]){
        if (namelist[i,1] == donationList$Geldgeber[j]){
          if (namelist[i,2] == donationList$Empfänger[j]){
            
            value <- extract.Integer(donationList$Betrag[j])
            sumOfDonations[i,3] <- as.numeric(sumOfDonations[i,3]) + value
          
          }
          
        }
      }
    }
  }
  # Entferne Personen welche nicht an ihre eigene Partei gespendet haben
  # Weitere Anpassungen
  donations.df <- filter.pennielessness(sumOfDonations)
  
  return(donations.df)
}


donations <- read.Lobbypedia("parteispenden.csv")

xml_list <- c("bundestag.xml", "berlin.xml","BW.xml","bayern.xml","thueringen.xml","brandenburg.xml","bremen.xml","hamburg.xml","hessen.xml", "meckpom.xml","niedersachsen.xml","nrw.xml","saarland.xml","sachsen.xml","anhalt.xml","holstein.xml","pfalz.xml")
namelist_abgeordnetenwatch <- read.multipleXML(xml_list)

sumOfSelfDonations <- search.selfDonations(donations, namelist_abgeordnetenwatch)
