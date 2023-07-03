#Alle benoetigten Packages sind hier aufgelistet. 
#Es wird zudem das Exiftool(-k).exe benoetigt. Dieses kann online
#heruntergeladen werden und muss in die Working Directory importiert werden.
library(jpeg)
library(tidyverse)
library(pixmap)
library(imager)
library(magick)
library(reshape2)
library(plotly)
library(sp)
library(rgdal)
library(terra)
library(sf)


#Hier wird der maximale Winkel der Drohne (WD) und der gesamte Blickwinkel (BW) definiert.

#WD entspricht dem maximalen Winkel, welchen die Drohne in den Himmel schauen kann.
#Beispiel fuer WD: Bei einem Kameraneigungswinkel von +10° und einem FOV der Drohne von 77°,
#betraegt der WD 42. Denn 10° + 77°/2 = 48° --> 90°(Zenit) - 48° = 42°. Der Blickwinkel der Drohne,
#muss von den 90° abgezogen werden um den WD zu erhalten

#BW entspricht dem gesamten Blickwinkel der Drohne. Beispiel: Wenn Bilder mit +10° Neigunswinkel
#und -10° Neigungswinkel gemacht werden und der FOV der Drohne 77° betraegt, liegt der BW bei 97.
#Denn 77° + (+10°-(-10°)) = 97°

WD <- 42
BW <- 97



#####Albedo bestimmen

#Bild einlesen bei "load.image" (muss .jpg sein).
#X- und Y- Koordinaten werden zu rad und anschliessend direkt
#zu Bogenmass umgewandelt (Zenit und Azimut).
B1 <- load.image("Pano_8.jpg") %>%
           grayscale() %>%
             as.data.frame() %>%
              select(x,y,value) %>%
               mutate(pixel = max(y)/BW*180,
                      x = x-1,
                      y = y-1+(pixel/180*WD)
                      ) %>%
              select(x,y,value,pixel) %>%
               mutate(x = (2*pi*x)/max(x)-pi,
                      y = pi*y/pixel,
                      value = value
                      )%>%
              select(x,y,value) %>%
               mutate("Grad_Azimut" = x/(pi/180),
                      "Grad_Zenit" = y/(pi/180),
                      value = value
                      )



#Legt ein Raster ueber das Bild, um den gewuenschten Bereich besser auszuwaehlen
ggplot(B1,aes(x = Grad_Azimut, y = Grad_Zenit))+
  geom_raster(aes(fill=value))+
  scale_y_reverse(breaks = c(0,20,40,60,80,90,100,120,140,160,180))+
  scale_x_continuous(breaks= c(-180,-160,-140,-120,-100,-80,-60,-40,-20,0,20,40,60,80,100,120,140,160,180))+
  scale_fill_gradient(low = "black", high = "white")+
  geom_vline(xintercept = c(-180,-160,-140,-120,-100,-80,-60,-40,-20,0,20,40,60,80,100,120,140,160,180), color="white", size=0.1)+
  geom_hline(yintercept = c(0,20,40,60,80,90,100,120,140,160,180), color="white", size=0.1)+
  labs(x = "Grad Azimut", y = "Grad Zenit")+
  theme(aspect.ratio = 0.5)


#Noerdliche und suedliche Hemispaehre koennen anhand der Winkelbereiche ausgewaehlt werden.
#Siehe Rasterplot.
Globalstrahlung <- B1 %>% filter(Grad_Azimut > -170 & Grad_Azimut < 170 & 
                         Grad_Zenit > 55 & Grad_Zenit < 90)

Messbereich <- B1 %>% filter(Grad_Azimut > -170 & Grad_Azimut <170 & 
              Grad_Zenit > 100 & Grad_Zenit <130)


#Plotten von den ausgewaehlten Bereichen um bildlichen Eindruck 
#der Auswahl zu bekommen und Auswahl mit interaktivem Plot zu verifizieren.
#Auswahl, kann anschliessend verbessert werden
ggplotly(ggplot(Globalstrahlung,aes(x = Grad_Azimut, y = Grad_Zenit)) +
           geom_raster(aes(fill=value))+
           scale_y_reverse()+
           scale_fill_gradient(low = "black", high = "white")+
           labs(x = "Grad Azimut", y = "Grad Zenit"))


ggplotly(ggplot(Messbereich,aes(x = Grad_Azimut, y = Grad_Zenit)) +
           geom_raster(aes(fill=value))+
           scale_y_reverse()+
           scale_fill_gradient(low = "black", high = "white")+
           labs(x = "Grad Azimut", y = "Grad Zenit"))


#Praezisere Auswahl falls noetig
Messbereich <- B1 %>% filter(Grad_Azimut > 47& Grad_Azimut <127.6 & 
                               Grad_Zenit > 101.5 & Grad_Zenit <105.7)


#Korrektur der Werte, um die Verzerrung durch die Projektion auf eine Kugel zu korrigieren
#Dazu wird das gewichtete Mittel verwendet.

#Gewichtetes Mittel Globalstrahlung
GS <- Globalstrahlung %>%
        select(Grad_Azimut, Grad_Zenit, value) %>%
          mutate(flaeche = 1 * sin(Grad_Zenit * (pi/180)),
                 weightedValue = value * flaeche)

WeightedGS <- weighted.mean(GS$weightedValue, GS$flaeche)


#Gewichtetes Mittel Messbereich
MB <- Messbereich %>%
        select(Grad_Azimut, Grad_Zenit, value) %>%
          mutate(flaeche = 1 * sin(Grad_Zenit * (pi/180)),
                weightedValue = value * flaeche)

WeightedMB <- weighted.mean(MB$weightedValue, MB$flaeche)



#Berechnen des prozentualen Anteil der reflektierten Strahlung 
#von dem ausgewaehlten Bereich.
Albedo <- WeightedMB/WeightedGS
Albedo



#####Bild Georeferenzieren

#Als erstes Exiftool in die Working Directory ziehen und 
#Bilddate mit Namen auswaehlen. "-n" und "-csv" koennen belassen werden. 
#Es muss beachtet werden, dass das ausgewaehlte Bild die benoetigten GPS informationen enthaelt
#Kann auch im Windows explorer verifiziert werden.
output <- system("exiftool(-k).exe -n -csv GPS_Pano_8.jpg", intern=TRUE)
Bild_Metadaten <- read.csv(textConnection(output), stringsAsFactors = FALSE)

#Data Frame mit Laenge, Breite und Albedowert erstellen
xy<-data.frame(x = Bild_Metadaten$GPSLongitude,
               y = Bild_Metadaten$GPSLatitude,
               Albedo = Albedo)

#SpatialPointsDataFrame erstellen mit Koordinatensystem WGS84
latslongs <- SpatialPointsDataFrame(coords=xy[,c(1,2)],data=xy,proj4string
                                    =CRS("+proj=longlat + ellps=WGS84"))

#GPX Datei erstellen und Pfad zum Speicherungsort, 
#so wie Namen, welchen die Datei bekommen sollte, angeben.
#GPX Datei wird anschliessend gespeichert und kann in QGIS importiert werden.
writeOGR(latslongs, dsn=
          "C:/Users/XXXX/OneDrive - ZHAW/6. Sem FS23/BA/QGIS_Waypoints/GPS_Pano_8.GPX",
         dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",
         driver="GPX", overwrite_layer = T)

