library(tidyverse)
library(plotrix) #rescale
library(ggmap) #google maps api --  do pozyskania danych o lokalizacji
#source("fixly.R")

print(head(wykonawcy_polaczeni %>% arrange(desc(n))))
print(head(wykonawcy_polaczeni %>% arrange(desc(Liczba_kategorii))))

# wprowadźmy wskaźnik liczby kategorii, w których mamy podpiętą opinię do ogólnej deklarowanej liczby kategorii -- im wyższa wartość, tym lepiej:
# (jeśli konto zostało usunięte, to możemy mieć wartość inf -- przy dalszej analizie trzeba się tym potencjalnie zająć, żeby móc pokazać wykres)
wykonawcy_polaczeni <- wykonawcy_polaczeni %>% 
             mutate(Opinie_do_kategorii=n/Liczba_kategorii) %>%
             arrange(desc(Opinie_do_kategorii))

print(head(wykonawcy_polaczeni %>% filter(Liczba_kategorii>5), n=10))

wykonawcy_polaczeni$Opinie_do_kategorii <- ifelse(wykonawcy_polaczeni$Opinie_do_kategorii > 1, 0, wykonawcy_polaczeni$Opinie_do_kategorii)

ggplot(data = wykonawcy_polaczeni, aes(x=Liczba_kategorii, y=n)) + 
  geom_point(alpha = 0.5, color = "navy") + 
  labs(title="Większość kontrahentów nie ma opinii na stronach kategorii", 
       x="Liczba kategorii deklarowanych na profilu", 
       y="Liczba kategorii, w których ma się 'promowaną' opinię")

print(wykonawcy_polaczeni %>% group_by(Lokalizacja) %>% count() %>% arrange(desc(nn)))

mapa <- map_data("world") %>% filter(region == "Poland")
miasta <- wykonawcy_polaczeni$Lokalizacja[wykonawcy_polaczeni$Lokalizacja != "NA"]
miasta <- as.data.frame(table(miasta))
miasta$wsp_frekwencji <- rescale(miasta$Freq, c(1,10))
miasta$lon <- rep_len(NA, nrow(miasta))
miasta$lat <- rep_len(NA, nrow(miasta))

while(any(is.na(miasta$lon))) {
  for (i in 1:nrow(miasta)) {
    print(i)
    if(is.na(miasta$lon[i])) {
      lonlat <- geocode(as.character(miasta$miasta[i]))
      miasta$lon[i] <- lonlat$lon
      miasta$lat[i] <- lonlat$lat
      Sys.sleep(1)}}}

ggplot() + 
  geom_polygon(data = mapa, aes(long, lat), color = "black", fill = "white") + 
  geom_point(data = miasta, aes(lon,lat, size=Freq), color = "blue", alpha = 0.5, show.legend = FALSE) + 
  geom_text(data=subset(miasta, Freq>1), aes(x=lon,y=lat,label=miasta), nudge_y = 0.1) + 
  coord_map() + 
  theme_void() + 
  labs(title = "Większe miasto ma więcej wykonawców, ale nie zawsze. Co z Łodzią?")
