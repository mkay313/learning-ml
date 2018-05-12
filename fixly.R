library(rvest)
library(tidyverse)

# wyciągamy linki do poszczególnych kategorii
kategorie_page <- read_html("https://fixly.pl/kategorie")
linki <- kategorie_page %>%
  html_nodes("a") %>%
  html_attr("href")
kategorie <- linki[sapply(linki, function(x) grepl("https://fixly.pl/kategoria/",x)) == TRUE]
kategorie_ile <- length(kategorie)

#nazwy kategorii
kategorie_nazwy <- map_chr(kategorie, function(x) gsub("https://fixly.pl/kategoria/", "", x))

# wyciągamy wykonawców z poszczególnych kategorii
wykonawcy_wg_kategorii <- vector(mode = "list", length = kategorie_ile)
names(wykonawcy_wg_kategorii) <- kategorie_nazwy
for (i in 1:kategorie_ile) {
  podstrona <- read_html(kategorie[i])
  wykonawcy_w_kategorii_page <- podstrona %>%
    html_nodes("a") %>%
    html_attr("href")
  wykonawcy_w_kategorii_page <- wykonawcy_w_kategorii_page[sapply(wykonawcy_w_kategorii_page, function(x) grepl("https://fixly.pl/profil/",x)) == TRUE]
  wykonawcy_wg_kategorii[[i]] <- map_chr(wykonawcy_w_kategorii_page, function(x) gsub("#feedback", "", x))
  Sys.sleep(sample(seq(0.1,1,0.1),1))
}

# przeorganizujmy trochę dane, żeby atrybuty wykonawców były podelementami wykonawców jako elementów naszej głównej listy danych
# możemy śmiało odlistować wszystkie dane -- pozyskamy potem informację o wszystkich miejscach, w których znajduje się wykładowca
# samo info, na której kategorii znaleźliśmy danego wykonawcę też zapiszemy -- może coś nam powie jak są wybierani wykonawcy pokazywani na podstronach?

