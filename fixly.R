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
  wykonawcy_w_kategorii_page <- wykonawcy_w_kategorii_page[sapply(wykonawcy_w_kategorii_page, function(x) grepl("https://fixly.pl/profil/",x)) == TRUE] %>%
    map_chr(function(x) gsub("#feedback", "", x))
  wykonawcy_wg_kategorii[[i]] <- data.frame(wykonawca_link = wykonawcy_w_kategorii_page, kategoria = kategorie_nazwy[i])
  Sys.sleep(sample(seq(0.1,1,0.1),1))
}

wykonawcy_i_kategorie_df <- bind_rows(wykonawcy_wg_kategorii)
wykonawcy_i_kategorie_df$wykonawca_nazwa <- map_chr(wykonawcy_i_kategorie_df$wykonawca, function(x) gsub("https://fixly.pl/profil/", "", x))

print(
  wykonawcy_i_kategorie_df %>%
    group_by(wykonawca_nazwa) %>%
    count() %>%
    arrange(desc(n))
)
wykonawcy <- unique(wykonawcy_i_kategorie_df$wykonawca_link)
wykonawca_ile <- length(wykonawcy)

wyscrapuj_po_xpath <- function(podstrona, xpath) {
  return(podstrona %>% html_nodes(xpath=xpath))
}

czy_url_dziala <- function(url){
  tryCatch(
    identical(status_code(HEAD(url)),200L), 
    error = function(e){
      FALSE
    })
}

# teraz trzeba by wyciągnąć dane o każdym wykonawcy
wykonawcy_profil<- vector(mode = "list", length = wykonawca_ile)

for(i in 1:wykonawca_ile) {
  if(!czy_url_dziala(wykonawcy[i])) { 
    wykonawcy_profil[[i]] <- "NA"
    next }
  
  podstrona <- read_html(wykonawcy[i])
  
  liczba_gwiazdek <- wyscrapuj_po_xpath(podstrona, '//*[contains(concat( " ", @class, " " ), concat( " ", "fa-star", " " ))]') %>%
    length()
  lokalizacja <- wyscrapuj_po_xpath(podstrona, '//*[contains(concat( " ", @class, " " ), concat( " ", "publicProfile__address", " " ))]') %>%
    html_text() %>%
    str_replace_all("\n", "") %>%
    trimws(which="both")
  feedback_kiedy <- sapply(podstrona, function (x) wyscrapuj_po_xpath(podstrona, 
                                                                      '//*[contains(concat( " ", @class, " " ), concat( " ", "publicProfile__reviews-time", " " ))]'), 
                           simplify = FALSE)$doc
  feedback_kiedy <- feedback_kiedy %>%
    html_text() %>%
    str_replace_all("\n", "") %>%
    trimws(which = "both")
  
  feedback_kto_co <- sapply(podstrona, function (x) wyscrapuj_po_xpath(podstrona, 
                                                                       '//*[contains(concat( " ", @class, " " ), concat( " ", "publicProfile__reviews-data-wrapper", " " ))]'),
                            simplify = FALSE)$doc
  feedback <- feedback_kto_co %>%
    html_nodes('p') %>%
    html_text() %>%
    str_replace_all("\n", "") %>%
    trimws(which="both") %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    data.frame()
  colnames(feedback) <- c("klient", "opinia")
  feedback$data <- feedback_kiedy
  feedback <- subset(feedback, select=-c(klient)) # upuszczamy kolumnę identyfikującą klientów -- łatwiej było ściągnąć te dane niż nie ze względu na układ strony, ale nie będziemy ich potrzebować
  
  liczba_kategorii_na_profilu <- sapply(podstrona, function (x) wyscrapuj_po_xpath(podstrona,
                                                                      '//*[contains(concat( " ", @class, " " ), concat( " ", "publicProfile__to-category", " " ))]'),
                           simplify = FALSE)$doc %>%
    html_text() %>%
    length()
  
  wykonawcy_opis_na_profilu <- wyscrapuj_po_xpath(podstrona,
                                                  '//*[contains(concat( " ", @class, " " ), concat( " ", "publicProfile__details", " " ))]') %>%
    html_text() %>%
    str_replace_all("\n|\r", "") %>%
    trimws(which = "both")
  
  wykonawcy_profil[[i]] <- list(feedback, liczba_gwiazdek, lokalizacja, wykonawcy_opis_na_profilu, liczba_kategorii_na_profilu)
  names(wykonawcy_profil[[i]]) <- c("Feedback", "Ocena_srednia", "Lokalizacja", "Opis_na_profilu", "Liczba_kategorii")
  
  Sys.sleep(sample(seq(0.1,1,0.1),1))
}

names(wykonawcy_profil) <- wykonawcy
