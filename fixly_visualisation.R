library(tidyverse)
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