library(dplyr)
## some pipe operator fun
# zadanie 2

summarise(iris, Sepal.Length.Sum = sum(Sepal.Length),
          Sepal.Width.mean = mean(Sepal.Width),
          Petal.Len.Median = median(Petal.Length),
          Petal.Width.Var = var(Petal.Width))

by_species <- iris %>%
  group_by(Species)

summarise(by_species, Sepal.Length.Sum = sum(Sepal.Length),
          Sepal.Width.mean = mean(Sepal.Width),
          Petal.Len.Median = median(Petal.Length),
          Petal.Width.Var = var(Petal.Width))


## more fun with summarizing and arranging

dane <- read.csv("http://www.biecek.pl/R/dane/danebiotech.csv", sep = ';', dec = ',')

# select the listed columns and save
dane %>%
  select(Wiek, Płeć.K.0.M.1, WIT, Kreatynina.1, Kreatynina.3, Kreatynina.7) -> dane

# arrange by WIT descending and Wiek
dane %>%
  arrange(desc(WIT), Wiek)

# group by sex and show means of some columns
dane %>%
  group_by(Płeć.K.0.M.1) %>%
  summarise(mean_age = mean(Wiek), 
            mean_k_1 = mean(Kreatynina.1),
            mean_k_3 = mean(Kreatynina.3),
            mean_k_7 = mean(Kreatynina.7))

dane %>%
  arrange(Płeć.K.0.M.1,desc(WIT), Wiek) -> dane

kobiety <- dane[dane$Płeć.K.0.M.1 == 'K',]
mezczyzni <- dane[dane$Płeć.K.0.M.1 == 'M',] 

# show means for women by WIT parameter
kobiety %>%
  group_by(WIT) %>%
  summarise(mean_age = mean(Wiek), 
            mean_k_1 = mean(Kreatynina.1),
            mean_k_3 = mean(Kreatynina.3),
            mean_k_7 = mean(Kreatynina.7))

#show means for men by WIT parameter
mezczyzni %>%
  group_by(WIT) %>%
  summarise(mean_age = mean(Wiek), 
            mean_k_1 = mean(Kreatynina.1),
            mean_k_3 = mean(Kreatynina.3),
            mean_k_7 = mean(Kreatynina.7))


# create factor with ages
faktor_wiekow <- cut(dane$Wiek, breaks = seq(min(dane$Wiek), max(dane$Wiek)+10, by= 10), include.lowest = T)
dane <- mutate(dane, Wiek = faktor_wiekow)
# summarize with age intervals
dane %>%
  group_by(Wiek) %>%
  summarise(mean_k_1 = mean(Kreatynina.1),
            mean_k_3 = mean(Kreatynina.3),
            mean_k_7 = mean(Kreatynina.7))
