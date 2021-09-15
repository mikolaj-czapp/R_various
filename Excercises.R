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


## basic plots 
#plot some icons on the diagonal

points <- c(0:9, 'abcd')
plot(1:25, 1:25, type = 'n')
for (i in 1:25){
  points(i ,i ,pch = i, xlim = c(0,25), ylim = c(0,25), cex = 2)
}


# plot normal distribution with some area highlighted

x <- seq(-2,2, 0.01)
plot(x,dnorm(x), type = 'l')

polyx <- c(seq(-1,1, 0.01))
polyy <- dnorm(polyx)

polyy <- append(polyy, c(0,0))
polyx <- append(polyx, c(1,-1))


polygon(x=polyx, y=polyy, density = 5, angle = 45, col = 'red')
arrows(x0=c(-1.2, 1.2), y0=(dnorm(c(-1.4, 1.4))+0.15), x1=c(-1,1), y1=dnorm(c(-1,1)), length = 0.1)
text(c(-1.2, 1.2), dnorm(c(-1.4, 1.4))+0.16, dnorm(c(-1,1)))


# plot some points with colours

a <- heat.colors(10)
b <- terrain.colors(10)
c <- topo.colors(10)
d <- cm.colors(10)
r <- rainbow(40)

radi = seq(0.5,1,0.5/40)
?symbols
symbols(1:10, 1:10, squares = 1:10, fg = a)
{tmp <- rep(c(0.4), 10)
  tmp <- append(tmp, rep(c(2), 10))
  tmp <- append(tmp, seq(0.1,1,0.1))
  termometry <- matrix(tmp, ncol = 3)
  
  thermometers = cbind(.5, 1, 0.6)
}
symbols(1:10, thermometers = termometry, fg = b)

plot(1:10, pch = 19, col= b, cex = 5)
plot(1:10, pch = 19, col= c, cex = 5)
plot(1:10, pch = 19, col= d, cex = 5)

t <- seq(0,pi, 0.1)
plot(0,0,type='n')

for (i in 1:40){
  lines(radi[i]*cos(t), radi[i]*sin(t), col= r[i], lwd = 8)
}

rect(-0.5,-0.5,0.5,0.5,density = 40, angle = 30, col = b[4])


library(dplyr)
library(ggplot2)
library(Przewodnik)

# some basic geometries

#plot average of 50 numbers
d=data.frame(x=1:50, y=sample(0:2, 50, replace = T))
ggplot() +
  geom_point(data=d, mapping=aes(x=x, y=y)) +
  geom_hline(yintercept=mean(d$y), color="red")


#bar chart of some cars
liczba <- summarise(group_by(mpg, class), liczba = n())

ggplot(mpg, aes(class)) + geom_bar() + geom_text(data = liczba, mapping = aes(x = class, y =liczba+5, label = liczba))


#plot with bitcoin price

bitcoin_stock <- read.csv("BTC-USD.csv", dec = '.')
bitcoin_stock <- mutate(bitcoin_stock, Date = as.Date(Date))
ggplot(data = bitcoin_stock, aes(x = Date, y = Close, group = 1)) +
  geom_line(color = '#E51837', size = .6) +
  scale_y_continuous(breaks=seq(0,70000,1500)) +
  scale_x_date(date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

# now with some augmentations
library(scales)

bitcoin_stock <- read.csv("BTC-USD.csv", dec = '.')
# make values and dates more readable
bitcoin_stock <- mutate(bitcoin_stock, Date = as.Date(Date))
btc <- ggplot(data = bitcoin_stock, aes(x = Date, y = Close, group = 1)) + geom_line(color = '#E51837', size = .6)

btc + scale_y_continuous(n.breaks = 25)

# values in USD
btc + scale_y_continuous(labels = dollar)
# scientific values
btc + scale_y_continuous(labels = scientific)

# change some date formats
# month/day
btc + scale_x_date(labels = date_format("%m/%d")) 
# month/day/year
btc + scale_x_date(labels = date_format("%D"))
# breaks every 6 months
btc + scale_x_date(breaks = date_breaks(width = '6 months'),
                   labels = date_format("%Y"))



# some colours for plotted functions

pierwsza <- function(x) {x-2}
druga <- function(x) {sin(x)}
trzecia <- function(x) {sqrt(exp(x))}

ggplot(data.frame(x = c(-5,5)), aes(x = x)) +
  stat_function(fun = pierwsza, aes(colour = "deeppink")) +
  stat_function(fun = druga, aes(colour = "#C12527")) +
  stat_function(fun = trzecia, aes(colour = "green"))  +
  scale_colour_manual("colours", values = c("deeppink", "#C12527", "green"))



#some fun with log and sqrt scales

speed <- ggplot(cars, aes(x = speed, y = dist)) + geom_point()

speed + scale_y_log10()

speed + scale_y_sqrt()

nums <- 1:100
df <- data.frame(x = nums, y=nums)

(aa <- ggplot(df, aes(x=x, y=y)) + geom_point())
aa + scale_y_log10()
aa + scale_x_sqrt()
aa  + scale_y_log10() + scale_x_log10()

