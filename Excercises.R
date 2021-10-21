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




library(ggplot2)
library(dplyr)
library(ggthemes)
library(grid)
library(scales)
library(khroma)
library(extrafont)

#some fun with visualizing data

avocado <- read.csv("avocado.csv", sep = ',')

avocado <- mutate(avocado, Date = as.Date(Date))

#plot with dot representing the median, the empty space is interquartile range,
#and the bars are the scope
plot <- ggplot(avocado, aes(x=factor(region), y=AveragePrice)) 
plot + theme_tufte(ticks = F) + geom_tufteboxplot() + 
  theme(axis.text.x = element_text(angle = 45))

# Google Docs style (max 10 colours)

s <- sample(levels(factor(avocado$region)), 8)
avocado2 <- avocado %>% filter(region %in% s)

plot2 <- ggplot(avocado2, aes(x=Date, y=Total.Volume, colour = factor(region)))
plot2 + theme_gdocs() + scale_color_gdocs() + geom_point()


#Wall Street Journal style (max 6 colours)

s <- sample(levels(factor(avocado$region)), 3)
avocado3 <- avocado %>% filter(region %in% s)

plot3 <- ggplot(avocado3, aes(x=Date, y=AveragePrice, colour = factor(region)))
plot3 + theme_wsj() + scale_colour_wsj("colors6", "") + geom_point()


#The Economist style

plot4 <- ggplot(avocado3, aes(x=Date,y=AveragePrice, fill = factor(region)))
plot4 + theme_economist() + scale_colour_economist() + geom_bar(stat = 'identity')


# Paul Tol style

avocado5 <- avocado %>% filter(region %in% c('Detroit', 'GreatLakes'))

plot5 <- ggplot(avocado5, aes(x=Date, y=Total.Bags, colour = factor(region)))
plot5 + geom_smooth() + scale_color_ptol("cyl") + theme_minimal() + geom_point()


# Solarized style


plot6 <- ggplot(avocado3, aes(x=Total.Volume, y=Large.Bags, colour = factor(region)))
plot6 + geom_point()  + theme_solarized() + scale_colour_solarized('blue') + facet_wrap(~ region)

# Excel style

plot7 <- ggplot(avocado3, aes(x=Total.Volume, y=Total.Bags, colour = factor(region)))
plot7 + geom_point() + theme_excel() + scale_colour_excel() + facet_grid(~region)

# highcharts

plot7 + geom_point() + theme_hc() + scale_colour_hc()



#Some coordinate transformation

pl + geom_smooth(method = 'lm') + coord_trans(x = "log10")
pl + geom_smooth(method = 'lm') + scale_x_log10()

# The difference between transforming the scales and
# transforming the coordinate system is that scale
# transformation occurs BEFORE statistics, and coordinate
# transformation afterwards.  Coordinate transformation also
# changes the shape of geoms.

pl + coord_trans(x = "sqrt", xlim = c(100000, 500000), ylim=c(500, 200000))



#Big avocado plot (I think the line colours are switched, but I didn't care
#to fix that)


avocado_final <- avocado %>% filter(region == 'Seattle')



plot_final <- ggplot(avocado_final, aes(x=Total.Volume, y=AveragePrice, colour = type)) + ggtitle("Avocado Prices in Seattle and the US, 2015-2018")
plot_final <- plot_final + geom_point() + geom_smooth(method = "lm", se = FALSE) + scale_x_log10(n.breaks = 8) + scale_y_continuous(labels = dollar)
plot_final <- plot_final + theme_economist() + scale_colour_economist()
plot_final <- plot_final + labs(x = "Total number of avocados sold", y = "Average price of a single avocado")
plot_final <- plot_final + theme(
  axis.title.x = element_text(size = 12, face = "bold", vjust = -1),
  axis.title.y = element_text(size = 12, face = "bold", vjust =4),
  
  plot.title = element_text(lineheight=.8, face="bold")
)

plot_final

prices <-  avocado %>% filter(region == 'Seattle' | region == 'TotalUS')
prices <- mutate(prices, type = ifelse(region == 'TotalUS', 'Average US', type))

plot_aux <- ggplot(prices, aes(x=Date, y=AveragePrice, colour = type)) + scale_y_continuous(labels = dollar)
plot_aux <- plot_aux + theme_economist_white() + scale_colour_economist() + geom_smooth(data = prices[prices$type == 'organic',]) +
  geom_smooth(data = prices[prices$type == 'conventional',]) + geom_smooth(data = prices[prices$type == 'Average US',])
plot_aux <- plot_aux + labs(x='',y = "Average price") + theme(axis.text.x = element_text(angle = 30), axis.title.y = element_text(vjust = 4))


plot_aux <- plot_aux + scale_x_date(breaks = date_breaks(width = '4 months'),
                                    labels = date_format("%b %y"))

vp1 <-viewport(width = 1, height = 1, x=0.5, y=0.5)

vp2 <-viewport(width = 0.6, height = 0.48, x = 0.69, y = 0.75)


print(plot_final, vp = vp1)
print(plot_aux, vp = vp2)

