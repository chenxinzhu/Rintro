getwd()
getwd()
setwd('/Users/chenx/scw_2018/intro_R/')
getwd()

i <- 2L
j <- 2
class(j)
typeof(j)
typeof(i)
k <- 1+4i
logical_vector <- c(TRUE, TRUE, FALSE, TRUE)
class(logical_vector)
char_vector <- c('abc','def','gg')
class(char_vector)
length(char_vector)
anyNA(char_vector)
mixed <- c('TRUE',TRUE)
class(mixed)
anothermix <- c("STANFORD", 3)
class(anothermix)
anothermix <- c("STANFORD", 2L)
class(anothermix)
anothermix <- c(2L, 3)
class(anothermix)
anothermix <- c(TRUE, 3)
class(anothermix)
anothermix <- c(TRUE, 2L)
class(anothermix)
anothermix <- c(TRUE)
class(anothermix)
anothermix <- c(TRUE, 4+2i)
class(anothermix)
anothermix <- c('an',4+2i)
class(anothermix)
anothermix <- c(2L,4+2i)
class(anothermix)
mylist <- list(chars = 'coffee', nums = c(1, 4, 5), logicals = FALSE, anotherlist = list(a = 'a', b=2))
mylist
char_vector[2]
mylist[2]
str(mylist)
mylist[3]
mylist$logicals
m <- matrix(nrow = 2, ncol = 3)
class(m)
m
m <- matrix(data = 1:6, nrow = 2, ncol = 3)

m <- matrix(data = 1:6, nrow = 2, ncol = 3, byrow = TRUE)
m
df <- data.frame(id = letters[1:10], x=1:10, y=11:20)
df
str(df)
class(df)
typeof(df)
head(df)
tail(df)
dim
names(df)
summary(df)
state<-factor(c('Arizona','California','Mass'))
state
state <- factor(c('AZ','CA','CA'))
state


survery <- data.frame(number = c(1,2,2,1,2), group = c('A', 'B','C', 'D','A'))
str(survery)


getwd()
gapminder <- read.csv("gapminder-FiveYearData.csv")
gapminder
dim(gapminder)
head(gapminder)
str(gapminder)
View(gapminder)
gapminder$continent

gapminder [10:15, c('lifeExp','gdpPercap')]
library(dplyr)
select(gapminder, lifeExp, gdpPercap)
gapminder%>%select(lifeExp, gdpPercap)

gapminder%>% filter(lifeExp>71)
Mexico <- gapminder%>%
       select(year, country, gdpPercap)%>%
       filter(country == 'Mexico')
str(Mexico)

gapminder %>% group_by(country) %>% tally()
gapminder %>% group_by(country) %>% summarise(avg = mean(pop), std = sd (pop))
gapminder %>% group_by(country) %>% summarise(avg = mean(pop), std = sd (pop), total = n()) %>% 
              arrange(desc(avg))

gapminder_mod <- gapminder
gapminder_mod <- gapminder_mod %>% mutate(new = pop*gdpPercap)
gapminder_mod %>% group_by(country) %>% summarise(avg = mean(lifeExp)) %>% 
              arrange(avg) %>% 
              arrange(desc(avg))
gapminder_mod %>% group_by(country) %>% summarise(avg = mean(lifeExp)) %>% 
              filter(avg == max(avg) | avg == min(avg))
plot(x = gapminder_mod$gdpPercap, y = gapminder_mod$lifeExp)

install.packages("ggplot2")
library(ggplot2)
#
ggplot(gapminder, aes(x = gdpPercap, y= lifeExp )) + 
  geom_point()
#log10
ggplot(gapminder, aes(x = log10(gdpPercap), y= lifeExp )) + 
  geom_point()
#transperancy
ggplot(gapminder, aes(x = log10(gdpPercap), y= lifeExp )) + 
  geom_point(alpha = 1/3, size =3)
#color
p<- ggplot(gapminder, aes(x = log10(gdpPercap), y= lifeExp, color = continent)) + 
  geom_point()
p+ facet_wrap(~ continent)
p<- p+ facet_wrap(~ continent)
p2 <- p+ geom_smooth(color = 'yellow')
p2

#combine dplyr with ggplot2
names(gapminder)
gapminder %>% select(pop)
gapminder %>% mutate(gdp = pop*gdpPercap) %>% ggplot(aes(gdp, lifeExp))+geom_point()

#histogram
p3 <- ggplot(gapminder_mod, aes(lifeExp, fill = continent))+geom_histogram(binwidth = 1)+
  ggtitle("Histogram")
p3

#saving plots
ggsave(p3, file="~/scw_2018/advanced_R/histagram_lifeExp.png")


  #line plots
gapminder_mod %>% filter(country == 'Afghanistan') %>%
ggplot(aes(x = year, y = lifeExp))+
  geom_line(color = 'blue')

#excise2

  p5 <- ggplot(gapminder, aes(x=lifeExp, y=year, color=continent))+
  geom_point()+facet_wrap(~ continent)+
    geom_smooth(color = 'orange', lwd=2, se=FALSE)
  p6 <- p5+geom_smooth(color = 'orange', lwd=2, se=FALSE, method = 'lm')
  p6
  ggsave(p6, file = 'geom_smooth_type.png')
  
  #density plot
  ggplot(gapminder, aes(gdpPercap, lifeExp))+
    geom_point(size=0.25)+
    geom_density_2d() + scale_x_log10()
  #combine plots
  install.packages('gridExtra')
library(gridExtra)  
gridExtra::grid.arrange(
  p5 <- ggplot(gapminder, aes(x=lifeExp, y=year, color=continent))+
    geom_point()+facet_wrap(~ continent)+
    geom_smooth(color = 'orange', lwd=2, se=FALSE),
  p7 <- ggplot(gapminder, aes(gdpPercap, lifeExp))+
    geom_point(size=0.25)+
    geom_density_2d() + scale_x_log10())
)

#loops

gapminder_mod %>% filter(continent == "Asia") %>% 
  summarise(avg= mean(lifeExp))
contin <- unique(gapminder_mod$continent)
contin

for (c in contin) {
  res <- gapminder_mod %>% filter(continent == c) %>% 
    summarise(avg= mean(lifeExp))
  print(res)
}

for (c in contin) {
  res <- gapminder_mod %>% filter(continent == c) %>% 
    summarise(avg= mean(lifeExp))
  print(paste0("The avg life expectancy of", c, "is:", res))
}

gapminder_mod %>% group_by(continent,year) %>% 
  summarise(avg= mean(lifeExp))
#

for (c in contin) {
  for (y in unique(gapminder_mod$year))
  res <- gapminder_mod %>% filter(continent == c) %>% 
    summarise(avg= mean(lifeExp))
  print(paste0("The avg life expectancy of", c, "is:", res))
}

gapminder_mod %>% group_by(continent,year) %>% 
  summarise(avg= mean(lifeExp))

#functions
mean(2,3)
adder <- function(x, y){
  return (x+y)
}
adder(2,3)

adder <- function(x, y){
  print (paste0("The sum of ", x, " and ",  y,  " is ", x+y))
}
adder(2,3)


#This is the end of the workshop
