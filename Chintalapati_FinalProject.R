#installing packages for analysis
install.packages("tidyverse")
library("tidyverse")
install.packages("dplyr")
library("dplyr")
install.packages("scales")
library("scales")
install.packages("ggplot2")
library("ggplot2")
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages("viridis")
library(viridis)
install.packages("kableExtra")
library(kableExtra)
options(knitr.table.format = "html")
install.packages("streamgraph")
library(streamgraph)
install.packages("DT")
library(DT)
install.packages("plotly")
library(plotly)
install.packages("lubridate")
library(lubridate)

#2.Import the date and name the table <ebola>
setwd("C:/Users/admin/Downloads")
ebola <- read.csv("ebola.csv",header=TRUE, sep=",")
options(max.print=999999)
ebola
#3.Display the head, tail and structure of <bio>
head(ebola)
tail(ebola)
str(ebola)
view(ebola)
colnames(ebola) <- c("Country","Date","suspected_cases","probable_cases","confirmed_cases","Total_cases","suspected_deaths","probable_deaths","confirmed_deaths","Total_deaths")
ebola
attach(ebola)
ebola$Date <- as.Date(ebola$Date)

#Descriptive statistics of the data
summary(ebola)
str(ebola)
unique(ebola$Country)
unique(ebola$Date)
options(scipen = 999)

#Graph 1:how countries are infected with ebola virus(cases)
ggplot(ebola, aes(x=Country, y=Total_cases)) + 
  labs(x="Country",y="Total Cases")+
  geom_bar(stat = "identity",width=0.5)+
  #scale_y_continuous(trans="log2")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Graph 2 :how countries are infected with ebola virus(deaths)
ggplot(ebola, aes(x=Country, y=Total_deaths)) + 
  geom_bar(stat = "identity",width=0.5)+
  labs(x="Country",y="Total Deaths")+
  #scale_y_continuous(trans="log2")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

Graph1<- ggplot(ebola, aes(x=Country, y=Total_deaths/100)) + 
  geom_bar(stat = "identity",width=0.5)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# another graph
p1 <- ggplot(ebolatotalcasescountry, aes(x=Country, y=Total_cases, group = 1)) +
  geom_line(linetype = "solid") +
  ggtitle("Total Infected People") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 <- ggplot(ebola, aes(x=Country, y=Total_deaths)) +
  geom_line(color="grey",size=2) +
  ggtitle("Total Deaths") +
  theme_ipsum()+theme(axis.text.x = element_text(angle = 90))
p2 + theme(axis.text.x = element_text(angle = 90))
p1+theme(axis.text.x = element_text(angle = 90))+p2+ theme(axis.text.x = element_text(angle = 90))
p1+p2

#Graph 3 



ebolatotalcases <- ebola %>%
  group_by("year"=year(ebola$Date), Country) %>%
  summarise(Total_cases = sum(Total_cases, na.rm = TRUE), Total_deaths=sum(Total_deaths,na.rm=TRUE))


ebolatotalcasescountry <- ebola %>%
  group_by( Country) %>%
  summarise(Total_cases = sum(Total_cases, na.rm = TRUE), Total_deaths=sum(Total_deaths,na.rm=TRUE))


#graph 3
ggplot(ebolatotalcases, aes(x=factor(year), y=Total_cases, group=Country, color=Country, group = 1)) +
  geom_line(linetype = "solid") +
  ggtitle("Total Infected People") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ebolatotalcases %>%
  ggplot( aes(x=factor(year), y=Total_cases, group=Country, color=Country), group=3) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Total number of ebola cases") +
  theme_ipsum()

#Graph 2: Total ebola cases in all countries
ebolatotalcases %>%
  ggplot( aes(x=factor(year), y=Total_cases, group=Country, color=Country), group=1) +
  geom_line(size=1) +
  labs(x="Year",y="Total Cases")+
  scale_y_continuous(trans = "log2")+
  geom_point(size=2.5)+
  theme(
      plot.title = element_text(size=14,hjust = 0.5)
  ) +
  ggtitle("Total number of Ebola cases") 
+ theme_ipsum()

#Graph 3:Total ebola deaths in all countries
ebolatotalcases %>%
  ggplot( aes(x=factor(year), y=Total_deaths, group=Country, color=Country), group=1) +
  geom_line(size=1) +
  labs(x="Year",y="Total Deaths")+
  scale_y_continuous(trans = "log2")+
  geom_point(size=2.5)+
  theme(
    plot.title = element_text(size=14,hjust = 0.5)
  ) +
  ggtitle("Total number of Ebola Deaths") 
+
  theme_ipsum()


#Graph 4: scatter plot of cases vs deaths in all countries
ebola %>%
  ggplot( aes(x=Total_cases, y=Total_deaths,color=Country)) +
  geom_point() +
  labs(x="Total Cases",y="Total Deaths")+
  theme(
    plot.title = element_text(size=14,hjust = 0.5)
  ) +
  ggtitle("Total number of Ebola Cases vs Deaths")
+  theme_ipsum()

#segregated ebola data year wise
ebola$year <- year(ebola$Date)
x<- ebola %>% 
  select(year ,Total_cases) 
#
x %>%
  ggplot( aes(y=Total_cases, color=year)) +
  geom_boxplot() +
  labs(x="Year",y="Total Deaths")+
  theme(
    plot.title = element_text(size=14)
  ) +
  ggtitle("Total number of ebola Deaths") +
  theme_ipsum()

#Creating table for Guinea
Guineacases <- ebola %>%
  filter(Country=="Guinea") %>%
  group_by(Date,Country) %>%
  summarise(Total_cases = sum(Total_cases, na.rm = TRUE), Total_deaths=sum(Total_deaths,na.rm=TRUE))

#Creating table for Liberia
Liberiacases <- ebola %>%
  filter(Country=="Liberia") %>%
  group_by(Date,Country) %>%
  summarise(Total_cases = sum(Total_cases, na.rm = TRUE), Total_deaths=sum(Total_deaths,na.rm=TRUE))

#Creating table for Sierra
SierraLeonecases <- ebola %>%
  filter(Country=="Sierra Leone") %>%
  group_by(Date,Country) %>%
  summarise(Total_cases = sum(Total_cases, na.rm = TRUE), Total_deaths=sum(Total_deaths,na.rm=TRUE))

#Line graph for Guinea
  Guineacases %>%
  ggplot( aes(x=Date))+
    geom_line(aes(y=Total_cases,color ="Total_cases"),size=1.5) + 
    geom_line(aes(y=Total_deaths,color="Total_deaths"),size=1.5) +
  labs(x="Date",y="Count")+
 #scale_y_continuous(trans = "log2")+
 # geom_point(size=2.5)+
  theme(
    plot.title = element_text(size=14,hjust = 0.5)
  ) +
  ggtitle("Ebola outbreak in Guinea")    
  + theme_ipsum()
  
  #Line graph for Liberia
  Liberiacases %>%
    ggplot( aes(x=Date))+
    geom_line(aes(y=Total_cases,color ="Total_cases"),size=1.5) + 
    geom_line(aes(y=Total_deaths,color="Total_deaths"),size=1.5) +
    labs(x="Date",y="Count")+
    #scale_y_continuous(trans = "log2")+
    # geom_point(size=2.5)+
    theme(
      plot.title = element_text(size=14,hjust = 0.5)
    ) +
    ggtitle("Ebola outbreak in Liberia")    
  + theme_ipsum()
  
  #Line graph for SierraLeone
  SierraLeonecases %>%
    ggplot( aes(x=Date))+
    geom_line(aes(y=Total_cases,color ="Total_cases"),size=1.5) + 
    geom_line(aes(y=Total_deaths,color="Total_deaths"),size=1.5) +
    labs(x="Date",y="Count")+
    #scale_y_continuous(trans = "log2")+
    # geom_point(size=2.5)+
    theme(
      plot.title = element_text(size=14,hjust = 0.5)
    ) +
    ggtitle("Ebola outbreak in Sierra Leone")    
  + theme_ipsum()
  
summary(Guineacases)
summary(Liberiacases)
summary(SierraLeonecases)
  