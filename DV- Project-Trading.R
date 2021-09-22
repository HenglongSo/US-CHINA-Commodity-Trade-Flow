#
# Author: Henglong So
# Purpose
#Purpose: Final Project
library(dplyr)
library(data.table)
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(base)
library(stringr)
library(tidyr)
library(lubridate)
library(rworldmap)
library(RColorBrewer)
library(plotly)
options(scipen = 9999, warn = -1, digits= 5)


trade <-
  read.csv("/Users/henglong/Documents/IST 719 /project/commodity_trade_statistics_data.csv", sep = ",", header = TRUE)
#View(trade)
head(trade)
str(trade)
#check range of year
range(trade$year)
#range(trade$trade_usd)
#converting weight to smaller digit
weight <-trade%>%group_by(country_or_area)%>%summarise(USD=round(sum(trade_usd)/1000000000))
weight
sum <-sum(weight$USD)
sum
#barplot for world trade
barplot(weight$USD, col = rainbow(3, s = 0.5), main = "Trade in USD for each country from 1988 to 2016", xlab = " USD Trade Transaction", ylab = "In USD Billions")
#plot the map for world country commodity trade
View(weight)
#matching countries
matched <- joinCountryData2Map(weight, joinCode="NAME", nameJoinColumn="country_or_area")
#plot data for world trade 
mapCountryData(matched, nameColumnToPlot="USD", mapTitle="Trade in USD for Each Country from 1988 to 2016", numCats=20, catMethod="fixedWidth", colourPalette=brewer.pal(9, "Purples"),addMapLegend)

#top 20 countries trade in the world
top_20<- weight[with(weight, order(-USD)), ]%>%head(20)
top_20
#ploting barplot for top 20 trade
ggplot(data=top_20, aes(x=country_or_area, y=USD,color=USD)) +
  geom_bar(stat="identity", fill="steelblue")+coord_flip()+ggtitle("Top 20 Countries Trade in The World") #,coord_flip())

#country
trade1 <- filter(trade, trade$country_or_area == "China" | trade$country_or_area == "China, Hong Kong SAR" | trade$country_or_area =="USA")

# year >2001
trade1 <- filter(trade1, trade1$year > 2001)

trade2 <- filter(trade1, trade1$flow=="Export" | trade1$flow=="Import")
trade2$country <- ifelse(trade2$country_or_area == "China" | trade2$country_or_area =="China, Hong Kong SAR", "china" , "usa")
## china export

chinaex <- filter (trade2, trade2$country=="china"  & trade2$flow== "Export")

# top 5 chinese export 
chinaex1 <- group_by(chinaex, category)
chinaex2 <- dplyr::summarise(chinaex1, sumusd=sum(trade_usd))

chinaex2 <- chinaex2[order(-chinaex2$sumusd),]
chinaex3 <- chinaex2[c(2:7),]

# top 5 chinese import 
chinaim <- filter (trade2, trade2$country=="china"  & trade2$flow== "Import")
chinaim1 <- group_by(chinaim, category)
chinaim2 <- dplyr::summarise(chinaim1, sumusd=sum(trade_usd))

chinaim2 <- chinaim2[order(-chinaim2$sumusd),]
chinaim3 <- chinaim2[c(2:7),]

ggplot(data=chinaex3, aes(x=reorder(category,sumusd), y=sumusd))+geom_bar(stat="identity", aes(fill=category))+theme(axis.text.x = element_text(angle = 60, hjust = 1))+coord_flip() +ggtitle("Top 6 Category for Chinese Export") + xlab("Category") +ylab("USD value")+theme(legend.position = "none")
#Category for Chinese Import
ggplot(data=chinaim3, aes(x=reorder(category,sumusd), y=sumusd))+geom_bar(stat="identity", aes(fill=category))+theme(axis.text.x = element_text(angle = 60, hjust = 1))+coord_flip() +ggtitle("Top 6 Category for Chinese Import") + xlab("Category") +ylab("USD value")+theme(legend.position = "none")

## Overall review of world tariff lists

overall <- filter(trade2, trade2$category=="all_commodities" )
overall1 <- group_by(overall, flow, country, year)
overall2 <- dplyr::summarise(overall1, sumusd=sum(trade_usd))

trade$gc<- ifelse (grepl("Swine", trade$commodity,  ignore.case=TRUE)=="TRUE","animal product",
                   ifelse(grepl("Aircraft", trade$commodity, ignore.case=TRUE)=="TRUE","aircraft product",
                          ifelse(grepl("electric", trade$commodity, ignore.case=TRUE)=="TRUE", "Electric product",
                                 ifelse(grepl("steel", trade$commodity, ignore.case=TRUE)=="TRUE", "Steel Product",
                                        ifelse(grepl("medical", trade$commodity, ignore.case=TRUE)=="TRUE", "Medical Product",
                                               ifelse(grepl("cotton", trade$commodity, ignore.case=TRUE)=="TRUE","Cotton Product",
                                                      ifelse(grepl("aluminium", trade$commodity, ignore.case=TRUE)=="TRUE","Aluminium Product",
                                                             ifelse(grepl("cars", trade$commodity, ignore.case=TRUE)=="TRUE","Auto Product",
                                                                    ifelse(grepl("automobiles", trade$commodity, ignore.case=TRUE)=="TRUE","Auto Product",
                                                                           ifelse(grepl("vehicles", trade$commodity, ignore.case=TRUE)=="TRUE","Auto Product", "Other"))))))))))

atrade<- filter(trade, trade$gc != "Other")

atrade$country <- ifelse(atrade$country_or_area == "China" | atrade$country_or_area =="China, Hong Kong SAR", "china" , atrade$country_or_area)

atrade1 <- filter(atrade, (flow=="Import" | flow=="Export" ) & year >2001)

atrade2 <- group_by(atrade1, flow, year)
atrade3 <- dplyr::summarise(atrade2, sumusd=sum(trade_usd))

aim <- filter(atrade3 , flow=="Import")
aex <-filter(atrade3, flow=="Export")
colnames(aex)<-c("f", "year", "usd")

aall <- merge(aim, aex, x.by=year)



# USA + China percentage
ausa <- filter(atrade1, atrade1$country_or_area=="USA")
plot(ausa)
ausa2 <- group_by(ausa, flow, year)
ausa3 <- dplyr::summarise(ausa2, sumusd1=sum(trade_usd))
acn <- filter(atrade1, atrade1$country=="china")
acn2 <- group_by(acn, flow, year)
acn3 <- dplyr::summarise(acn2, cusd=sum(trade_usd))

aall$usai<- as.numeric(unlist(ausa3[c(16:30), c(3)]))
aall$usae<- as.numeric(unlist(ausa3[c(1:15), c(3)]))
aall$cni<- as.numeric(unlist(acn3[c(16:30), c(3)]))
aall$cne <- as.numeric(unlist(acn3[c(16:30), c(3)]))

#china & usa percentage over the world
aall$usacni <- aall$usai+aall$cni
aall$usacne <- aall$usae+aall$cne
aall$usacnipct <-  aall$usacni/aall$sumusd
aall$usacnepct <-  aall$usacne/aall$usd
aall$usaipct <-  aall$usai/aall$sumusd
aall$usaepct <-  aall$usae/aall$usd
aall$cnipct <-  aall$cni/aall$sumusd
aall$cnepct <-  aall$cne/aall$usd


# Line plot for overall tariff lists
p <- plot_ly(aall , x=~year, y=~usd, name= "Import", type="scatter",  mode="lines") %>%
  add_trace(y= ~sumusd , name="Export" , mode="lines+markers") %>%
  add_trace(y= ~usacni , name="China and USA Export" , mode="lines+markers") %>%
  add_trace(y= ~usacne , name="China and USA Import" , mode="lines+markers") %>%
  layout (  title= "Tariff Commodity Trade History in the World ")
p

P9 <- ggplot(data=aall , x=~year, y=~usd, name= "Import", type="scatter",  mode="lines")
P9
#China and USA percentage 
Ch_us <- as.data.frame(aall)
p1 <- plot_ly(aall , x=~year, y=~usacnipct, tittle= "China & USA Export %", type="scatter",  mode="lines") %>%
  #add_trace(y= ~usacnepct , name="China and USA Import %" , mode="lines+markers") %>%
  add_trace(y= ~usaepct , name="USA Export %" , mode="lines+markers") %>%
  add_trace(y= ~usaipct , name="USA Import %" , mode="lines+markers") %>%
  add_trace(y= ~cnepct , name="China Export %" , mode="lines+markers") %>%
  add_trace(y= ~cnipct , name="China Import %" , mode="lines+markers") %>%
  layout (  title= "China and USA percent in the Tariff Commodity Trade History")%>%(type="scatter")
p1

ggplot(data = aall, aes(x = year, y = usd/1000000000)) + geom_line()%>%
  add_trace(y= ~usaepct , name="USA Export %" , mode="lines+markers") %>%
  add_trace(y= ~usaipct , name="USA Import %" , mode="lines+markers") %>%
  add_trace(y= ~cnepct , name="China Export %" , mode="lines+markers") %>%
  add_trace(y= ~cnipct , name="China Import %" , mode="lines+markers")%>%
ggplot(data = aall, aes(x = year, y = usd/1000000000))#+ geom_line( y= usaepct, tittle = "USA Export %")
ggplot(data = aall, aes(x = year, y = usd)) +
  geom_line(aes(y = usaepct, colour = "var0"))+
  geom_line(aes(y = usaipct, colour = "var1"))+
  geom_line(aes(y = cnepct, colour = "var2"))+
  geom_line(aes(y = cnipct, colour = "var3"))
#

ggplot(data=aall, aes(x=reorder(usaepct,usaipct,cnepct,cnipct), y=sumusd))+geom_bar(stat="identity", aes(fill=category))+theme(axis.text.x = element_text(angle = 60, hjust = 1))+coord_flip() +ggtitle("Top 6 Category for Chinese Import") + xlab("Category") +ylab("USD value")+theme(legend.position = "none")
#

trade2$gc<- ifelse (grepl("Swine", trade2$commodity,  ignore.case=TRUE)=="TRUE","animal product",
                    ifelse(grepl("Aircraft", trade2$commodity, ignore.case=TRUE)=="TRUE","aircraft product",
                           ifelse(grepl("electric", trade2$commodity, ignore.case=TRUE)=="TRUE", "Electric product",
                                  ifelse(grepl("steel", trade2$commodity, ignore.case=TRUE)=="TRUE", "Steel Product",
                                         ifelse(grepl("medical", trade2$commodity, ignore.case=TRUE)=="TRUE", "Medical Product",
                                                ifelse(grepl("cotton", trade2$commodity, ignore.case=TRUE)=="TRUE","Cotton Product",
                                                       ifelse(grepl("aluminium", trade2$commodity, ignore.case=TRUE)=="TRUE","Aluminium Product",
                                                              ifelse(grepl("cars", trade2$commodity, ignore.case=TRUE)=="TRUE","Auto Product",
                                                                     ifelse(grepl("automobiles", trade2$commodity, ignore.case=TRUE)=="TRUE","Auto Product",
                                                                            ifelse(grepl("vehicles", trade2$commodity, ignore.case=TRUE)=="TRUE","Auto Product", "Other"))))))))))

war <- filter(trade2, ( country== "china" | country=="usa" ) & gc != "Other")



# usa import
warusaim <- filter(war, war$country=="usa"  & war$flow== "Import")
warusaim1 <- group_by(warusaim, gc , year)
warusaim2 <- dplyr::summarise(warusaim1, sumusd=sum(trade_usd), tax=sum(trade_usd)*1.25)


mcp <- plot_ly(warusaim2 , x= ~ year , y=~sumusd , type="scatter", mode="markers",
               color=~gc, colors="Paired", sizes=c(min(warusaim2$sumusd),max(warusaim2$sumusd)),
               marker = list(opacity = 1.2, sizemode = 'diameter',size = 15),
               text= ~paste( year, tax, gc)) %>% layout(title="USA Import Value by year")
mcp

ggplot( data = warusaim2, aes(x = year, y = sumusd))+ geom_point(color = gc)+scale_size_continuous()
ggplot(data = warusaim2)+geom_point(aes(y = sumusd, x= year, colours = "Paired"))












warusaim2$pct <- warusaim2$sumusd*100/30572595934
usaim <- filter(warusaim2, year >2015)
usaim

#china 
#china import 

warusaimc <- filter(war, war$country=="china"  & war$flow== "Import")

warusaim1c <- group_by(warusaimc, gc , year)
warusaim2c <- dplyr::summarise(warusaim1c, sumusd=sum(trade_usd), tax=sum(trade_usd)*1.25)



mcpc <- plot_ly(warusaim2c , x= ~ year , y=~sumusd , type="scatter", mode="markers",
                color=~gc, colors="Paired", sizes=c(min(warusaim2$sumusd),max(warusaim2$sumusd)),
                marker = list(opacity = 2, sizemode = 'diameter', size = 15),
                text= ~paste( year, tax, gc)) %>% layout(title="China Import Value by year")
mcpc
export( mcpc, file = "mcpc.pdf")




cim<- filter(warusaim2c, year >2015)
cim$pct <- cim$sumusd*100/123012246718
cim

#Tariff Policy Impact 

#summary of all commodity value
itrade <- filter (trade, trade$category=="all_commodities" & year>2001 & (flow=="Import" | flow=="Export"))
itrade1 <- group_by(itrade, flow , year)
itrade2 <- dplyr::summarise(itrade1, allusd=sum(trade_usd), tax=sum(trade_usd)*1.25)


aall$alle<- as.numeric(unlist(itrade2[c(1:15), c(3)]))
aall$alli <- as.numeric(unlist(itrade2[c(16:30), c(3)]))

aall$allepct <- aall$sumusd*100/aall$alli
aall$allipct <-aall$usd*100/aall$alle


p4 <- plot_ly(aall , x=~year, y=~allepct, name= "Tariff Goods Export proportion %", type="scatter",  mode="lines") %>%
  add_trace(y= ~allipct , name="Tariff Goods Import proportion %" , mode="lines+markers") %>%
  layout (  title= "Tariff Goods proportion in Export and Import")
p4

