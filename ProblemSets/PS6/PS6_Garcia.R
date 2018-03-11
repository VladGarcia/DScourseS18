library(rvest)
url<- "https://en.wikipedia.org/wiki/All-time_Olympic_Games_medal_table"

# Extract html table using rvest
# Get xpath file from inspecting website 

Olympics_count <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table()
Olympics_count <- Olympics_count[[1]]

data.frame(Olympics_count)
str(Olympics_count)

write.csv(Olympics_count, "olympics_count.csv")

df<- read.csv("Olympics_count.csv", header = T, sep = ",")

Summer.Games<- data.frame(USA=c(705,795,1022),URS=c(296,319,395) ,GBR=c(291,295,263),FRA=c(263,241,212),GER=c(230,194,191))
row.names(Summer.Games)<- c("Bronze","Silver","Gold")
barplot(as.matrix(Summer.Games),
        legend.text = row.names(Summer.Games),
        args.legend = list(x="right"),
        col=c("darkorange4","darkgrey","gold"))

Winter.Games<- data.frame(NOR=c(111,125,135),USA=c(90,110,105),GER=c(60,88,92) ,AUT=c(87,81,64) ,CAN=c(62,64,73)
  )
row.names(Winter.Games)<- c("Bronze","Silver","Gold")
barplot(as.matrix(Winter.Games),
        legend.text = row.names(Winter.Games),
        args.legend = list(x="right"),
        col=c("darkorange4","darkgrey","gold"))

Total.Games<- data.frame(USA=c(795,905,1127) ,URS=c(355,376,473) ,GBR=c(307,299,274) ,GER=c(290,282,283) ,FRA=c(316,276,248))
row.names(Total.Games)<- c("Bronze","Silver","Gold")
barplot(as.matrix(Total.Games),
        legend.text = row.names(Total.Games),
        args.legend = list(x="right"),
        col=c("darkorange4","darkgrey","gold"))
