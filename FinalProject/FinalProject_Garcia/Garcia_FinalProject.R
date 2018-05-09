library(rvest)
library(stringr)
library(moments)
library(stargazer)
library(ggplot2)
library(ggrepel)

website  <- read_html("https://ideas.repec.org/top/top.person.twitter.html")
website2 <- read_html("https://ideas.repec.org/top/top.person.nbcites.html")
website3 <- read_html("https://ideas.repec.org/top/top.person.hindex.html")

#------------------------------
# Extracting data from website1
#------------------------------
# Note: Contains twitter ranking, author name, IDEAS id, link to Citec profile, number of twitter followers, and twitter handles

# extracting rankings (based on twitter followers)
rank.twitter <- website %>% html_nodes("td:nth-child(2)") %>% html_text(trim = TRUE)
rank.twitter <- str_sub(rank.twitter, start = 0, end = -2) # to trim last "period" character

# extractig names
economist    <- website %>% html_nodes("#listing a")
author       <- economist %>% html_text(trim = TRUE)
author       <- author[author!=""] # to drop empty cells

# extracting ID
author.id    <- html_attr(economist, "name")
author.id    <- na.omit(author.id) # drop all NA values

# extract link
ideas.link   <- html_attr(economist, "href")
ideas.link   <- na.omit(ideas.link) # drop all NA values

# extracting number of twitter followers
t.followers  <- website %>% html_nodes("td~ td+ td") %>% html_text(trim = TRUE)

# building data frame
top25.twitter  <- cbind(rank.twitter, author, author.id, ideas.link, t.followers)
top25.twitter  <- as.data.frame(top25.twitter)
# change 'last name, name' format to 'name-last name' format
splits         <- str_split_fixed(top25.twitter$author, ",", 2)
top25.twitter$author      <- paste(splits[,2], splits[,1], sep = ' ')
# Complete ideas.link column
top25.twitter$ideas.link  <- paste("https://ideas.repec.org", top25.twitter$ideas.link, sep = "")

# extract Twitter handle
t.handle      <- 0 # create empty vector 
for (i in top25.twitter$ideas.link){
  t.handle[i] <- read_html(c(i)) %>% # run loop to go through each author profile in IDEAS
    html_node("tr:nth-child(10) td+ td a") %>%
    html_text(trim=TRUE)
}
t.handle      <- as.data.frame(t.handle)
t.handle      <- t.handle[-1,] # drop first empty row 
# Incoroporate twitter handle column into dataset
top25.twitter$t.handle  <- t.handle
# extract citec link
citec.link       <- 0
for (i in top25.twitter$ideas.link){
  citec.link[i]  <- html_attr((read_html(c(i)) %>%
                               html_node("#cites a:nth-child(3)")), "href")
}
citec.link       <- as.data.frame(citec.link)
citec.link       <- citec.link[-1,]
# incorporate Citec column into dataset
top25.twitter$citec.link <- citec.link # though we only have 182 citec links, and we are missing 172
head(top25.twitter)

#------------------------------
# Extracting data from website2
#------------------------------
# Note: Contains author name, IDEAS id, institution of affiliation, total number of citations 
# Extract from node
author1 <- website2 %>% html_nodes("td > a")

# extract author name
author  <- website2 %>% html_nodes("td > a") %>% html_text(trim=TRUE)
author  <- author[-c(1:21)] # get rid of first 21 rows
author  <- author[author!=""] # drop empty observations 

# Extract id
id      <- html_attr(author1, "name")
id      <- na.omit(id) # get rid of empty cells

# Extract total number of citations as of March 2018, score = citations
citations <- website2 %>% html_nodes("td~ td+ td") %>% html_text(trim=TRUE)
citations <- citations[-c(1:35)] # drop first 35 rows with irrelevant information

# Extract institution of affiliation
affiliation <- website2 %>% html_nodes("p") %>% html_text(trim=TRUE)
affiliation <- affiliation[-c(1:3)] # drop initial rows with no info
affiliation <- affiliation[-c(2635:2659)] # drop last rows with no info 

# build dataset
top5.citations <- cbind(author, id, citations, affiliation)
top5.citations <- as.data.frame(top5.citations)
head(top5.citations)
# clean affiliation info
splits2 <- str_split_fixed(top5.citations$affiliation, ",",10) #where 1)Department, 2)University, 3)City, 4)State(country)secondinstitution, 5)City of second institution, 6)state of second university 
# create new strings of variables out of splits2
top5.citations$department             <- splits2[,1]
top5.citations$university             <- splits2[,2]
top5.citations$city                   <- splits2[,3]
top5.citations$scnd.country.instition <- splits2[,4]
top5.citations$scnd.city              <- splits2[,5]
top5.citations$scnd.state             <- splits2[,6]
top5.citations$scnd.university        <- splits2[,7]
top5.citations$scnd.location          <- splits2[,8]
top5.citations$scnd.state.country     <- splits2[,9]
top5.citations$affiliation            <- NULL # drop initial column where we got splits from initially
#Note: consider droping second- institution/city/state/university/etc for simplicity. Also on coumn for city, clean all values that include 'University/School' 

#-------------------------------
# Extracting data from website 3
#-------------------------------
# Note: contains author, h index, rank

# extract ranking
h.ranking  <- website3 %>% 
  html_nodes("tr > :nth-child(1)") %>%
  html_text(trim=TRUE)
h.ranking  <- h.ranking[-c(1:5)] # drop first 6 rows with no relevant information
# extract id 
author     <- website3 %>%
  html_nodes("td > a") 
id         <- html_attr(author, "name")
id         <- na.omit(id) # drop all na values
# extract author name
h.author   <- website3 %>%
  html_nodes("td > a") %>%
  html_text(trim=TRUE)
h.author   <- h.author[h.author!=""] # drop empty cells
h.author   <- h.author[-c(1:21)] # delete first 21 rows
# extract h.index
h.index    <- website3 %>%
  html_nodes("td~ td+ td") %>%
  html_text(trim=TRUE)
h.index    <- h.index[-c(1:35)]
# create dataset
top5.hindex<- cbind(id, h.ranking, h.index, h.author)
head(top5.hindex)

#-------------------------------------------
# Merge all 3 tables to create final dataset
#-------------------------------------------
# Note: merge table on top5.index, top5.citations, and top25.twitter.

# start merge with top5.hindex and top5.citations, by id and author
# change name in top5.hindex for easier merge
top5.hindex  <- as.data.frame(top5.hindex)
colnames(top5.hindex)[which(names(top5.hindex)=="h.author")] <- "author"
# merge top5.hindex and top5.citations by id and author
merged.data1 <- merge(top5.hindex, top5.citations, by.x=c("id","author"), all = TRUE)

# merge merged.data1 with top25.twitter by id and author
# change name in top25.twitter for easier merge
colnames(top25.twitter)[which(names(top25.twitter)=="author.id")] <- "id"
# merge merged.data1 with top25.twitter by only
merged.data2<- merge(top25.twitter, merged.data1, by="id") # dropping all non-matching observations

# clean final data set for linear model 
merged.data2$author.y               <- NULL
merged.data2$scnd.country.instition <- NULL
merged.data2$scnd.city              <- NULL
merged.data2$scnd.state             <- NULL
merged.data2$scnd.university        <- NULL
merged.data2$scnd.location          <- NULL
merged.data2$scnd.state.country     <- NULL
final.dataset                       <- merged.data2
str(merged.data2)
# by looking at the structure of our dataset, we see that most variables are in the factor form, so we convert them into the numeric form
final.dataset$t.followers           <- as.numeric(as.character(final.dataset$t.followers))
final.dataset$h.index               <- as.numeric(as.character(final.dataset$h.index))
final.dataset$citations             <- as.numeric(as.character(final.dataset$citations))
# create new logarithmic variables for our econometric model
final.dataset$ln.followers          <- log(final.dataset$t.followers)
final.dataset$ln.citations          <- log(final.dataset$citations)
attach(final.dataset)
head(final.dataset)
# order dataset 
final.dataset            <- final.dataset[c("id", "author.x", "ideas.link", "t.handle", "citec.link","department", "university", "city", "rank.twitter", "t.followers", "ln.followers", "citations", "ln.citations", "h.ranking", "h.index")]
View(final.dataset)  
# no data on citec.link was left after merge, so we drop column
final.dataset$citec.link <- NULL

#-------------------------------
# Compute descriptive statistics
#-------------------------------
descriptive            <- cbind(final.dataset$t.followers, final.dataset$citations, final.dataset$h.index)
# assign column names
colnames(descriptive)  <- c("t.followers", "citations", "h.index")
summary(descriptive)
nrow(descriptive)
# calculate standard deviations
descriptive <- as.data.frame(descriptive)
t.followers <- as.numeric(descriptive$t.followers)
citations   <- as.numeric(descriptive$citations)
h.index     <- as.numeric(descriptive$h.index)

sd(descriptive$t.followers) # s.d. for twitter followers
skewness(descriptive$t.followers)
kurtosis(descriptive$t.followers)

sd(descriptive$citations, na.rm = TRUE) # s.d. for citations
skewness(descriptive$citation, na.rm = TRUE)
kurtosis(descriptive$citation, na.rm = TRUE)

sd(descriptive$h.index, na.rm = TRUE) # s.d. for h.index
skewness(descriptive$h.index, na.rm = TRUE)
kurtosis(descriptive$h.index, na.rm = TRUE)

#-----------------------
# create plotting graph
#----------------------

pointslabel <- c("@paulkrugman", " @LHSummers" ," @amspence98","@mfratzscher"," @tnannicini", "@gregmankiwblog", "@scottirwinui", "@robertjshiller", " @krogoff", " @JeanTirole","@pikettylemonde", "@raffasadun", "@r_thaler", " @JeanTirole", "@raffasadun", "@DrDaronAcemoglu", "@heckmanequation")
pointslabel

p1          <- ggplot(final.dataset,aes(y = ln.followers, x = ln.citations)) +
               geom_point(aes(color=h.index)) +
                     scale_x_continuous(name = "Percentage Change Citations", breaks = 1:10) +
                     scale_y_continuous(name = "Percentage Change Twitter Followers", breaks = 1:15) +
                     scale_color_continuous(name = "H-Index") +
               geom_text_repel(aes(label=t.handle),
                     color = "grey20",
                     data = subset(final.dataset, final.dataset$t.handle %in% pointslabel),
                     force = 10) +
               ggtitle("Twitter Followers and Citations Among Economists")
p1
#------------------------
# Compute regression
#---------------------
results.regression <- lm(ln.followers ~ ln.citations + h.index, data = final.dataset)
summary(results.regression)