# Scholarly Citations and Twitter: An Insight into the Case of Economists
Final data science project for Econ-5970 of the University of Oklahoma, Norman Campus. This project aims to scrape data from [RePec](https://ideas.repec.org/), which is a database containing information on registered economists, their publications, institution of affiliation, and contributions. I use R Studio Version 1.0.153 for all web scraping and coding practices, as well as the [SelectorGadget](http://selectorgadget.com/) which helps click CSS selectors from static websites and allows you to extract and parse data as text. The extension can be easily downloaded and installed for free. 

## Getting Started
In order to replicate the results, the following packages need to called and/or installed in R Studio:
```
library(rvest)
library(stringr)
library(moments)
library(stargazer)
library(ggplot2)
library(ggrepel)
```
All the information was extracted from [IDEAS](https://ideas.repec.org/), however, three main web pages within this website were used for this project: 1) '[Top 25% Economists by Twitter Followers]'(https://ideas.repec.org/top/top.person.twitter.html), 2) '[Top 5% Authors, Number of Citations, as of MArch 2018]'(https://ideas.repec.org/top/top.person.nbcites.html), and 3) '[Top 5% Authors, h-index, as of March 2018]'(https://ideas.repec.org/top/top.person.hindex.html); which we will call 'website,' 'website2,' and 'website3.' We read the html information from each of these links by using the function 'read_html; from the rvest package. 
```
website  <- read_html("https://ideas.repec.org/top/top.person.twitter.html")
website2 <- read_html("https://ideas.repec.org/top/top.person.nbcites.html")
website3 <- read_html("https://ideas.repec.org/top/top.person.hindex.html")
```

### Extracting data from website1
This data set will contain information on Twitter ranking, author name, the IDEAS id (which is the personal link to each author's profile within the IDEAS data base, link to the authors' Citec profile (which has more extensive information on on the academic activity of the author), number of twitter followers, and twitter handles.

First, we will extract the ranking information based on the number of twitter followers, which is done by the first line of code. With the help of the SelectorGadget, we click on the desired elements we want to scrape, and discover that the CSS selector for that element is "td:nth-child(2)". The pipe operator makes the scraping of data easier, as we can tell R to directly trim the text related to our CSS selector from a website. The returned information will not be clean, in fact each ranking as a period at the end. We will trim the this last character with the second line of code.  
```
rank.twitter <- website %>% html_nodes("td:nth-child(2)") %>% html_text(trim = TRUE)
rank.twitter <- str_sub(rank.twitter, start = 0, end = -2) # to trim last "period" character
```
We proceed to extract the authors/economists' names and last names, and again, the CSS selector that contains this information will be "#listing a". We do not trim the text related to this node directly, as we did in the previous example, because this node contains far more information that we will be using, so we store it in a different vector that we will call 'economist.' From this vector, we do trim the text, store it in a vector named 'author' and clean all the empty observations:
```
economist    <- website %>% html_nodes("#listing a")
author       <- economist %>% html_text(trim = TRUE)
author       <- author[author!=""] # to drop empty cells
```
To extract the IDEAS ID, we will run the following code and omit all NA values in order to clean our data. This information is not embedded as  text, but as an attribute. Because of this reason, we will be using the function 'html_attr' from the Rvest package:
```
author.id    <- html_attr(economist, "name")
author.id    <- na.omit(author.id) # drop all NA values
```
Finally, to the number of Twitter followers, we can fortunately do it with one line of code. This information did not need to be cleaned:
```
t.followers <- website %>% html_nodes("td~ td+ td") %>% html_text(trim = TRUE)
```
We finally bind all strings of information to build a data frame by running the following code:
```
top25.twitter <- cbind(rank.twitter, author, author.id, ideas.link, t.followers)
top25.twitter <- as.data.frame(top25.twitter)
```
To edit and make our data set look a bit nicer, we can reorder the 'Last Name, Name' format associated with the author name, to a 'Name and Last Name' format. We are doing this so it can be readable, but also to avoid further troubles when merging the different data sets we are preparing. We are spliting our column by the comma symbol into two splits, then we paste such splits:
```
splits<- str_split_fixed(top25.twitter$author, ",", 2)
top25.twitter$author <- paste(splits[,2], splits[,1], sep = ' ')
```
Our column with information on the IDEAS' links to each authors' personal profiles is incomplete as well, so we paste the missing information to every observation in the column by running the following code:

```
top25.twitter$ideas.link<- paste("https://ideas.repec.org", top25.twitter$ideas.link, sep = "")
```
With this last piece of information, we will be able to extract the Twitter handles from every registered author with a Twitter account that appears in the list 'Top 25% Economists, by Twitter Followers.' We will run a loop for this purpose, so we need to create an empty vector to store the data our loop will gather. The CSS selector associated with the Twitter handles is "tr:nth-child(10) td+ td a", so we add this information as well. Fortunately, this information is stored as text, so we will only trim this text. Running this code might take longer, depending on the capacities of your personal computer: 
```
t.handle      <- 0 # create empty vector 
for (i in top25.twitter$ideas.link){
  t.handle[i] <- read_html(c(i)) %>% # run loop to go through each author profile in IDEAS
    html_node("tr:nth-child(10) td+ td a") %>%
    html_text(trim=TRUE)
}
```
We save it as a data frame and delete the first row with empty values: 

```
t.handle <- as.data.frame(t.handle)
t.handle <- t.handle[-1,] # drop first empty row 
```
We incorporate the Twitter handle column into our data set: 
```
top25.twitter$t.handle<- t.handle
```
Again, with loops, we extract each authors' personal link in the Citec data base, drop the first empty row, and incoporate as a column to our first data set from website1. The citec.link variable will have a lot of missing values, but I haven't explored the reason behind this. Finally, after we incorporate this column into the data set, our final data set will look  like this: 
```
citec.link<- 0
for (i in top25.twitter$ideas.link){
  citec.link[i]<- html_attr((read_html(c(i)) %>%
                               html_node("#cites a:nth-child(3)")), "href")
}
citec.link<- as.data.frame(citec.link)
citec.link<- citec.link[-1,]
# incorporate Citec column into dataset
top25.twitter$citec.link<- citec.link # though we only have 182 citec links, and we are missing 172
head(top25.twitter)

 rank.twitter                author     id                            ideas.link t.followers         t.handle citec.link
1            1       Paul R. Krugman  pkr10  https://ideas.repec.org/e/pkr10.html     4501778     @paulkrugman       <NA>
2            2  Xavier Sala-i-Martin psa510 https://ideas.repec.org/f/psa510.html      441591    @xsalaimartin       <NA>
3            3    Joseph E. Stiglitz  pst33  https://ideas.repec.org/e/pst33.html      219312 @JosephEStiglitz       <NA>
4            4     Alejandro Gaviria pga134 https://ideas.repec.org/e/pga134.html      190877       @agaviriau       <NA>
5            5     Erik Brynjolfsson  pbr87  https://ideas.repec.org/e/pbr87.html      165839        @erikbryn       <NA>
6            6        Justin Wolfers   pwo9   https://ideas.repec.org/e/pwo9.html      143943   @justinwolfers       <NA>

```
### Extracting Data from Website2
Data set contains information on author name, IDEAS ID profile, insititution of affiliation, and total number.

We extract this information with the help of the CSS selector, we click on the the names of the authors and find out that the node that hosts this information is "td >a", so we extact text embedded in this node and store in a vector named author1:
```
author1 <- website2 %>% html_nodes("td > a")
```
To extract the name of the authors, or economists, we run the following lines of code which will clean the first 21 rows with no relevant information, and drop the empty observations:
```
author <- website2 %>% html_nodes("td > a") %>% html_text(trim=TRUE)
author <- author[-c(1:21)] # get rid of first 21 rows
author <- author[author!=""] # drop empty observations 
```
Then, we proceed to extract the ID associated with the authors' profiles within the IDEAS data base, and get rid of the empty cells in order to clean the information. Again, the IDs cannot be extracted as text, but as html attributes, therefore we use a slightly different code:
```
id <- html_attr(author1, "name")
id <- na.omit(id) # get rid of empty cells
```
Again, we use the CSS selector to click on the citations information from the website, which will return the following node "td~ td+ td". The pipe operator simply indicates tha different commands that we are performing on the node, so we can easily get the number of citations. The second line of code will drop the first 35 rows that have no relevant information:
```
citations <- website2 %>% html_nodes("td~ td+ td") %>% html_text(trim=TRUE)
citations <- citations[-c(1:35)] # drop first 35 rows with irrelevant information
```
In a similar fashion, we extract data on the institution of affiliation by inputing the node "p" in the same code. We name this vector "affiliation." To clean this data, we drop rows on top and at the bottom because they have no information. However, affiliation data is really messy but we will clean this later on:
```
affiliation <- website2 %>% html_nodes("p") %>% html_text(trim=TRUE)
affiliation <- affiliation[-c(1:3)] # drop initial rows with no info
affiliation <- affiliation[-c(2635:2659)] # drop last rows with no info 
```
We finally build the data set by binding the different columns with data on author, ID, citations, and affiliations and store it as a data frame.
```
top5.citations <- cbind(author, id, citations, affiliation)
top5.citations <- as.data.frame(top5.citations)
```
We proceed to clead data on institution of affiliation, which is not formatted nicely. We will use the "str_split_fixed" function again, to separate data on the affiliation column by comma symbols, with a total of 10 splits that will correspond to the 1)Department, 2)University, 3)City, 4)State(country)secondinstitution, 5)City of second institution, 6)state of second university, 7)second university, 8) second location, 9) second state/country. Then, we create new strings of variables out of the splits. We drop the initial "affiliation column."
```
splits2 <- str_split_fixed(top5.citations$affiliation, ",",10) 
top5.citations$department<- splits2[,1]
top5.citations$university<- splits2[,2]
top5.citations$city<- splits2[,3]
top5.citations$scnd.country.instition<- splits2[,4]
top5.citations$scnd.city<- splits2[,5]
top5.citations$scnd.state<- splits2[,6]
top5.citations$scnd.university<- splits2[,7]
top5.citations$scnd.location<- splits2[,8]
top5.citations$scnd.state.country<- splits2[,9]
top5.citations$affiliation<- NULL # drop initial column where we got splits from initially
```
This data set will look like this:
```
head(top5.citations)
              author     id citations                       department                   university                       city
1    Andrei  Shleifer  psh93     41870          Department of Economics           Harvard University                  Cambridge
2    James J. Heckman  phe22     28047          Department of Economics        University of Chicago                    Chicago
3     Robert J. Barro pba251     26492          Department of Economics           Harvard University                  Cambridge
4 Robert F. Engle III   pen9     22861               Finance Department     Stern School of Business  New York University (NYU)
5    Kenneth S Rogoff pro164     22585          Department of Economics           Harvard University                  Cambridge
6  Joseph E. Stiglitz  pst33     22541 Finance and Economics Department  Graduate School of Business        Columbia University
                                           scnd.country.instition                           scnd.city                scnd.state            scnd.university
1  Massachusetts (USA)National Bureau of Economic Research (NBER)                           Cambridge       Massachusetts (USA)                           
2                                                  Illinois (USA)                                                                                         
3                                             Massachusetts (USA)                                                                                         
4                                                   New York City  New York (USA)Volatility Institute  Stern School of Business  New York University (NYU)
5                                             Massachusetts (USA)                                                                                         
6                                                   New York City                      New York (USA)                                                     
   scnd.location scnd.state.country
1                                  
2                                  
3                                  
4  New York City     New York (USA)
5                                  
6                                  

```
### Extracting data from website 3
This data set will contain data on author name, the H-index, and the rank associated with the H-index. 

We start off by extracting the ranking information from website 3, which wil have a similar code to our previous exapmples, but we will change the html node to "tr > :nth-child(1)" that we discovered with the help of the CSS selector. We also drop the first 6 rows with no relevant information: 
```
h.ranking<- website3 %>% 
  html_nodes("tr > :nth-child(1)") %>%
  html_text(trim=TRUE)
h.ranking<- h.ranking[-c(1:5)] # drop first 6 rows with no relevant
```
The html node with ID information corresponds to "td > a", but we are only able to extract this information with an additional line of code that will extract the attributed of the html node. We omit all NA values to clean the data:
```
author<- website3 %>%
  html_nodes("td > a") 
id<- html_attr(author, "name")
id<- na.omit(id) # drop all na values
```
For the author name, we use the "html_text" function instead of the attributes one, and trim the text. To clean the data we drop the empty cells and the first 21 rows.
```
h.author<- website3 %>%
  html_nodes("td > a") %>%
  html_text(trim=TRUE)
h.author<- h.author[h.author!=""] # drop empty cells
h.author<- h.author[-c(1:21)] # delete first 21 rows
```
The H-index is also embedded as a text within the node "d~ td+ td". We drop the first rows with no relevant information.
```
h.index<- website3 %>%
  html_nodes("td~ td+ td") %>%
  html_text(trim=TRUE)
h.index<- h.index[-c(1:35)]
```
Finally, we bind these strings of data into one data frame that we will name "top5.hindex." We will drop the columns with second instuttions, cities or states for simplicity. Although the final data set on Top 5% authors by H-index will look like this:
```
top5.hindex<- cbind(id, h.ranking, h.index, h.author)
head(top5.hindex)
  id h.ranking h.index             author
1 psh93         1      87   Andrei  Shleifer
2 phe22         2      79   James J. Heckman
3 pti33         3      74       Jean  Tirole
4 pre33         4      66 Carmen M. Reinhart
5 pst33         5      65 Joseph E. Stiglitz
6 pac16         6      64    Daron  Acemoglu 
```
## Merging Data Sets
Now that we have a total of 3 data sets (top5.hindex, top5.citations, and top25.twitter), we can merge them to build a final data set which we will use for our regression analysis. 

We will start by merging the top5.index and top.5ciations data sets by ID and atuhor (which are variables that will match). We also rename the column h.author by author only, to make the merging process easier. We call this first set of merged data merged.data1:
```
top5.hindex<- as.data.frame(top5.hindex)
colnames(top5.hindex)[which(names(top5.hindex)=="h.author")] <- "author"
merged.data1<- merge(top5.hindex, top5.citations, by.x=c("id","author"), all = TRUE)
```
We merge merged.data1 with top25.twitter, by matching observations by ID and author, we also rename author.id for id in the top35.twitter data set for easier merge: 
```
colnames(top25.twitter)[which(names(top25.twitter)=="author.id")] <- "id"
merged.data2<- merge(top25.twitter, merged.data1, by="id") # dropping all non-matching observations
```
Finally, we clean our data set, mainly we get read on repeated column-wise observations and information that I do not consider relevant, although it could potentially be used for other sorts of statistical anaysis. 
```
merged.data2$author.y<- NULL
merged.data2$scnd.country.instition<- NULL
merged.data2$scnd.city<- NULL
merged.data2$scnd.state<- NULL
merged.data2$scnd.university<- NULL
merged.data2$scnd.location<- NULL
merged.data2$scnd.state.country<- NULL
final.dataset<- merged.data2
```
By looking at the structure of this data set, we see that most variables are in factor form instead of the numeric form.
```
str(merged.data2)
data.frame':	94 obs. of  13 variables:
 $ id          : Factor w/ 309 levels "pac16","pac70",..: 1 9 10 14 18 23 29 34 35 37 ...
 $ rank.twitter: Factor w/ 305 levels "1","10","100",..: 250 116 19 9 279 24 35 267 143 135 ...
 $ author.x    : chr  " Daron Acemoglu" " Joshua D Angrist" " David Andolfatto" " Susan Carleton Athey" ...
 $ ideas.link  : chr  "https://ideas.repec.org/e/pac16.html" "https://ideas.repec.org/e/pan29.html" "https://ideas.repec.org/e/pan9.html" "https://ideas.repec.org/e/pat6.html" ...
 $ t.followers : Factor w/ 305 levels "10105","101142",..: 117 186 294 3 40 2 304 53 259 163 ...
 $ t.handle    : Factor w/ 310 levels "@1954swilliamson",..: 73 197 57 275 26 153 29 215 56 78 ...
 $ citec.link  : Factor w/ 128 levels "0","http://citec.repec.org/p/a/pag22.html",..: NA NA NA NA NA NA NA NA NA NA ...
 $ h.ranking   : Factor w/ 53 levels "1","100","109",..: 41 40 NA 42 47 50 47 52 14 42 ...
 $ h.index     : Factor w/ 53 levels "14","15","16",..: 48 31 NA 10 28 8 28 45 20 10 ...
 $ citations   : Factor w/ 1683 levels "1000","10006",..: 704 1604 1595 569 1369 747 417 559 1375 1093 ...
 $ department  : chr  "Economics Department" "Economics Department" "Research Division" "Graduate School of Business" ...
 $ university  : chr  " Massachusetts Institute of Technology (MIT)" " Massachusetts Institute of Technology (MIT)" " Federal Reserve Bank of St. Louis" " Stanford University" ...
 $ city        : chr  " Cambridge" " Cambridge" " St. Louis" " Stanford" ...
 ```
 We convert such factors into numeric form:
 ```
final.dataset$t.followers<- as.numeric(as.character(final.dataset$t.followers))
final.dataset$h.index<- as.numeric(as.character(final.dataset$h.index))
final.dataset$citations<- as.numeric(as.character(final.dataset$citations))
```
We finally create additional variables that will take the natural log of our variables of interest: number of twitter followers and total number of citations.
```
final.dataset$ln.followers<- log(final.dataset$t.followers)
final.dataset$ln.citations<- log(final.dataset$citations)
```
Now that we have our final data set, we can attach, so we can reorder the variables to be visually more appealing. We drop the citec.link column because no observations were left after mergning our data sets. 
```
attach(final.dataset) 
final.dataset<- final.dataset[c("id", "author.x", "ideas.link", "t.handle", "citec.link","department", "university", "city", "rank.twitter", "t.followers", "ln.followers", "citations", "ln.citations", "h.ranking", "h.index")] 
final.dataset$citec.link<- NULL
head(final.dataset) 
    id              author.x                            ideas.link         t.handle                      department
1  pac16        Daron Acemoglu  https://ideas.repec.org/e/pac16.html @DrDaronAcemoglu            Economics Department
2  pan29      Joshua D Angrist  https://ideas.repec.org/e/pan29.html       @metrics52            Economics Department
3   pan9      David Andolfatto   https://ideas.repec.org/e/pan9.html        @dandolfa               Research Division
4   pat6  Susan Carleton Athey   https://ideas.repec.org/e/pat6.html     @susan_athey     Graduate School of Business
5 pba124       Richard Baldwin https://ideas.repec.org/e/pba124.html       @baldwinre International Economics Section
6 pba464          Kaushik Basu https://ideas.repec.org/f/pba464.html    @kaushikcbasu         Department of Economics
                                                        university       city rank.twitter t.followers ln.followers citations ln.citations h.ranking h.index
1                      Massachusetts Institute of Technology (MIT)  Cambridge          250       24034    10.087225     19676     9.887155         6      64
2                      Massachusetts Institute of Technology (MIT)  Cambridge          116        3677     8.209852      9207     9.127719        57      44
3                                Federal Reserve Bank of St. Louis  St. Louis           19        8696     9.070618       914     6.817831      <NA>      NA
4                                              Stanford University   Stanford            9       10249     9.234935      1715     7.447168       628      23
5  The Graduate Institute of International and Development Studies     Genève          279       16205     9.693075      5188     8.554104        72      41
6                                               Cornell University     Ithaca           24      101142    11.524281      2060     7.630461       824      21
> 
```
## Statistical Analysis
Now that we have our final data set, we can start computing the summary statistics and regression analysis. We start with descriptive statistics on our variables of interest, which are t.followers, citations, and h.index, that we bind in a new data frame that we name 'descriptive.' Then we compute the summary statistics.
```
descriptive<- cbind(final.dataset$t.followers, final.dataset$citations, final.dataset$h.index) # assign column names
colnames(descriptive)<- c("t.followers", "citations", "h.index")
summary(descriptive)
  t.followers        citations        h.index     
 Min.   :   1948   Min.   :  834   Min.   :14.00  
 1st Qu.:   4322   1st Qu.: 1216   1st Qu.:17.00  
 Median :  10782   Median : 2280   Median :23.00  
 Mean   :  79153   Mean   : 5001   Mean   :28.59  
 3rd Qu.:  29953   3rd Qu.: 7174   3rd Qu.:39.00  
 Max.   :4501778   Max.   :28047   Max.   :79.00  
                   NA's   :6       NA's   :7     
 ```
 Again, the structure of our variables of interst won't allow us to easily run certain functions, so we transform them into the numeric format, and then we calculate the standard deviations, skewness and kurtosis for each of our variables fo interest, with the corresponding functions of our moments package in R:
 ```
 descriptive <- as.data.frame(descriptive)
t.followers <- as.numeric(descriptive$t.followers)
citations   <- as.numeric(descriptive$citations)
h.index     <- as.numeric(descriptive$h.index)

sd(descriptive$t.followers) # s.d. for twitter followers
[1] 464782.6
skewness(descriptive$t.followers)
[1] 9.316691
kurtosis(descriptive$t.followers)
[1] 89.10564

sd(descriptive$citations, na.rm = TRUE) # s.d. for citations
[1] 5849.281
skewness(descriptive$citation, na.rm = TRUE)
[1] 1.940945
kurtosis(descriptive$citation, na.rm = TRUE)
[1] 6.330175

sd(descriptive$h.index, na.rm = TRUE) # s.d. for h.index
[1] 15.40655
skewness(descriptive$h.index, na.rm = TRUE)
[1] 1.299223
kurtosis(descriptive$h.index, na.rm = TRUE)
[1] 3.932327
```
The regression analysis will be calculated with the following code, where the percent change point of Twitter followers will be the dependent variable, and ln.citation and h.index will be our independent variables.
```
results.regression<- lm(ln.followers ~ ln.citations + h.index, data = final.dataset)
summary(results.regression)
Call:
lm(formula = ln.followers ~ ln.citations + h.index, data = final.dataset)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.4971 -0.9893 -0.2432  0.9799  4.5641 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)   
(Intercept)  -0.67092    2.91189  -0.230  0.81838   
ln.citations  1.48416    0.45610   3.254  0.00168 **
h.index      -0.05868    0.02848  -2.061  0.04268 * 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.315 on 78 degrees of freedom
  (13 observations deleted due to missingness)
Multiple R-squared:  0.2011,	Adjusted R-squared:  0.1806 
F-statistic: 9.818 on 2 and 78 DF,  p-value: 0.0001574
```
## Creating a Graph
We can also create a graph with data with information on all the variables of interest that we included in our linear regression, plus certain observations on the TWitter handles fo the different authors. This for visualization purposes. We start by creating a vector with information on the Twitter handles of certain authors, which are really observations I picked from final data set. 
```
pointslabel<- c("@paulkrugman", " @LHSummers" ," @amspence98","@mfratzscher"," @tnannicini", "@gregmankiwblog", "@scottirwinui", "@robertjshiller", " @krogoff", " @JeanTirole","@pikettylemonde", "@raffasadun", "@r_thaler", " @JeanTirole", "@raffasadun", "@DrDaronAcemoglu", "@heckmanequation")
pointslabel
unique(final.dataset$t.handle)
```
We use the ggplot package to create this plot.
```
p1<- ggplot(final.dataset,aes(y = ln.followers, 
                         x = ln.citations))+
     geom_point(aes(color=h.index))+
     scale_x_continuous(name = "Percentage Change in citations",
                      breaks = 1:10)+
     scale_y_continuous(name = "Percentage Change Twitter Followers",
                      breaks = 1:15)+
     scale_color_continuous(name = "H-Index")+
     geom_text_repel(aes(label=t.handle),
                     color = "grey20",
                     data = subset(final.dataset, final.dataset$t.handle %in% pointslabel),
                     force = 10)+
  ggtitle("Twitter Followers and Citations Among Economists")
```



