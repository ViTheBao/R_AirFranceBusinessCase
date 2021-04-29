

#install.packages("tidyverse")
#install.packages("rJava")
#install.packages("qdap")
#install.packages("tm")
#install.packages("worldcloud")
#install.packages("plotrix")
#install.packages("dendextend")
#install.packages("ggthemes")
#install.packages("RWeka") #idk what this is for
#install.packages("stringr") # Install stringr package

#Calling the libraries
library(readxl)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyverse)
library(rJava)
library(qdap)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggthemes)
library(stringr)
#library(RWeka)

#############################################
#Primary Data Massaging
############################################
airfrance_df<- read_excel("Downloads/Air France Case Spreadsheet Supplement.xls", sheet= "DoubleClick")
View(airfrance_df)

#looking at structure of air france
nrow(airfrance_df)
glimpse(airfrance_df)
str(airfrance_df)
#Copying the dataframe to perform operations 
airfrance <- airfrance_df

#Obtaining the overall columns names and removing unused column in the data
colnames(airfrance)
remove_col <- c('Publisher ID', 'Keyword ID', 'Match Type', 'Campaign', 'Keyword Group', 'Category', 'Keyword Type', 'Status', 
                 'Total Cost/ Trans.')
'Engine Click Thru %'
#'Trans. Conv. %'
airfrance <- select(airfrance, -remove_col)

#Checking if any in large numbers or empty string is present in the data 
sum(airfrance== Inf)
sum(airfrance== "")
sum(is.na(airfrance))

#using the sapply function to check for missing values source
sapply(airfrance, function(x) sum(is.na(x)))

#Creating a revenue variable in the table
airfrance$revenue <- airfrance$Amount - airfrance$`Total Cost`

#Creating revenue/per booking variable 
airfrance$'Revenue per booking' <- round(airfrance$revenue/airfrance$`Total Volume of Bookings`,2)

#Creating booking probability
airfrance$'Booking Probability' <- round(( airfrance$`Trans. Conv. %` * airfrance$`Engine Click Thru %`) /100,2)
#Creating booking cost 
airfrance$'Cost per booking' <-as.numeric(airfrance$`Total Cost`/ airfrance$`Total Volume of Bookings`)

#turning us-publishers into 1, globals = 0 
airfrance$'US Publisher' <- c()
airfrance$'US Publisher' <- str_detect(airfrance$`Publisher Name`, "US")
airfrance$'US Publisher' <- as.numeric(airfrance$'US Publisher')

#substituting errors in spelling
unique(airfrance$`Bid Strategy`)
airfrance$`Bid Strategy` <- gsub("Position 1 -2 Target", "Position 1-2 Target", 
                                 airfrance$`Bid Strategy`)
airfrance$`Bid Strategy` <- gsub("Postiion 1-4 Bid Strategy", "Position 1-4 Bid Strategy", 
                                 airfrance$`Bid Strategy`)


#Checking for negative revenues  and creating a separate dataset for analysis 
sum(airfrance$revenue < 0) ## question what should we do with that 
neg_rev_air <- airfrance[which(airfrance$revenue < 0),]


###############################################
#Descriptive Statistics
###############################################
#creating a new data frame
air_df <- airfrance

#stats for mean, median, sd, min, max of meaningful numeric variables
air_stats <- c("Mean", "Median", "SD", "Min", "Max")
impressions <- round(c(mean(air_df$Impressions),median(air_df$Impressions),sd(air_df$Impressions),min(air_df$Impressions),max(air_df$Impressions)), 2)
num_clicks <- round(c(mean(air_df$Clicks),median(air_df$Clicks),sd(air_df$Clicks),min(air_df$Clicks),max(air_df$Clicks)), 2)
total_amount <- round(c(mean(air_df$Amount),median(air_df$Amount), sd(air_df$Amount),min(air_df$Amount), max(air_df$Amount)), 2)
total_cost <- round(c(mean(air_df$`Total Cost`),median(air_df$`Total Cost`),sd(air_df$`Total Cost`),min(air_df$`Total Cost`),max(air_df$`Total Cost`)), 2)
net_revenue <-round(c(mean(air_df$revenue),median(air_df$revenue),sd(air_df$revenue),min(air_df$revenue),max(air_df$revenue)), 2)
#net_rpb <- round(c(mean(air_df$`Revenue per booking`),median(air_df$`Revenue per booking`),sd(air_df$`Revenue per booking`),min(air_df$`Revenue per booking`),max(air_df$`Revenue per booking`)), 2)
net_booking_prob <- round(c(mean(air_df$`Booking Probability`),median(air_df$`Booking Probability`),sd(air_df$`Booking Probability`),min(air_df$`Booking Probability`),max(air_df$`Booking Probability`)), 2)
#creating a dataframe to show the summary of air_stats
air_summ <- as.data.frame(cbind(air_stats, impressions , num_clicks, total_amount, total_cost, net_revenue, net_booking_prob))
air_summ


#for each company 
#competitors by SEM
competitor <- unique(air_df$`Publisher Name`)
competitor
#sales by competitor SEM
#starting with an empty vector
total_rev <- c()
total_clicks <- c()
cost_total <- c()
total_book <- c()
total_net_revenue <- c()
#creating a for loop to compare competitors
for (i in 1:length(competitor)) {
  total_rev <- c(total_rev,sum(airfrance$revenue[which(airfrance[,1] == competitor[i])]))
  print(total_rev)
  total_clicks <- c(total_clicks,sum(airfrance$Clicks[which(airfrance[,1]==competitor[i])]))
  cost_total<- c(cost_total,sum(airfrance$`Total Cost`[which(airfrance[,1]==competitor[i])]))
  total_book <- c(total_book,sum(airfrance$`Total Volume of Bookings`[which(airfrance[,1]==competitor[i])]))
  total_net_revenue <- c(total_net_revenue, sum(airfrance$revenue[which(airfrance[,1]==competitor[i])]))

}#ending loop

#combining Kayak with other publishers for a total amount 
all_competitors <- cbind(c(competitor,"Kayak-US"))
competitor_sales <- as.numeric(cbind(c(total_rev, "233694")))
competitor_clicks<- as.numeric(cbind(c(total_clicks, "2839")))
competitor_costs <- as.numeric(cbind(c(cost_total, "3567.13")))
competitor_books <- as.numeric(cbind(c(total_book, "208")))
competitor_net_revenue <- as.numeric(cbind(c(total_net_revenue, "230126.87")))

#creating a matrix for overall competitors
my_matrix<- matrix( c(all_competitors, competitor_sales, competitor_clicks, competitor_costs, competitor_books, competitor_net_revenue), ncol = 6, nrow = 8)
overall_competitors<- as.data.frame(my_matrix)
#changing the column names
colnames(x= overall_competitors) <- c("Competitors", "Total Sales", "Total Clicks", "Total Costs", "Total Bookings", "Net Revenue")

#Changing the type of the variables in the matrix 
overall_competitors$`Total Sales` <- round(as.numeric(overall_competitors$`Total Sales` ))
overall_competitors$`Total Clicks` <- round(as.numeric(overall_competitors$`Total Clicks`))
overall_competitors$`Total Costs` <- round(as.numeric(overall_competitors$`Total Costs`))
overall_competitors$`Total Bookings` <- round(as.numeric(overall_competitors$`Total Bookings`))
overall_competitors$`Net Revenue` <- round(as.numeric(overall_competitors$`Net Revenue`))
# Adding new columns with usefull information
overall_competitors$'Revenue per booking' <- round(overall_competitors$`Net Revenue`/overall_competitors$`Total Bookings`)
overall_competitors$'Cost per booking' <- round(overall_competitors$`Total Costs`/overall_competitors$`Total Bookings`)
overall_competitors$'ROA' <- round((overall_competitors$'Revenue per booking'/overall_competitors$'Cost per booking'), 2)
overall_competitors$'Avg. CPC'<- (overall_competitors$'Total Costs'/overall_competitors$'Total Clicks')

#empty vector to create 1, 0 in the United States or not
overall_competitors$'US based' <- c()
overall_competitors$'US based' <- str_detect(overall_competitors$`Competitors`, "US")
overall_competitors$'US based' <- as.character(as.numeric(overall_competitors$'US based'))



#Ordering the values based on the ROA
overall_competitors[order(overall_competitors$ROA),]

#printing overall competitors and their totals
overall_competitors

#Logistic regression to find the 
#is there a difference of what SEM is doing in the US vs ROW- no
#need to focus on something to better their market share in the states
air_regression <- airfrance

#initial logistic regression
my_logitair<- glm(airfrance$`US Publisher` ~ airfrance$'Booking Probability' +airfrance$revenue + airfrance$'Amount' + airfrance$'Total Volume of Bookings'+airfrance$'Clicks'+airfrance$'Impressions'+airfrance$'Total Cost'+airfrance$'Avg. Pos.', 
                  data=air_regression, family = "binomial") 
summary(my_logitair)

#Cleaned logistic regression 
cleaned_logitair<- glm(airfrance$`US Publisher` ~  airfrance$'Impressions'+airfrance$'Avg. Pos.' , 
                  data=air_regression, family = "binomial") 
summary(cleaned_logitair)


#creating a pivot table to compare ROA and Cost per clicks
air_df_piv <- airfrance %>% group_by(`Publisher Name`) %>% summarize(
  af_avg_ROA = mean(air_ROA),
  af_avg_cpc = mean(`Avg. Cost per Click`)
)
summary(air_df_piv)
#maybe we could campare impressions and bpooking???

#creating a pivot table to compare SEMS
air_piv2 <- overall_competitors %>% group_by(Competitors) %>% summarize(
  overall_records = n(),
  sum_tc = sum(`Total Costs`),
  avg_ROA = mean(ROA),
  avg_cpc = mean(`Avg. CPC`),
  avg_prob = mean(`Total Bookings`),
  
)

summary(air_piv2 )

###############################
#Using a bar chart to find highest ROA in relation to competitor
#Kayak wa sthe higest ROA bc theyre cheap...Google charges a lot
x <- (overall_competitors$Competitors)
#where are the Ys coming from
y <- c(overall_competitors$ROA)
data <- data.frame(x, y)

data$x <- factor(data$x, levels = data[["x"]])
p <- plot_ly(data, x= ~x, y= ~y, type = "bar", name = "Return on Advertising", color = I("blue"), alpha = 0.5) %>%
  layout(title = "Return On Advertising",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p
########################################
#creating a bubble Chart
#Bubble CHart

p <- plot_ly(air_piv2, x = ~sum_tc, y = ~avg_prob,
             textposition = "auto",
             type = 'scatter', 
             mode = 'markers', 
             size = ~avg_cpc, 
             color = ~`Competitors`, 
             colors = 'Paired',
             marker = list(opacity = 0.8, sizemode = 'diameter')) %>%
  layout(title = 'Bookings Based on Clicks',
         xaxis = list(title = "Sum of Total Clicks", showgrid = TRUE),
         yaxis = list(title = "Average Amount of Bookings ", showgrid = TRUE),
         showlegend = TRUE)

p
summary(air_piv2)
####Findings from bubble 
#Even though you are paying for higher costs per click on google, yahoo- you may not pay as much for yahoo in clicks but theres still a high
#amount of booking. 
#may also gove startegy on how SE marketing works-clearly yahoos BS works....
#why does the excell sheet not show yahoo global?====Business Recommendation
#increase start through yahoo bc its primarily in the US

#Creating a Stacked Bar Chart
# Stacked Bar Plot with Colors and Legend
#looking at overall competitors
#WE NEED TO RESCALE THIS GRAPH
p4 <- ggplot() + geom_bar(aes(y = `Total Costs` , x = `US based`, fill = Competitors), data = overall_competitors,
                          stat="identity")
p4

#Using Match Data Type Frame

air_md <- air_df

#Slice the data

#changing to NAs 
#WE NEED TO DO MORE WITH BIDSTRATEGY\
air_md$air_ROA <- as.numeric(air_md$revenue / air_md$`Total Cost`)
air_md[air_md=="Inf"] <- NA

air_md <- air_md[which(air_md$air_ROA > 0),]

air_md <- air_md[which(air_md$air_ROA != 'NA'),]

bid_strat <- unique(air_md$`Bid Strategy`)

bid_strat1 <- bid_strat

#creating empty vectors for competitors
ROA_bid_strat <-c()
Yahoo_US_BS <- c()
MSN_Global_BS <- c()
MSN_US_BS <- c()
Google_Global_BS <- c()
Google_US_BS <- c()
Overture_Global_BS<- c()
Overture_US_BS <- c()


for (i in 1:length(bid_strat)){
  Yahoo_US_BS <- c(Yahoo_US_BS, mean(air_md$air_ROA[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                        air_md$'Publisher Name' == competitor[1])]))
  MSN_Global_BS <- c(MSN_Global_BS, mean(air_md$air_ROA[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                            air_md$'Publisher Name' == competitor[2])]))
  Google_Global_BS <- c(Google_Global_BS, mean(air_md$air_ROA[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                  air_md$'Publisher Name' == competitor[3])]))
  Overture_Global_BS <- c(Overture_Global_BS, mean(air_md$air_ROA[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                      air_md$'Publisher Name' == competitor[4])]))
  Google_US_BS <- c(Google_US_BS, mean(air_md$air_ROA[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                          air_md$'Publisher Name' == competitor[5])]))
  Overture_US_BS <- c(Overture_US_BS, mean(air_md$air_ROA[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                              air_md$'Publisher Name' == competitor[6])]))
  MSN_US_BS <- c(MSN_US_BS, mean(air_md$air_ROA[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                  air_md$'Publisher Name' == competitor[7])]))
  i <- i +1
}

#creating empty verctors for average positioning
Yahoo_US_AP <- c()
MSN_Global_AP <- c()
MSN_US_AP <- c()
Google_Global_AP <- c()
Google_US_AP <- c()
Overture_Global_AP<- c()
Overture_US_AP <- c()
#looking at average positioning compared to bid strategy
for (i in 1:length(bid_strat)){
  Yahoo_US_AP <- c(Yahoo_US_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                            air_md$'Publisher Name' == competitor[1])]))
  MSN_Global_AP <- c(MSN_Global_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                air_md$'Publisher Name' == competitor[2])]))
  Google_Global_AP <- c(Google_Global_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                      air_md$'Publisher Name' == competitor[3])]))
  Overture_Global_AP <- c(Overture_Global_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                          air_md$'Publisher Name' == competitor[4])]))
  Google_US_AP <- c(Google_US_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                              air_md$'Publisher Name' == competitor[5])]))
  Overture_US_AP <- c(Overture_US_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                  air_md$'Publisher Name' == competitor[6])]))
  MSN_US_AP <- c(MSN_US_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                        air_md$'Publisher Name' == competitor[7])]))
}

ROA_bid_strat <- cbind(bid_strat, Yahoo_US_BS,Yahoo_US_AP, MSN_Global_BS,MSN_Global_AP, 
                       Google_Global_BS, Google_Global_AP,
                       Overture_Global_BS, Overture_Global_AP, Google_US_BS, Google_US_AP,
                       Overture_US_AP, Overture_US_BS, MSN_US_AP, MSN_US_BS)
View(ROA_bid_strat)

#creating a new data frame
air_md2 <- air_df

#Slice the daa
air_md2$air_ROA <- as.numeric(air_md2$revenue / air_md2$`Total Cost`)
air_md2[air_md2=="Inf"] <- NA
air_md2 <- air_md2[which(air_md2$air_ROA != 'NA'),]
bid_strat <- unique(air_md2$`Bid Strategy`)

#creating a stacked bar chart to look at bid strategy vs revenue
#all of the global publishers have specific bid strategys, 
#google is around the board but all of their Us competitors dont really
#us comp need specific positions in order to target the market better
p4 <- ggplot() + geom_bar(aes(y = `revenue` , x = `Bid Strategy`, fill = `Publisher Name`), data = air_md2,
                          stat="identity")
p4

#Looking for keywords
key_air <- air_df

View(key_air)

# Filter the Data with ROA higher than 0
key_air$air_ROA <- as.numeric(key_air$revenue / key_air$`Total Cost`)

#key_air <- filter(key_air, air_ROA > 0)
nrow(key_air)
key_air_us <- filter(key_air, `US Publisher` == 1)
key_air_us

barplot(key_air_us[1:10,]$air_ROA, las=1, names.arg= key_air_us[1:10,]$Keyword, 
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

# analysis  by channel
Key_Global <- key_air$Keyword
View(Key_Global)
head(Key_Global)


key_global_us <- key_air_us$Keyword
View(key_global_us)
head(key_global_us)

#looking at keywords and sources
key_source <- VectorSource(Key_Global)
key_corpus <- VCorpus(key_source)

key_corpus

#looking at certain keywordds
key_corpus[[14]]
key_corpus[[14]]$content


#IM NOT SURE IF WE CAN CHANGE THE CORPUS PART....
#to help create a matrix of terms for each observation
clean<- function(corpus){
  Corpus <- tm_map(corpus, stripWhitespace)
  Corpus <- tm_map(corpus, removePunctuation)
  Corpus <- tm_map(corpus, removeNumbers)
  Corpus <- tm_map(corpus, content_transformer(tolower))
  Corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  return(corpus)
}

clean_corp <- clean(key_corpus)

# Print out a cleaned up Global keyword

clean_corp[[24]][1]

#creating a term document matrix to clean 

key_tdm <- TermDocumentMatrix(clean_corp)

# Print key_tdm data
key_tdm

# Convert to a matrix
key_matrix<- as.matrix(key_tdm)

# Print the dimensions of the matrix
dim(key_matrix)


# Review a portion of the matrix
key_matrix[ 60:65, 10:20]

#calculating row sums for key_matrix
key_term_freq <- rowSums(key_matrix)
key_term_freq <- sort(key_term_freq, decreasing = T)

# View the top 10 most common words
key_term_freq[1:10]


#word clouds
Global_terms <- data.frame(
  term = names(key_term_freq),
  num = key_term_freq
)

key_source_us <- VectorSource(key_global_us)
key_corpus_us <- VCorpus(key_source_us)

key_corpus_us
clean_corp_us <- clean(key_corpus_us)

key_tdm_us <- TermDocumentMatrix(clean_corp_us)

# Print key_tdm data
key_tdm_us

# Convert to a matrix
key_matrix_us<- as.matrix(key_tdm_us)

# Print the dimensions of the matrix
dim(key_matrix_us)

#calculating row sums for key_matrix
key_term_freq_us <- rowSums(key_matrix_us)
key_term_freq_us <- sort(key_term_freq_us, decreasing = T)

#word clouds
Global_terms_us <- data.frame(
  term = names(key_term_freq_us),
  num = key_term_freq_us
)

#BAr plot looking at global terms
barplot(Global_terms_us[1:10,]$num, las=1, names.arg= Global_terms_us[1:10,]$term, col ="lightblue", main ="Most Frequent Words-US",
        ylab = "Word frequencies-US")

###CHANGE THE COLORS
head(Global_terms)
wordcloud(Global_terms$term, Global_terms$num,
          max.words = 100, 
          colors = c("grey80", "red", "blue4")
)


