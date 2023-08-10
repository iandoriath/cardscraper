# wrestlingdata.com has TV ratings information that Cagematch does not track
# This code based on scraping package by Jason Laso (2017)

library(rvest)
library(stringr)
library(tidyr)

# scrape example given on website just for reference on how to use rvest
url = "http://wrestlingdata.com/index.php?befehl=quoten"
webpage = read_html(url)
sb_table <- html_nodes(webpage, 'table')
sb <- html_table(sb_table, fill = T, header = T)[[13]]
head(sb)

#initialize vector for loop
ratings=c()

# Loop to scrape all pages with Raw info
for(i in 1:30){
  
  #assemble list of URLs
  url[i] = paste("http://wrestlingdata.com/index.php?befehl=quoten&art=2&liga=1&show=1&sort=0&seite=",i, sep="")
  
  #read the url and isoltate the nodes
  Sys.sleep(1)
  webpage = read_html(url[i])
  htmlnodes <- html_nodes(webpage, 'table')
  
  #read in the 13th node (ratings data) as a table
  page.data <- html_table(htmlnodes, fill = T, header = T)[[14]]
  
  #append each page into ratings data frame
  ratings = rbind(ratings, page.data)
  
  #progress tracker for loop
  print(i)
}

# Loop to scrape all pages with Smackdown info
for(i in 1:24){
  
  #assemble list of URLs
  url[i] = paste("http://wrestlingdata.com/index.php?befehl=quoten&art=2&liga=1&show=2&sort=0&seite=",i, sep="")
  
  #read the url and isoltate the nodes
  Sys.sleep(1)
  webpage = read_html(url[i])
  htmlnodes <- html_nodes(webpage, 'table')
  
  #read in the 13th node (ratings data) as a table
  page.data <- html_table(htmlnodes, fill = T, header = T)[[14]]
  
  #append each page into ratings data frame
  ratings = rbind(ratings, page.data)
  
  #progress tracker for loop
  print(i)
}

#rename columns
colnames(ratings) = c("date", "event", "rating","viewers")

#reformat dates
ratings$date = as.Date(ratings$date, "%Y/%m/%d")

#let's also extract year & month into their own columns for possible seasonal analysis.
ratings$year = as.numeric(format(ratings$date, "%Y"))
ratings$month = as.numeric(format(ratings$date, "%m"))

#ratings and views read in as characters. change missings to 0 and then convert to numeric
ratings$rating[ratings$rating == "-"] = 0
ratings$rating = as.numeric(ratings$rating)

#viewers data reads in commas in numbers. remove the commas before conversion
ratings$viewers = gsub('\\,', '', ratings$viewers)
ratings$viewers[ratings$viewers == "-"] = 0
ratings$viewers = as.numeric(ratings$viewers)

#check structure
str(ratings)
summary(ratings)

# We need to extract "Raw" and "Smackdown" from the event variable because the shows have used different 
# iterations of the same names over the years. This loop searches each row and assigns a much simpler 
# show name to a new column, show.

for(i in 1:nrow(ratings)){
  
  # This is searching for "RAW" or "Smack" in the event string and returning the new show column.
  
  if(grepl("RAW", ratings$event[i])){
    ratings$show[i] = "raw"
    
  }else if(grepl("Smack", ratings$event[i])){
    ratings$show[i] = "smackdown"
    
  }else{
    ratings$show[i] = "misc"
  }
}

# Check results
table(ratings$show)

# The scrape reads in an extra row, "Brawl to End it All" since it includes the word "raw". Remove that row.
ratings = ratings[-which(ratings$show == "misc"),] 

# Let's write a csv to our directory with our cleaned up dataset now.
#write.csv(ratings, "wwetvratings.csv", row.names=F)