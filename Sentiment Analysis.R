install.packages("xml2")
library(xml2)
library(rvest)
library(dplyr)
library(ggplot2)
library(magrittr)
library(qdap)
library(gdata)
library(plyr)
library(tidytext)
library(tm)
library(rJava)
library(Rwordseg)
library(SnowballC)
library(slam)
library(Matrix)
library(RColorBrewer)
library(wordcloud)

#data from Dec.20, 2018
#scrap and clean ranked 1st book Community Reviewers data
#commentor's individual stars on all pages (first book)
first.reviews.stars <- c()
for(i in 1:10){
  URL <- read_html(paste0("https://www.goodreads.com/book/show/41839602-still-me?ac=1&from_search=true&page=",i))
  all.reviews.stars = html_nodes(URL, ".friendReviews") %>% html_text()
  all.info <- gsub("\n", "", all.reviews.stars)
  want.info <- gsub("[·].*","",all.info)
  want.info <- gsub(".* rated it","",want.info)
  want.info <- gsub(".* marked it as","",want.info)
  want.info <- trim(want.info)
  all.stars <- revalue(want.info, c("did not like it"="1","it was ok"= "2","liked it"="3","really liked it"="4", "it was amazing" = "5"))
  first.reviews.stars = c(first.reviews.stars, all.stars)
  Sys.sleep(sample(3:5, 1))
  print(i)
}

head(first.reviews.stars,20)

write.table(first.reviews.stars, file = "first.reviews.stars.txt", row.names = FALSE, col.names = "review.star")
first.reviews.stars <- read.delim("first.reviews.stars.txt")

View(first.reviews.stars)


#review text from all pages (first book)
first.reviews.text <- c()
for(i in 1:10){
  URL <- read_html(paste0("https://www.goodreads.com/book/show/41839602-still-me?ac=1&from_search=true&page=",i))
  all.reviews = html_nodes(URL, ".reviewText") %>% html_text()
  reviews.text <- gsub("\n", "", all.reviews)
  reviews.text <- tolower(reviews.text)
  reviews.text <- removePunctuation(reviews.text)
  first.reviews.text = c(first.reviews.text, reviews.text)
  Sys.sleep(sample(3:5, 1))
  print(i)
}
View(head(first.reviews.text))

#combine commentor's individual stars & comment as df
first.reviewers.df <- data.frame(first.reviews.stars, first.reviews.text, stringsAsFactors = FALSE)

View(first.reviewers.df)

#There is one GIF, turned the GIF back into words (found out below when doing tf-idf)
#May not be the same review number now, because these information were scraped on Dec.20, 2018

View(first.reviews.text[244])
first.reviews.text[244] <- "I am bored"



#select only the comments with star reviews 
select <- c(1,2,3,4,5)
View(first.reviewers.df[first.reviewers.df$stars %in% select,])
first.reviewers.df <- first.reviewers.df[first.reviewers.df$stars %in% select,]
View(head(first.reviewers.df))

write.csv(first.reviewers.df , file = "first.reviews.star.text.csv", row.names = FALSE, col.names = TRUE)
first.reviewers.df <- read.csv("first.reviews.star.text.csv")
View(first.reviewers.df)


#polarity of text
first.reviewers.df %$% polarity(text)


# Calculate polarity score by stars
book1_conversation <- first.reviewers.df %$% polarity(text, stars)

#polarity chart
pol_chart <- scores(book1_conversation)
View(pol_chart)


# Plot the conversation polarity in word doc
plot(book1_conversation)

