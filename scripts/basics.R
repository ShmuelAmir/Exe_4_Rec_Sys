library(RMySQL)
library(sqldf)
library(dplyr)

mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname='book_recommendations',
                            host='localhost',
                            port=3306,
                            user='root',
                            password='password')

# how many users?
users <- dbGetQuery(mysqlconnection, "SELECT * FROM book_recommendations.`bx-users`;")
dim(users)[1]

# how many books?
books <- dbGetQuery(mysqlconnection, "SELECT * FROM book_recommendations.`bx-books`;")
dim(books)[1]

# how many ratings? 
ratings <- dbGetQuery(mysqlconnection, "SELECT * FROM book_recommendations.`bx-book-ratings`;")
dim(ratings)[1]

# histograms of ratings (by user/by book)
rating_user_counts <- ratings %>%
  group_by(`User-ID`) %>%
  summarise(bin = n()) %>%  # bin -> how many time user rates
  group_by(bin) %>%
  summarise(N = n())        # N   -> how many people give number of ratings

rating_book_counts <- ratings %>%
  group_by(`ISBN`) %>%
  summarise(bin = n()) %>%
  group_by(bin) %>%
  summarise(N = n())


# top-10 books with ratings? histogram of ratings
top_10_books <- ratings %>%
  group_by(`ISBN`) %>%
  summarize(N = n()) %>%
  arrange(desc(N)) %>%
  head(11) %>%
  left_join(books, by="ISBN") %>%
  select(`Book-Title`, N) %>%
  na.omit()

# top-10 users that rated? histogram of ratings
top_10_users <- ratings %>%
  group_by(`User-ID`) %>%
  summarize(N = n()) %>%
  arrange(desc(N)) %>%
  head(10)

# save what we need in the main file
save(users, books, ratings, file = "recommender3.rdata")

rm(mysqlconnection, users, books, ratings, rating_user_counts, rating_book_counts, top_10_users, top_10_books)
gc()
