library(dplyr)
library(ggplot2)
#library(knitr)
library(recommenderlab)
library(stringr)
library(crayon)
library(RMySQL)
library(sqldf)


# Load the data from MySQL
mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname='book_recommendations',
                            host='localhost',
                            port=3306,
                            user='root',
                            password='password')

users <- dbGetQuery(mysqlconnection, "SELECT * FROM book_recommendations.`bx-users`;")
books <- dbGetQuery(mysqlconnection, "SELECT * FROM book_recommendations.`bx-books`;")
ratings <- dbGetQuery(mysqlconnection, "SELECT * FROM book_recommendations.`bx-book-ratings`;")


# ---------- Clean the data ---------- #

# remove non valid isbn
books <- books %>%
  mutate(ISBN = gsub("[^0-9]", "", ISBN)) %>%
  filter(str_length(ISBN) == 10)

ratings <- ratings %>%
  mutate(ISBN = gsub("[^0-9]", "", ISBN)) %>%
  filter(str_length(ISBN) == 10)

# get only the users, books, and ratings that have connections
ratings <- ratings %>% semi_join(books, by = "ISBN")
users <- users %>% semi_join(ratings, by = "User-ID")
books <- books %>% semi_join(ratings, by = "ISBN")


# ---------- Create the matrices ---------- #

# similarity matrix - calculate diff between users

# user-item rating matrix - reorganize ratings-df rows = users, cols = books, cells = rating


# ---------- The model ---------- #

# Train a model with holdout (k-fold)


# ---------- Predictions: ---------- #

# get user recommendations for 500 books

# get new book ISBN

# give recommendation - for the user to the new book



