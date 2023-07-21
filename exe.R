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

rm(mysqlconnection)
gc()


# ---------- Clean the data ---------- #

# remove unneeded columns
books <- books %>%
  select(-starts_with("Image"))

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

ratings$`User-ID` <- as.factor(ratings$`User-ID`)
ratings$ISBN <- as.factor(ratings$ISBN)

rm(users, books)
gc()


# ---------- Create the matrices ---------- #

# user-item rating matrix - reorganize ratings-df rows = users, cols = books, cells = rating
M <- sparseMatrix(
  i = as.integer(ratings$`User-ID`),
  j = as.integer(ratings$ISBN),
  x = ratings$`Book-Rating`,
  dims = c(nlevels(ratings$`User-ID`), nlevels(ratings$ISBN)),
  dimnames = list(levels(ratings$`User-ID`), levels(ratings$ISBN)),
)

UI <- new("realRatingMatrix", data = M)

rm(M)
gc()




train <- UI[1:1000]
rec <- Recommender(train, method = "UBCF")
pre <- predict(rec, UI[1001:1002], n = 5)




















# R <- rle(colSums(M>0))
# 
# thresh <- 3
# S <- sort(R$lengths[R$values > thresh], decreasing = TRUE)
# 
# run <- S[1]
# loc <- grep(names(S[1]), colnames(M))
# 
# nonzero.rows <- unique(sort(as.vector(
#   apply(M[, (loc-run):(loc-1)], MARGIN = 2, FUN = function(e) which(e>0))
# )))
# 
# M[nonzero.rows, (loc-run):(loc-1)]
# 
# ratings.n <- normalize(UI)
# ratings.n.vec <- as.vector(ratings.n@data)


# ---------- The model ---------- #

# Train a model with holdout (k-fold)
percent_train <- 0.8
items_to_keep <- 2
rating_threshold <- 6 # 3
n_eval <- 5           # k

eval_sets <- evaluationScheme(
  data = UI, method = "split",
  train = percent_train, given = items_to_keep,
  goodRating = rating_threshold, k = n_eval
)

# save(eval_sets, UI, M, nonzero.rows, R, ratings, file = "recommender3.rdata")


# User Based

system.time(
  eval_recommender <- Recommender(
    data = getData(eval_sets, "train"),
    method = "UBCF", parameter = NULL
  )
)



# Item Based

# system.time(
#   eval_recommender <- Recommender(
#     data = getData(eval_sets, "train"),
#     method = "IBCF", parameter = NULL # method = IBCF
#   )
# )


# similarity matrix - calculate diff between users שתי מטריצות אחת למשתמשים ואחת לספרים
model <- eval_recommender@model
# model$weighted <- FALSE

normalize_data <- normalize(getData(eval_sets, "known"), method = model$normalize)

U <- similarity(UI[1:4,], method = "cosine", which = "users")
# U <- similarity(normalize_data, model$data, model$method, 
#                 min_matching = model$min_matching_items, 
#                 min_predictive = model$min_predictive_items)
  
# I <- similarity(
#     newdata, model$data, method = model$method, min_matching = model$min_matching_items, 
#     min_predictive = model$min_predictive_items
# )
  


# ---------- Predictions: ---------- # לעשות בחלקים לפי חלקים של מטריצת הדמיון

items_to_recommend <- 10
system.time(
  eval_prediction <- predict(
    object = eval_recommender,
    newdata = getData(eval_sets, "known"),
    n = items_to_recommend,
    type = "ratings"
  )
)

# get user recommendations for 500 books

# get new book ISBN

# give recommendation - for the user to the new book









