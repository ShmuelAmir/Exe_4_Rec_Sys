library(dplyr)
library(ggplot2)
#library(knitr)
library(recommenderlab)
library(stringr)
library(crayon)


# Load the data from the saved file
load("recommender3.rdata")
# pdf("recommender.pdf")


# ---------- Clean the data ---------- #

# remove unneeded columns
books <- books %>%
  select(-starts_with("Image"))

# remove non valid ISBN
books <- books %>%
  mutate(ISBN = gsub("[^0-9]", "", ISBN)) %>%
  filter(str_length(ISBN) == 10)

ratings <- ratings %>%
  mutate(ISBN = gsub("[^0-9]", "", ISBN)) %>%
  filter(str_length(ISBN) == 10)

# remove users who don't rated enough
users_num_ratings <- ratings %>%
  group_by(`User-ID`) %>%
  summarise(bin = n()) %>%
  filter(bin > 20)

ratings <- ratings %>% semi_join(users_num_ratings, by = "User-ID")

# get only the users, books, and ratings that have connections
ratings <- ratings %>% semi_join(books, by = "ISBN")
users <- users %>% semi_join(ratings, by = "User-ID")
books <- books %>% semi_join(ratings, by = "ISBN")

ratings$`User-ID` <- as.factor(ratings$`User-ID`)
ratings$ISBN <- as.factor(ratings$ISBN)
ratings <- ratings %>% mutate(`Book-Rating` = if_else(`Book-Rating` == 0, NA, `Book-Rating`)) # maybe remove because the calculations

# remove duplicate rows
ratings <- ratings %>% distinct(`User-ID`, `ISBN`, .keep_all = TRUE)

rm(users, users_num_ratings)
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

# 
# vector_ratings <- as.vector(UI@data)
# 
# # remove zeros
# vector_ratings <- vector_ratings[vector_ratings != 0]
# table(vector_ratings)
# hist(vector_ratings, main="Histogram of Ratings", xlab="Rating Value")


# removing less than 3 quantile
row.threshold <- quantile(rowCounts(UI))[[2]]
col.threshold <- quantile(colCounts(UI))[[4]]
UI.ratings <- UI[
  rowCounts(UI) >= row.threshold, 
  colCounts(UI) >= col.threshold
]
dim(UI.ratings@data)

# normalize the data
UI.ratings.n <- normalize(UI.ratings)
UI.ratings.n.vec <- as.vector(UI.ratings.n@data)
UI.ratings.n.vec <- UI.ratings.n.vec[UI.ratings.n.vec != 0]

hist(UI.ratings.n.vec, main = "Histogram of Normalized Ratings", xlab = "Rating")


# ---------- The model ---------- #

# Train a model with holdout (k-fold)
percent_train <- 0.8
given <- 15
goodRating <- 5 # check maybe another number
k <- 1           

eval_sets <- evaluationScheme(
  data = UI.ratings, method = "split",
  train = percent_train, given = given,
  goodRating = goodRating, k = k
)

# User Based
system.time(
  eval_recommender <- Recommender(
    data = getData(eval_sets, "train"),
    method = "UBCF", parameter = NULL
  )
)

# ---------- Predictions (for all users): ---------- #

# get user recommendations for 500 books
items_to_recommend <- 10
system.time(
  eval_prediction <- predict(
    object = eval_recommender,
    newdata = getData(eval_sets, "known"),
    n = items_to_recommend,
    type = "ratings"
  )
)


eval_accuracy <- calcPredictionAccuracy(x = eval_prediction,
                                        data = getData(eval_sets, "unknown"),
                                        byUser = TRUE)
head(eval_accuracy)

save(eval_accuracy, eval_prediction, eval_recommender, UI.ratings, ratings, file = "recommender3.rdata")
rm(eval_accuracy, eval_prediction, eval_recommender, UI.ratings, ratings)
rm(UI.ratings.n, UI.ratings.n.vec, UI)
gc()


# Item Based

system.time(
  item_eval_recommender <- Recommender(
    data = getData(eval_sets, "train"),
    method = "IBCF", parameter = NULL
  )
)

system.time(
  item_eval_prediction <- predict(
    object = item_eval_recommender,
    newdata = getData(eval_sets, "known"),
    n = items_to_recommend,
    type = "ratings"
  )
)

item_eval_accuracy <- calcPredictionAccuracy(x = item_eval_prediction,
                                        data = getData(eval_sets, "unknown"),
                                        byUser = TRUE)
head(item_eval_accuracy)




# check the model by confusion matrix????????????
# show the confusion matrix from recommender lab - sum = num of non zero ratings
# roc - reciver operating rate, calc from the matrix



# get new book ISBN

# give recommendation - for the user to the new book




# get the titles of the books

