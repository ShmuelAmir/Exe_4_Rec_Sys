library(dplyr)
library(ggplot2)
#library(knitr)
library(recommenderlab)
library(stringr)
library(crayon)


# Load the data from the saved file
load("recommender3.rdata")

# 0 - unread book
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

slotNames(UI)
head(names(colCounts(UI)))

vector_ratings <- as.vector(UI@data)

# remove zeros
vector_ratings <- vector_ratings[vector_ratings != 0]
table(vector_ratings)
hist(vector_ratings, main="Histogram of Ratings", xlab="Rating Value")


ratings <- UI[rowCounts(UI) > 50, colCounts(UI) > 100]
dim(ratings)


#  -----------------return 5 ISBN books for each user - example on small data
# train <- UI[1:1000]
# rec <- Recommender(train, method = "UBCF")
# pre <- predict(rec, UI[1001:1002], n = 5)
# 
# similarity(UI[1:50,], method = "cosine", which = "users", min_matching = 10)
# 
# scheme <- evaluationScheme(
#   UI[1:1000], 
#   method = "cross-validation", 
#   k = 10, 
#   given = -5, 
#   goodRating = 7)






#  ----------------- from the lab
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
given <- 2
goodRating <- 5 
k <- 1           

eval_sets <- evaluationScheme(
  data = UI, method = "split",
  train = percent_train, given = given,
  goodRating = goodRating, k = k
)

# save(eval_sets, UI, M, nonzero.rows, R, ratings, file = "recommender3.rdata")

# example from a tutorial
# model_train_scheme <- UI %>%
#   evaluationScheme(method = 'split', 
#                    train = 0.8, 
#                    given = 2,
#                    goodRating = 6,
#                    k = 1)
# 
# model_params <- list(method = "cosine",
#                      nn = 10, # find each user's 10 most similar users.
#                      sample = FALSE, # already did this.
#                      normalize = "center")
# 
# model1 <- getData(model_train_scheme, "train") %>% 
#   Recommender(method = "UBCF", parameter = model_params)
# 
# model1_pred <- predict(model1, getData(model_train_scheme, "known"), type = "ratings")


# chatGPT


batch_size <- 505

num_rows <- nrow(UI)
num_batches <- ceiling(num_rows / batch_size)

all_predictions <- vector("list", num_batches)
start <- Sys.time()
print(num_batches)

for (i in 1:num_batches) {
  start_row <- (i - 1) * batch_size + 1
  end_row <- min(i * batch_size, num_rows)
  
  current_batch <- UI[start_row:end_row, ]
  
  model <- Recommender(current_batch, method = "UBCF")
  
  predictions <- predict(model, current_batch)
  
  all_predictions[[i]] <- predictions
  
  end <- Sys.time()
  print(paste("done for batch: ", i, "in ", end - start))
  start <- end
}

final_predictions <- do.call(rbind, all_predictions)



















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

# U <- similarity(UI[1:4,], method = "cosine", which = "users")
U <- similarity(normalize_data, model$data, model$method,
                min_matching = model$min_matching_items,
                min_predictive = model$min_predictive_items)
  
# I <- similarity(
#     newdata, model$data, method = model$method, min_matching = model$min_matching_items, 
#     min_predictive = model$min_predictive_items
# )
  

# check the model by confusion matrix????????????
# show the confusion matrix from recommender lab - sum = num of non zero ratings
# roc - reciver operating rate, calc from the matrix - לשחק עם החותך מה נחשב טוב ומה רע
#    המהתנה goodRating  <- 5 

# ---------- Predictions (for all users): ---------- # לעשות בחלקים לפי חלקים של מטריצת הדמיון

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









