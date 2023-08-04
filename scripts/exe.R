library(dplyr)
library(ggplot2)
#library(knitr)
library(recommenderlab)
library(crayon)


# Load the data from the saved file
load("recommender3.rdata")
# pdf("recommender.pdf")


# ---------- Functions Definition ---------- #

split_predict <- function(eval_recommender, eval_sets, items_to_recommend = 10) {
  
  normalized.data <- normalize(newData, method = model$normalize)
  
  similarity_matrix <- similarity(newdata, model$data, method = model$method,
                                  min_matching = model$min_matching_items,
                                  min_predictive = model$min_predictive_items
  )
  
  
  
}


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

rm(M, books)
gc()


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

hist(UI.ratings.n.vec, main = "Histogram of Normalized Ratings", xlab = "Normalized Rating")

rm(UI.ratings.n, UI.ratings.n.vec, UI, row.threshold, col.threshold)
gc()


# ---------- The model ---------- #

# Train a model with holdout (k-fold)
eval_sets <- evaluationScheme(
  data = UI.ratings, 
  method = "cross-validation",
  train = 0.8, 
  given = -1,
  goodRating = 6, 
  k = 5
)

# User Based
system.time(
  eval_recommender <- Recommender(
    data = getData(eval_sets, "train"),
    method = "UBCF", parameter = NULL
  )
)

model <- eval_recommender@model 
sim_mtx <- similarity(
  getData(eval_sets, "known"), 
  model$data, 
  method = model$method,
  min_matching = model$min_matching_items, 
  min_predictive = model$min_predictive_items
)

known.data <- getData(eval_sets, "known")

num_rows <- nrow(known.data)
batch_size <- num_rows / 10
num_batches <- ceiling(num_rows / batch_size)

all_predictions <- vector("list", num_batches)
start <- Sys.time()

for (i in 1:num_batches) {
  start_row <- (i - 1) * batch_size + 1
  end_row <- min(i * batch_size, num_rows)

  current_batch <- known.data[start_row:end_row, ]

  predictions <- predict(
    eval_recommender,
    current_batch,
    n = 5,
    type = "ratings"
  )

  # all_predictions[[i]] <- as(predictions, "matrix")
  all_predictions[[i]] <- predictions

  end <- Sys.time()
  print(paste("done for batch:", i, "in", format(end - start)))
  start <- end
}

# final_predictions <- do.call(rbind, all_predictions)
final_predictions <- Reduce("+", all_predictions)


# calcPredictionAccuracy myself 

















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


save(eval_accuracy, eval_prediction, eval_recommender, UI.ratings, ratings, file = "recommender3.2.rdata")
rm(eval_accuracy, eval_prediction, eval_recommender, UI.ratings, ratings)
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


# evaluate some models
models_to_evaluate <- list(IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
                           IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
                           UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
                           UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
                           random   = list(name = "RANDOM", param = NULL) 
)

n_recommendations <- c(1, 3, 5, 10, 15, 20)
results <- evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)

# Draw ROC curve
plot(results, y = "ROC", annotate = 1, legend="topleft")
title("ROC Curve")

# Draw precision / recall curve
plot(results, y = "prec/rec", annotate=1)
title("Precision-Recall")


dev.off()


# check the model by confusion matrix????????????
# show the confusion matrix from recommender lab - sum = num of non zero ratings
# roc - reciver operating rate, calc from the matrix



# get new book ISBN

# give recommendation - for the user to the new book
# take a random user and run:
# getList(predict_result)



# get the titles of the books

