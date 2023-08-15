library(dplyr)
library(ggplot2)
library(recommenderlab)
library(crayon)
library(tidyverse)


# Load the data from the saved file
load("recommender3.rdata")
# pdf("recommender.pdf")


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

rm(M, books, users)
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

save(ratings, UI.ratings, file = "temp.rdata")
rm(ratings, UI.ratings.n, UI.ratings.n.vec, UI, row.threshold, col.threshold)
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


known.data <- getData(eval_sets, "known")
train.data <- getData(eval_sets, "train")
unknown_data <- as(getData(eval_sets, "unknown"), "matrix")

load("temp.rdata")
save(ratings, UI.ratings, eval_sets, file = "temp.rdata")
rm(ratings, UI.ratings, eval_sets)
gc()


# ---------- functions ---------- #

num_rows <- nrow(known.data)
batch_size <- num_rows / 10
num_batches <- ceiling(num_rows / batch_size)


# my.evaluate a method with n recommendations by splitting the matrix
my.recommender <- function(method, param) {
  system.time(
    eval_recommender <- Recommender(
      data = train.data, method = method, parameter = param
    )
  )
    
  return(eval_recommender)
}


my.evaluate <- function(eval_recommender, n, method, param) {
  all_predictions <- vector("list", num_batches)
  
  start <- Sys.time()
  for (i in 1:num_batches) {
    start_row <- (i - 1) * batch_size + 1
    end_row <- min(i * batch_size, num_rows)
    
    current_batch <- known.data[start_row:end_row, ]
    
    predictions <- predict(
      eval_recommender,
      current_batch,
      n = n,
      type = "ratings"
    )
    
    all_predictions[[i]] <- as(predictions, "matrix")
    
    end <- Sys.time()
    print(paste("method:", method, "n:", n, "batch:", i, "time:", format(end - start)))
    start <- end
  }
  
  final_predictions <- do.call(rbind, all_predictions)
  
  
  # calculate metrics
  TP <- 0
  FP <- 0
  FN <- 0
  TN <- 0
  
  
  rows <- dim(final_predictions)[[1]]
  cols <- dim(final_predictions)[[2]]
  
  for (i in 1:rows) {
    for (j in 1:cols) {
      curr_org <- unknown_data[i,j]
      curr_pred <- final_predictions[i,j]
      
      
      if (is.na(curr_org) || is.na(curr_pred)) {
        next
      }
      
      
      if (curr_org > 5 & curr_pred > 5) {
        TP = TP + 1
      } else if (curr_org <= 5 & curr_pred <= 5) {
        TN = TN + 1
      } else if (curr_org > 5 & curr_pred <= 5) {
        FN = FN + 1
      } else if (curr_org <= 5 & curr_pred > 5) {
        FP = FP + 1
      }
    }
  }
  
  
  TPR <- TP / (TP + FN) # Recall
  FPR <- FP / (FP + TN)
  Precision <- TP / (TP + FP)
  return(list(method, n, param, TPR, FPR, Precision))
}


# evaluate a list of methods for different numbers of books

models_to_evaluate <- list(
  # list(name = "IBCF", param = list(method = "cosine")),
  # list(name = "IBCF", param = list(method = "pearson")),
  list(name = "UBCF", param = list(method = "cosine")),
  list(name = "UBCF", param = list(method = "pearson")),
  list(name = "RANDOM", param = NULL) 
)

n_recommendations <- c(1, 3, 5, 10, 15, 20)

result <- list()

for (model in models_to_evaluate) {
  rec <- my.recommender(model$name, model$param)
  
  for (n in n_recommendations) {
    result <- append(result, my.evaluate(rec, n, model$name, model$param))
  }
}


# create data.frame from the list
col_num <- 6
row_num <- length(result) / col_num

data_matrix <- matrix(result, ncol = col_num, byrow = TRUE)
colnames(data_matrix) <- c("method", "n", "param", "TPR", "FPR", "Precision")

data_df <- as.data.frame(data_matrix, row.names = NULL)
data_df <- lapply(data_df, function(x) c(unlist(x)))


method <- c(data_df$method)
n <- c(data_df$n)
param <- vector("character", length(data_df$TPR))
TPR <- c(data_df$TPR)
FPR <- c(data_df$FPR)
Precision <- c(data_df$Precision)

i <- 0
while (i  < length(data_df$param)) {
  param[i] <- p
  i = i + 1
}

df <- data.frame(
  method = as.factor(method),
  n,
  param = as.factor(param),
  TPR,
  FPR,
  Precision
)

# Draw ROC curve
df %>%
  ggplot(aes(FPR, TPR, colour = method)) +
  geom_line() +
  geom_label(aes(label = n)) +
  labs(title = "ROC curves", colour = "Model") +
  theme_grey(base_size = 14)

# Draw precision / recall curve
df %>%
  ggplot(aes(TPR, Precision, colour = method)) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "Precision-Recall curves", colour = "Model") +
  theme_grey(base_size = 14)



# ---------- Predictions (for all users): ---------- #

# get user recommendations for 500 books

# get new book ISBN

# give recommendation - for the user to the new book
# take a random user and run:
# getList(predict_result)



# get the titles of the books

dev.off()




