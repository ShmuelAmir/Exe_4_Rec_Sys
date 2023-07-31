library(dplyr)
library(stringr)


load("recommender3.rdata")

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
# ratings <- ratings %>% 
#   mutate(`Book-Rating` = if_else(`Book-Rating` == 0, NA, `Book-Rating`)) # maybe remove because the calculations

# remove duplicate rows
ratings <- ratings %>% distinct(`User-ID`, `ISBN`, .keep_all = TRUE)

save(books, ratings, file = "recommender3.rdata")

rm(users, ratings, books, users_num_ratings)
gc()



