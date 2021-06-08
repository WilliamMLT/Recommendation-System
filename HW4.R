```{r}
rm(list=ls())
#set working directory
# setwd("~/uc_davis/BAX_401/Class_7")
data <- readxl::read_xlsx('HW4_Data.xlsx',
                          sheet='Combined Sheet',
                          range='A1:AY188',
                          col_names=T)
```
```{r}
column_names <- data$Combined_Name
data <- data.frame(t(data[,-1]))
colnames(data) <- column_names
data[is.na(data)] <- 0
```
```{r}
# normalizes a vector
normalize <- function (v) {
  avg <- mean(subset(v, v > 0))
  for (i in 1:length(v)) {
    if (v[i] > 0) {
      v[i] <- v[i] - avg
    }
  }
  return(v)
}
```
```{r}
# returns cosine similarity of two vectors
cosinesim <- function(a, b) {
  #a %*% b / sqrt(a %*% a * b %*% b)
  denom <- sqrt(a %*% a * b %*% b)
  if (denom == 0){
    return(0)
  } else {
    a %*% b / denom
  }
}
# returns vector of cosines between user and all other users
user_user_cosines <- function(user, df) {
  cols <- colnames(df)
  cosines <- vector()
  for (i in cols) {
    if (i != user) {
      res <- cosinesim(df[[user]], df[[i]])
      cosines <- c(cosines, res)
    }
  }
  return(cosines)
}
# creates vector of predictions for user for every movie
find_rating <- function(user, df, normalize = FALSE) {
  
  avg = 0
  # normalize data frame if needed
  if (normalize == TRUE) {
    avg <- mean(subset(df[[user]], df[[user]] > 0))
    df <- data.frame(lapply(df, normalize)) 
  }
  
  # create list of other users
  cols <- colnames(df)
  cols <- cols[!cols %in% user]
  
  ratings <- vector()
  len <- length(df[[user]])
  for (i in 1:len){
    other_ratings <- vector()
    cosines <- vector()
    for (col in cols) {
      # create vector of nonuser ratings
      res <- df[[col]][i]
      other_ratings <- c(other_ratings, res)
      
      # create vector of cosines between user and all other users
      res <- cosinesim(df[[user]], df[[col]])
      cosines <- c(cosines, res)
    }  
    rating <- avg + (other_ratings %*% cosines / sum(abs(cosines)))
    ratings <- c(ratings, rating)
  }
  return(ratings)
}
find_rating('ShachiGovil', data, normalize = T)
```
# Original Data User-User
# predict movie ratings without normalization
```{r}
find_rating('ShachiGovil', data)
```
# User-User Mean Centered
# predict movie ratings with normalization
```{r}
shachi <- find_rating('ShachiGovil', data, normalize = T)
```
amy <- find_rating('AmyRussell', data, normalize = T)
find_rating('AmyRussell', data)
```
camille <- find_rating('CamilleMack', data, normalize = T)
find_rating('CamilleMack', data)

shachi[7]
shachi[12]
shachi[35]
amy[7]
amy[12]
amy[35]
camille[7]
camille[12]
camille[35]



# predict movie ratings without normalization
shachi_1 <- find_rating('ShachiGovil', data)
amy_1 <- find_rating('AmyRussell', data)
camille_1 <- find_rating('CamilleMack', data)

shachi_1[7]
shachi_1[12]
shachi_1[35]
amy_1[7]
amy_1[12]
amy_1[35]
camille_1[7]
camille_1[12]
camille_1[35]


data.frame(shachi = c(shachi_1[7], shachi_1[12], shachi_1[35]), 
           amy = c(amy_1[7], amy_1[12], amy_1[35]), 
           camille = c(camille_1[7], camille_1[12], camille_1[35]))

shachi_1[35]
amy_1[7]
amy_1[12]
amy_1[35]
camille_1[7]
camille_1[12]
camille_1[35]