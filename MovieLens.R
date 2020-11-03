#start time ALL, to see how long is processing
startTime_ALL <- Sys.time()

#' ## 1 Introduction  
#' 
#' This project aims to create a movie recommendation system using the MovieLens dat aset. 
#' Information on the available scripts, implementation system and processing time can be found in the appendix at the end

#' ## 2 Method and Analysis  
#' ### 2.1 Download Data and Generate Data Sets 
#' ### 2.1.1 Install packages and load library  
#' First it is necessary to download and install the R packages used in this project
#' 
#2.1.1 Install packages and call library for the project
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(ggthemes))  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(jjb)) install.packages("jjb", repos = "http://cran.us.r-project.org")
if(!require(naniar)) install.packages("naniar", repos = "http://cran.us.r-project.org")


#2.1.1 Load library 
library(tidyverse)
library(lubridate)
library(caret)
library(data.table)
library(dslabs)
library(ggthemes)
library(scales)
library(recosystem)
library(knitr)
library(kableExtra)
library(ggplot2)
library(gridExtra)
library(jjb)
library(naniar)

#make directory figs to save figures
mkdir("figs")

#' ### 2.1.2 Download Data  
#' The data source for the project is a 10M version of the MovieLens data sets 
#' (http://files.grouplens.org/datasets/movielens/ml-10m.zip) 
#'  The 10M MovieLens files are downloaded so that can be used to create the data sets used in the project.


#' ### 2.1.3 Generate Data Sets  

#2.1.2 Download data
#download data
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#2.1.3 Generate datasets
#Split the downloaded MovieLens dataset into 
#edx set 90% and 
#validation set 10% 
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

## if using R 3.6 or earlier
##movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
##                                           title = as.character(title),
##                                           genres = as.character(genres))

# if using R 4.0 or later
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#Split edx set in two sets -
#train_edx with 80% and 
#test_edx with 20% of edx set data

set.seed(1, sample.kind="Rounding")

edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_edx <- edx[-edx_test_index, ]
temp_2 <- edx[edx_test_index, ]

#Make sure userId and movieId in test_edx set are also in train_edx set
test_edx <- temp_2 %>%
  semi_join(train_edx, by = "movieId") %>%
  semi_join(train_edx, by = "userId")

# Add rows removed from test_edx set back into train set
remove <- anti_join(temp_2, test_edx)
train_edx <- rbind(train_edx, remove)

rm(edx_test_index, temp_2, remove)


#' ### 2.2  Exploration and Vizualization  
#' Insight into the basic characteristics and properties of the data 
#' is obtained by inspecting the edx data set.

#head(edx)
kable(edx[1:5, ], caption="edx") %>% kable_styling(latex_options = "hold_position", font_size = 8)

str(edx)

#Classes ‘data.table’ and 'data.frame':	9000055 obs. of  6 variables:
# $ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ movieId  : num  122 185 292 316 329 355 356 362 364 370 ...
# $ rating   : num  5 5 5 5 5 5 5 5 5 5 ...
# $ timestamp: int  838985046 838983525 838983421 838983392 838983392 838984474 838983653 ...
# $ title    : chr  "Boomerang (1992)" "Net, The (1995)" "Outbreak (1995)"  ...
# $ genres   : chr  "Comedy|Romance" "Action|Crime|Thriller" "Action|Drama|Sci-Fi|Thriller" ...


#' ### 2.2.1  About Rating  
#' To see how often a certain rank is used
edx %>% group_by(rating) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=rating, y=count)) + 
  geom_col()+
  xlab("rating") +
  ylab("count") +
  theme(text = element_text(size=9)) + 
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  ggtitle("Rating Count")+
  theme(plot.title = element_text(size = 9))

kable(edx %>% group_by(rating) %>% summarize(n=n()) %>% t(), format.args = list(big.mark = ",", scientific = FALSE) , caption= "Rating count", align = "llllllllll") %>% kable_styling(latex_options = "hold_position", font_size = 8)

#' ### 2.2.2  About MovieId  
format(n_distinct(edx$movieId), big.mark= ',')

#' There are 10,677 different movies in the edx data set 
#' and theirs distribution by number of rankings is shown in the graph below.

#Ratings distribution by movie
edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins=20) +
  ggtitle("Ratings distribution by movie")+
  theme(plot.title = element_text(size = 9)) +
  scale_x_log10() +
  theme(text = element_text(size=9)) +
  xlab("rating count") +
  ylab("movieId count") 


#' ### 2.2.3 About UserId  
format(n_distinct(edx$userId), big.mark= ',')
#' There are 69,878 different users in the edx set. 

#Ratings distribution by user
edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins=20) +
  scale_x_log10() +
  theme(text = element_text(size=9)) +
  xlab("rating count") +
  ylab("userId count") +
  ggtitle("Ratings distribution by user")+
  theme(plot.title = element_text(size = 9))


#' ### 2.2.4  About Year Rated (timestamp)  
#' 
#' The timestamp variable represents the time and date in which the rating was provided

#Mean ratings by year rated
edx %>% mutate(year_rated = year(as_datetime(timestamp))) %>%
  group_by(year_rated) %>%
  summarise(rating =mean(rating)) %>% 
  ggplot(aes(year_rated,rating)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, span = 2/3) +
  theme(text = element_text(size=9)) +
  ggtitle("Mean ratings by year rated") +
  theme(plot.title = element_text(size = 9))

#' ### 2.2.5 About Genres  
n_distinct((edx$genres))

#' The edx data set has 797 distinct genres.
#'  The previously described genre variable in the edx data set has over 
#'  700 different values.

#'  we will split the genres information into multiple row into the genres_edx data set.
genres_edx <- edx %>% separate_rows(genres, sep ="\\|")

table_2 <- tibble(data_set="edx", rows=format(nrow(edx), big.mark= ','))
table_2 <- bind_rows(table_2, tibble(data_set="genres_edx", rows=format(nrow(genres_edx), big.mark= ',')))
print.data.frame(table_2)

#Number of ratings genres
p1 <-  genres_edx %>%
  group_by(genres) %>% 
  summarize(n=n()) %>%
  mutate(genres=reorder(genres,n)) %>%
  ggplot(aes(x=n, y=genres)) +  
  geom_bar(stat = "identity") + 
  ggtitle("Number of ratings genres") +
  theme(plot.title = element_text(size = 7)) +
  xlab("number of ratings") +
  ylab("genres") +
  scale_x_continuous(labels = comma) +
  theme(text = element_text(size=7))

#Distinct movies by genres
p2 <- genres_edx %>%
  group_by(genres) %>% 
  summarize(n=n_distinct(movieId)) %>%
  mutate(genres=reorder(genres,n)) %>%
  ggplot(aes(x=n, y=genres)) +  
  geom_bar(stat = "identity") + 
  ggtitle("Distinct movies by genres") + 
  theme(plot.title = element_text(size = 7)) + 
  xlab("distinct movies") +
  ylab("genres") +
  scale_x_continuous(labels = comma) +
  theme(text = element_text(size=7))

grid.arrange(p1, p2, ncol = 2,  widths=c(1, 1))


#' The following graph shows the average rating per each genres.
png(file="figs/fig_1.png", width=480, height=300)
#average rating per each genres
genres_edx %>%
  group_by(genres) %>% 
  boxplot(rating ~ genres, ., las=2, cex.axis = 0.8) 
dev.off()

include_graphics("figs/fig_1.png", auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE), dpi=NA)

#' ### 2.2.6  About Movie Relase Year  
#' 
#' The title column in the edx data set contains the title of the movie and also the movie release year
##movie release year
movie_relase_year_edx <- edx %>%
  mutate (movie_relase_year=as.numeric(str_sub(title, -5, -2)))

kable(movie_relase_year_edx[1:5, ] ) %>% kable_styling(latex_options = "hold_position", font_size = 8)


#' distribution of the average rating of movies according to the year in which the movie was released.
png(file="figs/fig_2.png", width=480, height=300)
#average rating of movies according to release year
movie_relase_year_edx %>% 
  group_by(movie_relase_year) %>%
  boxplot(rating ~ movie_relase_year, ., las=2, cex.axis = 0.6)
dev.off()

include_graphics("figs/fig_2.png", auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE), dpi=NA)

#Mean ratings by movie relase year
edx %>% mutate (movie_relase_year=as.numeric(str_sub(title, -5, -2))) %>%
  group_by(movie_relase_year) %>%
  summarise(rating = mean(rating)) %>% 
  ggplot(aes(movie_relase_year, rating)) +
  geom_point() +
  geom_smooth(method = 'loess', span = 2/3) +
  ggtitle("Mean ratings by movie relase year") +
  theme(plot.title = element_text(size = 9)) 


#' ### 2.2.7 User - Movie Matrix  

#user - movie rating matrix 
matrix_all_edx <- edx %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>% 
  spread(movieId, rating) %>% as.matrix()


#' The table below shows how much empty data there is in the matrix (matrix_all_edx).
#all
lenght_2 <- length(matrix_all_edx)
#empty
lenght_2_na <- length(matrix_all_edx[is.na(matrix_all_edx)])
#filled
lenght_2_not_na <- length(matrix_all_edx[!is.na(matrix_all_edx)])
table_2_7 <- tibble(data="all", rows=format(lenght_2, big.mark= ','))
table_2_7 <- bind_rows(table_2_7, tibble(data="empty", rows=format(lenght_2_na, big.mark= ',')))
table_2_7 <- bind_rows(table_2_7, tibble(data="filled", rows=format(lenght_2_not_na, big.mark= ',')))
print.data.frame(table_2_7)


#' The figure below shows for 100 randomly selected users and movies 
#' which movie is rated by which user (filled fields) and unrated films are those with empty fields.
png(file="figs/fig_3.png", width=480, height=270)
#user - movie matrix
edx %>% filter(userId %in% sample(unique(edx$userId), 100)) %>%
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="movies", ylab="users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")
#title("user - movie matrix") 

dev.off()

include_graphics("figs/fig_3.png", auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE), dpi=NA)


#' ### 2.3 Preprocessing, Data Cleaning and Prepare   
#preparing data sets
edx <- edx %>% select(userId, movieId, rating, title) %>%
  mutate (movie_relase_year=as.numeric(str_sub(title, -5, -2))) 

train_edx <- train_edx %>% select(userId, movieId, rating, title) %>%
  mutate (movie_relase_year=as.numeric(str_sub(title, -5, -2))) 

test_edx  <- test_edx  %>% select(userId, movieId, rating, title) %>% 
  mutate (movie_relase_year=as.numeric(str_sub(title, -5, -2)))

validation <- validation %>% select(userId, movieId, rating, title) %>% 
  mutate (movie_relase_year=as.numeric(str_sub(title, -5, -2)))


kable(edx[1:5, ], caption='modeling information' ) %>% kable_styling(latex_options = "hold_position", font_size = 8)

table_2_3_1 <- tibble(MovieLens_split="edx", rows=format(nrow(edx), big.mark= ',' ))
table_2_3_1 <- bind_rows(table_2_3_1, tibble(MovieLens_split="validation", rows=format(nrow(validation), big.mark= ',' )))

table_2_3_1b <- tibble(edx_split="train_edx", rows=format(nrow(train_edx), big.mark= ',' ))
table_2_3_1b <- bind_rows(table_2_3_1b, tibble(edx_split="test_edx", rows=format(nrow(test_edx), big.mark= ',' )))

kable( list(table_2_3_1, table_2_3_1b), caption = 'Data sets', booktabs = TRUE, valign = 't')  %>% kable_styling(latex_options = "hold_position", font_size = 8)


#' ### 2.4 Modeling Approach  

#' Each model is trained on "train_edx" and then tested on "test_edx" data 
#' and finally the resulting Root Mean Square Error (RMSE) value of the model is calculated.

#function that calculates RMSE error
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#' ### 2.4.1 Model 1 - Baseline  
#' 
#' Model 1 is based only on ratings data and predicts that all userId will give the same rating to all movieId.

#calculating the mean rating value 
m1_mean <- mean(train_edx$rating)
m1_mean

#result for Model 1 - Baseline
m1_rmse <- RMSE(test_edx$rating, m1_mean)
m1_rmse

#RMSE calculation and listing in the results table
rmse_table <- tibble(Model="1 Baseline", RMSE=RMSE(test_edx$rating, m1_mean))

#print rmse_table
#kable(rmse_table, caption="RMSE results") %>% kable_styling(latex_options = "hold_position", font_size = 8)
kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "HOLD_position", font_size = 8) 


#' ### 2.4.2  Model 2 - Movie Effect  

#' Train Model 2 for movie effect on train_edx data set:
#train model for movie effect 
m2_avgs_movie <- 
  train_edx %>% 
  group_by(movieId) %>% 
  summarize(avgs_movie = mean(rating - m1_mean), .groups = "drop")

#' Test Model 2 with movie effect on test_edx data set,
#test movie effect model
pred_m2_avgs_movie <- m1_mean + test_edx   %>%
  left_join(m2_avgs_movie, by = "movieId") %>%
  .$avgs_movie

#' and calculate RMSE:
#RMSE calculation and listing in the results table
rmse_table <- bind_rows(rmse_table, tibble(Model = "2 Movie Effect", 
                                           RMSE=RMSE(test_edx$rating, pred_m2_avgs_movie)))

#print rmse_table
#kable(rmse_table, caption="RMSE results") %>% kable_styling(latex_options = "hold_position", font_size = 8)
kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "HOLD_position", font_size = 8) 

#' ### 2.4.3 Model 3 – Movie and User Effects  
#train model for movie and user effects 
m3_avgs_user <- 
  train_edx %>% 
  left_join(m2_avgs_movie, by = "movieId") %>%
  group_by(userId) %>% 
  summarize(avgs_user = mean(rating - m1_mean - avgs_movie), .groups = "drop")

#test movie and user effects model
pred_m3_avgs_user <- test_edx %>%
  left_join(m2_avgs_movie, by = "movieId") %>% 
  left_join(m3_avgs_user, by = "userId") %>% 
  mutate(pred_m3 = m1_mean + avgs_movie + avgs_user) %>%
  .$pred_m3

#RMSE calculation and listing in the results table
rmse_table <- bind_rows(rmse_table, tibble(Model = "3 Movie and User Effects", 
                                           RMSE=RMSE(test_edx$rating, pred_m3_avgs_user)))

#print rmse_table
#kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "hold_position", font_size = 8)
kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "HOLD_position", font_size = 8) 

#' ### 2.4.4 Model 4 – Movie, User and Release Year Effects  
#' Train Model 4 for movie, user and release year effects on train_edx data set:
#train model for movie, user and release year effects
m4_avgs_relase <- 
  train_edx %>% 
  left_join(m2_avgs_movie, by = "movieId") %>%
  left_join(m3_avgs_user, by = "userId") %>% 
  group_by(movie_relase_year) %>%
  summarize(avgs_relase = mean(rating - m1_mean - avgs_movie - avgs_user), .groups = "drop")

#test movie, user and release year effects model
pred_m4_avgs_relase <- test_edx %>%
  left_join(m2_avgs_movie, by = "movieId") %>% 
  left_join(m3_avgs_user, by = "userId") %>% 
  left_join(m4_avgs_relase, by = "movie_relase_year") %>% 
  mutate(pred_m4 = m1_mean + avgs_movie + avgs_user+avgs_relase) %>%
  .$pred_m4

#RMSE calculation and listing in the results table
rmse_table <- bind_rows(rmse_table, tibble(Model = "4 Movie, User and Release Year Effects", 
                                           RMSE=RMSE(test_edx$rating,pred_m4_avgs_relase)))

#print rmse_table
#kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "hold_position", font_size = 8)
kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "HOLD_position", font_size = 8) 



#' ### 2.4.5 Model 5 – Regularized Movie and User Effects  

#regularization function with lambda parameter (movie and user effects)
regularization_m_u2 <- function(lambda, train_set, test_set){
  
  #baseline model
  m1_mean <- mean(train_set$rating)
  
  #train movie effect regularized with lambda
  m2_avgs_movie <-
    train_set %>%
    group_by(movieId) %>%
    summarize(avgs_movie = sum(rating - m1_mean)/(n()+lambda), .groups = "drop")
  
  #train user effect regularized with lambda
  m3_avgs_user <-
    train_set %>%
    left_join(m2_avgs_movie, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(avgs_user = sum(rating - m1_mean - avgs_movie)/(n()+lambda), .groups = "drop")
  
  #test regularized movie and user effects model
  pred_m3_avgs_user <- 
    test_set %>%
    left_join(m2_avgs_movie, by = "movieId") %>%
    left_join(m3_avgs_user, by = "userId") %>%
    mutate(pred_m3 = m1_mean + avgs_movie + avgs_user) %>%
    .$pred_m3
  
  #return RMSE for regularized model with lambda parameter
  return(RMSE(pred_m3_avgs_user, test_set$rating))
}



#define a set of lambdas
lambda_set <- seq(0, 10, 0.25)

#calculate RMSE for each lambda
rmses_L <- sapply(lambda_set, regularization_m_u2, 
                  train_set=train_edx, test_set=test_edx)

#take lambda which returns the lowest RMSE, best lambda
lambda <- lambda_set[which.min(rmses_L)]

#train Model 5 using best lambda parameter
#train base lane model
m1_mean <- mean(train_edx$rating)

#train regularized movie effect model
m2_avgs_movie <-
  train_edx %>%
  group_by(movieId) %>%
  summarize(avgs_movie = sum(rating - m1_mean)/(n()+lambda), .groups = "drop")

#train regularized user effect model
m3_avgs_user <-
  train_edx %>%
  left_join(m2_avgs_movie, by = "movieId") %>%
  group_by(userId) %>%
  summarize(avgs_user = sum(rating - m1_mean - avgs_movie)/(n()+lambda), .groups = "drop")

#test regularized model
pred_reg_m3_avgs_user <- test_edx %>%
  left_join(m2_avgs_movie, by = "movieId") %>%
  left_join(m3_avgs_user, by = "userId") %>%
  mutate(pred_m3 = m1_mean + avgs_movie + avgs_user) %>%
  .$pred_m3

#RMSE calculation and listing in the results table
rmse_table <- bind_rows(rmse_table, tibble(Model = "5 Regularized Movie and User Effects", 
                                           RMSE=RMSE(test_edx$rating, pred_reg_m3_avgs_user)))

#print rmse_table
#kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "hold_position", font_size = 8)
kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "HOLD_position", font_size = 8) 



#' ### 2.4.6 Model 6 – Regularized Movie, User and Release Year Effects  

#regularization function with lambda parameter (movie, user, release effects)
regularization_m_u_r4 <- function(lambda, train_set, test_set){
  
  #baseline model
  m1_mean <- mean(train_set$rating)
  
  #train movie effect regularized with lambda
  m2_avgs_movie <-
    train_set %>%
    group_by(movieId) %>%
    summarize(avgs_movie = sum(rating - m1_mean)/(n()+lambda), .groups = "drop")
  
  #train user effect regularized with lambda
  m3_avgs_user <-
    train_set %>%
    left_join(m2_avgs_movie, by = "movieId") %>%
    group_by(userId) %>%
    summarize(avgs_user = sum(rating - m1_mean - avgs_movie)/(n()+lambda), .groups = "drop")
  
  #train release year effect regularized with lambda
  m4_avgs_relase <-
    train_set %>%
    left_join(m2_avgs_movie, by = "movieId") %>%
    left_join(m3_avgs_user, by = "userId") %>%
    group_by(movie_relase_year) %>%
    summarize(avgs_relase = sum(rating - m1_mean - avgs_movie - avgs_user)/(n()+lambda), .groups = "drop")
  
  #test regularized movie, user and release year effects model
  pred_m4_avgs_relase <- test_set %>%
    left_join(m2_avgs_movie, by = "movieId") %>%
    left_join(m3_avgs_user, by = "userId") %>%
    left_join(m4_avgs_relase, by = "movie_relase_year") %>%
    mutate(pred_m4 = m1_mean + avgs_movie + avgs_user+avgs_relase) %>%
    .$pred_m4
  
  #return RMSE for regularized model with lambda parameter
  return(RMSE(pred_m4_avgs_relase, test_set$rating))
}

#define a set of lambdas
lambda_set <- seq(0, 10, 0.25)

#calculate RMSE for each lambda
rmses_L <- sapply(lambda_set, regularization_m_u_r4,
                  train_set=train_edx, test_set=test_edx)


#save best lambda for later, for Final validation
lamda_r_m_u_r <- lambda_set[which.min(rmses_L)]

#take lambda which returns the lowest RMSE, best lambda
lambda <- lambda_set[which.min(rmses_L)]
lambda

#train model using best lambda parameter
#train base lane model
m1_mean <- mean(train_edx$rating)

#train regularized movie effect model
m2_avgs_movie <-
  train_edx %>%
  group_by(movieId) %>%
  summarize(avgs_movie = sum(rating - m1_mean)/(n()+lambda), .groups = "drop")

#train regularized user effect model
m3_avgs_user <-
  train_edx %>%
  left_join(m2_avgs_movie, by = "movieId") %>%
  group_by(userId) %>%
  summarize(avgs_user = sum(rating - m1_mean - avgs_movie)/(n()+lambda), .groups = "drop")

#train regularized release year effect model
m4_avgs_relase <-
  train_edx %>%
  left_join(m2_avgs_movie, by = "movieId") %>%
  left_join(m3_avgs_user, by = "userId") %>%
  group_by(movie_relase_year) %>%
  summarize(avgs_relase = sum(rating - m1_mean - avgs_movie - avgs_user)/(n()+lambda), .groups = "drop")

#test regularized Model 6
pred_reg_m4_avgs_relase <- test_edx %>%
  left_join(m2_avgs_movie, by = "movieId") %>%
  left_join(m3_avgs_user, by = "userId") %>%
  left_join(m4_avgs_relase, by = "movie_relase_year") %>%
  mutate(pred_m4 = m1_mean + avgs_movie + avgs_user+avgs_relase) %>%
  .$pred_m4

#RMSE calculation and listing in the results table
rmse_table <- bind_rows(rmse_table, tibble(Model = "6 Regularized Movie, User and Release Effects",
                                           RMSE=RMSE(test_edx$rating, pred_reg_m4_avgs_relase)))

#print rmse_table
#kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "hold_position", font_size = 8)
kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "HOLD_position", font_size = 8) 



#' ### 2.4.7 Model 7 - Matrix Factorization  
#' For matrix factorization the recosystem package is used. 

#train_edx and test_edx convert to recosystem column format
mf_train_edx <-  with(train_edx, data_memory(user_index = userId,
                                             item_index = movieId,
                                             rating     = rating))

mf_test_edx  <-  with(test_edx,  data_memory(user_index = userId,
                                             item_index = movieId,
                                             rating     = rating))

#create model object
mf_reco <-  recosystem::Reco()

#set seed for randomized values 
#find best tuning parameters
#this can take VERY LONG execution time
set.seed(123, sample.kind = "Rounding")
opts_2 <- mf_reco$tune(mf_train_edx,  opts = list(dim = c(10, 20, 30), 
                                                  lrate = c(0.1, 0.2),
                                                  costp_l2 = c(0.01, 0.1), 
                                                  costq_l2 = c(0.01, 0.1),
                                                  nthread  = 1, niter = 10))

#train model calling train, with best parameters from tune
mf_reco$train(mf_train_edx, opts = c(opts_2$min, nthread = 1, niter = 20))

#test Model 7
pred_mf_reco <-  mf_reco$predict(mf_test_edx, out_memory())
head(pred_mf_reco, 5)

#RMSE calculation and listing in the results table
rmse_table <- bind_rows(rmse_table, tibble(Model = "7 Matrix Factorization", 
                                           RMSE=RMSE(test_edx$rating, pred_mf_reco)))


#print rmse_table
#kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "hold_position", font_size = 8)
kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "HOLD_position", font_size = 8) 


#' ## 3 Results  
#' ### 3.1 Final Validation for Model 6 – Regularized Movie, User and Release Year Effects  

#Model 6 best lambda, lamda_r_m_u_r
lamda_r_m_u_r

#final train Model 6 using best lambda parameter on edx data set
#train base lane model on edx data set
edx_mean <- mean(edx$rating)

#train regularized movie effect model on edx data set 
m2_avgs_movie <-
  edx %>%
  group_by(movieId) %>%
  summarize(avgs_movie = sum(rating - edx_mean)/(n()+lamda_r_m_u_r), .groups = "drop")

#train regularized user effect model on edx data set 
m3_avgs_user <-
  edx %>%
  left_join(m2_avgs_movie, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(avgs_user = sum(rating - edx_mean - avgs_movie)/(n()+lamda_r_m_u_r), .groups = "drop")

#train regularized release year effect model on edx data set 
m4_avgs_relase <-
  edx %>%
  left_join(m2_avgs_movie, by = 'movieId') %>%
  left_join(m3_avgs_user, by = "userId") %>%
  group_by(movie_relase_year) %>%
  summarize(avgs_relase = sum(rating - edx_mean - avgs_movie - avgs_user)/(n()+lamda_r_m_u_r), .groups = "drop")

#test regularized Model 6 on validation data set 
final_pred_reg_m4_avgs_relase <- validation %>%
  left_join(m2_avgs_movie, by = "movieId") %>%
  left_join(m3_avgs_user, by = "userId") %>%
  left_join(m4_avgs_relase, by = "movie_relase_year") %>%
  mutate(pred_m4 = edx_mean + avgs_movie + avgs_user + avgs_relase) %>%
  .$pred_m4

#RMSE calculation and listing in the results table
rmse_table <- bind_rows(rmse_table, tibble(Model = "8 Final Validation for Model 6 Regularization m+u+r",
                                           RMSE=RMSE(validation$rating, final_pred_reg_m4_avgs_relase)))


#print rmse_table
#kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "hold_position", font_size = 8)
kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "HOLD_position", font_size = 8) 


# ### 3.2 Final validation for Model 7 - Matrix Factorization  

#edx and validation data set convert to recosystem format
mf_edx <-  with(edx, data_memory(user_index = userId,
                                 item_index = movieId,
                                 rating     = rating))

mf_validation  <-  with(validation,  data_memory(user_index = userId,
                                                 item_index = movieId,
                                                 rating     = rating))

#create model object
mf_reco_final <-  recosystem::Reco()

#set seed for randomized values
#find best tuning parameters
#this  can take VERY LONG execution time
set.seed(123, sample.kind = "Rounding")
opts_final <- mf_reco_final$tune(mf_edx,  opts = list(dim = c(10, 20, 30), 
                                                      lrate = c(0.1, 0.2),
                                                      costp_l2 = c(0.01, 0.1), 
                                                      costq_l2 = c(0.01, 0.1),
                                                      nthread  = 1, niter = 10))

#train model calling train, with best parameters from tune
mf_reco_final$train(mf_edx, opts = c(opts_final$min, nthread = 1, niter = 20))

#test Model 7 on validation data
pred_mf_reco_final <-  mf_reco_final$predict(mf_validation, out_memory())
head(pred_mf_reco_final, 5)

#RMSE calculation and listing in the results table
rmse_table <- bind_rows(rmse_table, tibble(Model = "9 Final Validation for Model 7 Matrix Factorization", 
                                           RMSE=RMSE(validation$rating, pred_mf_reco_final)))

#print rmse_table
#kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "hold_position", font_size = 8)
kable(rmse_table, caption="RMSE results" ) %>% kable_styling(latex_options = "HOLD_position", font_size = 8) 


#' ### 3.3 Rating prediction for Final Validation  for Model 7 - Matrix Factorization  
#'   
#' With the best model of all previously tested,
#'  a system of recommendations based on matrix factorization, 
#'  a rating prediction will be performed on validation data set. 

#prediction with matrix factorization
mf_prediction <- tibble(userId=validation$userId, movieId=validation$movieId, 
                        title = validation$title, rating=validation$rating, 
                        mf_pred_rating = pred_mf_reco_final) %>%
  arrange(-mf_pred_rating)

#top 5, highest predicted rating value
head(mf_prediction, 5)


#predict for userId 35305, for edx data set movies which that user did not rate

#creating a list of movies from the edx data set that was not rated by userId=35305
#make list with userID=35305, empty ratings ratings=NA, and movies movieId 

#user 35305 rated movies list
edx_35305 <- edx %>% filter(edx$userId==35305)
movieID_35305 <- unique(edx_35305$movieId)

#edx movie list
edx_movieId <- unique(edx$movieId)

#movies which user 35305 did not rate
a_b <- setdiff( unique(edx$movieId), unique(edx_35305$movieId))
predict_movieId <- data.table(movieId=a_b)

#add movie title information
predict_movieId <- left_join(x = predict_movieId, y = edx) 
# Remove duplicate rows
predict_movieId <- predict_movieId %>% distinct(movieId, .keep_all= TRUE)

#set all list with  userId=35305
predict_movieId$userId[predict_movieId$userId >= 1] <- 35305
#set all list with  rating=NA 
predict_movieId <- predict_movieId %>% 
  replace_with_na(replace = list(rating = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)))

#list of non rating movies for userId=35305, rating=NA, movieId, title
edx_movieId_set_35305 <- predict_movieId  

#edx_movieId_set_35305  convert to recosystem format
mf_edx_movieId_set_35305  <-  with(edx_movieId_set_35305,  data_memory(user_index = userId,
                                                                       item_index = movieId,
                                                                       rating     = rating))



#rating prediction for userId 35305
pred_mf_edx_movieId_set_35305 <-  mf_reco_final$predict(mf_edx_movieId_set_35305, out_memory())
head(pred_mf_edx_movieId_set_35305, 5)

mf_prediction_edx_35305 <- tibble(userId=edx_movieId_set_35305$userId, 
                                  movieId=edx_movieId_set_35305$movieId, 
                                  title = edx_movieId_set_35305$title, 
                                  predicted__rating = pred_mf_edx_movieId_set_35305) %>% 
  arrange(-predicted__rating )


#top 5 predicted for userId 35305
head(mf_prediction_edx_35305 , 5)

#last 5 predicted for userId 35305
tail(mf_prediction_edx_35305 , 5)



#' ## 5 Appendix  
#' The project used a Windows 10 computer, with an i3 processor and 8 GB of RAM. 
#' The program code is written in R (version 4.0.2) and RStudio (version 1.3.1073) was used for development.

#' For the described system, 
#' processing time is about 3.5 hours, with the most time-consuming, about 90%, 
#' falling on the parts related to matrix factorization (2.4.7 Model 7 - Matrix Factorization 
#' and 3.2 Final validation for Model 7 Matrix Factorization).  




#end Time ALL
endTime_All <-  Sys.time()


#processin time (start-end)
#start time ALL
startTime_ALL

#end Time ALL
endTime_All

