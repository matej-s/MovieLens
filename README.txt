REEADME.txt

This project contains scripts for the capstone project of HarvardX Data Science Professional Certificate program.
This project aims to create a movie recommendation system using the MovieLens data set.

MovieLens.R - R code used to evaluate data set and build machine learning models.
MovieLens_report.Rmd - R Markdown script used to create the PDF report.
MovieLens_report.pdf - pdf report document, result of processing Rmd script.


Project related documentation (MovieLens.R, MovieLens_report.Rmd, MovieLens_report.pdf) can be accessed on the GitHub  https://github.com/matej-s/MovieLens . The MovieLens data set used in the project were downloaded from the https://grouplens.org/datasets/movielens/10m/ .
The project used a Windows 10 computer with an i3 processor and 8 GB of RAM. The program code is written in R (version 4.0.2) and RStudio (version 1.3.1073) was used for development. For the described system, generating a pdf document (MovieLens_report.pdf) using the MovieLens_report.Rmd script takes about 3.5 hours, with the most time-consuming, about 90%, falling on the parts related to matrix factorization (2.4.7 Model 7 - Matrix Factorization and 3.2 Final validation for Model 7 Matrix Factorization). 


Table of Contents
#1 Introdution

#2 Method and Analysis

#2.1 Download data and generate datasets 
#2.1.1 Install packages and load library
#2.1.2 Download data 
#2.1.3 Generate datasets

#2.2  Exploration and vizualization
# 2.2.1  About rating
# 2.2.2  About moveId
# 2.2.3  About userId
# 2.2.4  About year rated
# 2.2.5  About genres
# 2.2.6  About movie relase year
# 2.2.7  User-Movie Matrix

#2.3 Preprocessing, Data Cleaning and Prepare

#2.4 Modeling approach
#2.4.1 Model 1 - Baseline
#2.4.2 Model 2 - Movie Effect 
#2.4.3 Model 3 – Movie and User Effects
#2.4.4 Model 4 – Movie, User and Release Year Effects
#2.4.5 Model 5 – Regularized Movie and User Effects
#2.4.6 Model 6 – Regularized Movie, User and Release Year Effects
#2.4.7 Model 7 - Factorization
  
#3   Results 
#3.1 Final Validation  for model 6 Regularization  (movie + user + relase)
#3.2 Final Validation  for model 7
#3.3 Rating prediction for Final Validation  for model 7 Factorization

#4 Conclusion

#5 Appendix



