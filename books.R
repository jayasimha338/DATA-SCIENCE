library("recommenderlab")
library(caTools)
book <- read.csv('D:/WORK/RECOMMENDATION SYSTEM/books.csv')
library(tm)

View(book)
class(book)
str(book)

#rating distribution
hist(book$ratings...3.)

#the datatype should be realRatingMatrix inorder to build recommendation
bookmatrix <- as(book[,3:6], 'realRatingMatrix')

#Popularity based ###

bookmodel <- Recommender(bookmatrix, method="POPULAR")
      
#Predictions for all users 
recommended_book <- predict(bookmodel, bookmatrix[414:419], n=5)
as(recommended_book, "list")
## Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering

# User Based Collaborative Filtering(UBCF) ###

bookmodel1 <- Recommender(bookmatrix, method="UBCF")

#Predictions for all users 
recommended_book1 <- predict(bookmodel1, bookmatrix[412:413], n=5)
as(recommended_book1, "list")


