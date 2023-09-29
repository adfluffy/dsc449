# Question 1
9*(5-4-3+2)==0
# By placing the parentheses around (5-4-3+2) we are able to create a term with
# a value of 0. Then multiplying 9*0 results in 0


# Question 2
setwd("C:/Users/devan/Downloads") #sets working directory to the downloads folder
getwd() #confirms prior code execution was successful


#Question 3
# The below code creates vector with top 10 passing yards from the website
# http://www.nfl.com/stats/categorystats?archive=false&conference=null&statisticCategory=PASSING&season=2022&seasonType=REG&experience=&tabSeq=0&qualified=false&Submit=Go 
passingyards <- c(5250,4739,4694,4547,4475,4438,4283,4282,4113,3701)
print(passingyards) #confirms prior execution is correct

yardspermeter <- 0.9144 #Number of yards per meter (yard/meter)
# The following code takes the product of vector passingyards and scalar yardspermeter 
# to convert yards to meters. New values are stored in the vector passingmeters
passingmeters <- passingyards*yardspermeter 
print(passingmeters) #confirms prior execution is correct


#Question 4
# Following code creates a vector comprised of the 12 months of the year
months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
print(months)#confirms execution is correct

# Following code creates a vector comprised of the all integers from 1 to 360 (inclusive)
onetothreesixty <- seq(from=1,to=360,by=1)
print(onetothreesixty)#confirms execution is correct

# Following code creates a vector comprised of sequential months starting at September and ending after the 360th occurrence
somanymonths <- rep(x=months[c(9:12,1:8)],length.out=360)
print(somanymonths) #confirms execution is correct


#Question 5
ouncespergallon <- 128 #sets the ounces per gallon value to be used later in the question
portions <- 10 #sets the number of desired portions of milk

# A) Integer Values Only
milkremainder <- ouncespergallon %% portions
# Above code takes the modulo of the ounces of milk per gallon divided by the number of portions desired, 
# resulting in the remaining unused milk portion

ouncesmilkperperson_a <- (ouncespergallon-milkremainder)/portions
# Above code subtracts the milk remainder from the number of ounces of milk per gallon, 
# which is then divided into the desired portions and stored as ouncesmilkperperson_a 

# B) Exhaust all milk available
ouncesmilkperperson_b <- ouncespergallon/portions
# Above code divides the ounces of milk per gallon by the number of desired portions
# and stores that value as ouncesmilkperperson_b


#Question 6
install.packages('rgdal')
library(rgdal)
# Above code loads in the rgdal package (as well as the sp package due to dependency)
# then loads in the library to the session (again, as well as sp)


#Question 7
averagepassingyards <- sum(passingyards)/length(passingyards)
# Above code sums the values fo the passingyards vector and then devides that value 
# by the number of values within the vector. This results in the average value of the
# passingyards vector. 

averagepassingmeters <- averagepassingyards*yardspermeter
# Above code converts the average of the passingyards vector from yards to meters, calling
# the previously set yardspermeter variable. The average passing distance, now in meters,
# is stored as averagepassingmeters


#Question 8
course_data <- c("ECO 550","ECO 581","ECO 514","ECO 321","ECO 488","ECO 489","ECO 190","ECO 442","ECO 385")
# Above code creates a vector containing the 9 most recent courses I've taken

course_matrix <- matrix(course_data,nrow=3,ncol=3,byrow=TRUE)
print(course_matrix)
# Above code creates a 3x3 matrix using the information in the course_data vector. That
# Information is input into the matrix incrementing the column value until a row is 
# full and then advancing the row value while resetting the column value back to 1

course_matrix_transpose <- matrix(course_data,nrow=3,ncol=3,byrow=FALSE)
print(course_matrix_transpose)
# Above code creates a 3x3 matrix using the information in the course_data vector. That
# Information is input into the matrix incrementing the row value until a column is 
# full and then advancing the column value while resetting the row value back to 1. This
# results in a transposed version of the course_matrix matrix.


#Question 9
course_grades <- c(3.00,3.67,3.33,4.00,4.00,3.33,4.00,4.00,2.00)
# Above code creates a vector with grade values for each course listed in the course_data vector

course_array <- array(data=c(course_data,course_grades),c(3,3,2))
# Above code combines the course_data and course_grades vecotors and arranges them into a
# 3x3x2 array
print(course_array[1,,]) #allows me to confirm that the courses/grades properly aligned


#Question 10
months[5]
months[6]
months[5]<months[6]
# Above code checks if the 5th entry in the months vector (May, confirmed by months[5]) 
# is less than the 6th entry in the months vector (June, confirmed bu months[6]). 
# Result of code execution is FALSE

months_matrix <- matrix(c(months,1:12),nrow=2,ncol=12,byrow=TRUE)
months_matrix[2,5]<months_matrix[2,6]
# Above code converts the months vector into a matrix, where row 1 is the name of
# the month in english and row 2 is the order that the month occurs in a year.
# By then comparing the 2nd row values for both May and June we can determine that
# May does come before June in the year while keeping the order of the original vector.
# A TRUE value is returned. 