# Loads in the sqldf library to run later SQL Code
library(sqldf)
# Imports the csv data for use in answering the assigned questions
Sites <- read.csv(file = "F:/Users/Devan/Documents/Education/DSC449/Assignments/Assignment 4/giffords.csv", header=TRUE)
Customers <- read.csv(file = "F:/Users/Devan/Documents/Education/DSC449/Assignments/Assignment 4/giffordscust.csv", header=TRUE)
Flavor <- read.csv(file = "F:/Users/Devan/Documents/Education/DSC449/Assignments/Assignment 4/giffordsice.csv", header=TRUE)
Toppings <- read.csv(file = "F:/Users/Devan/Documents/Education/DSC449/Assignments/Assignment 4/giffordstop.csv", header=TRUE)
Orders <- read.csv(file = "F:/Users/Devan/Documents/Education/DSC449/Assignments/Assignment 4/giffordsorder.csv", header=TRUE)

# Q1: Determines that each of the 500 orders has a first scoop of dessert
step1 <- "
SELECT Scoop1
FROM Orders
WHERE Scoop1 != ''
"
sqldf(step1)

# Q2: Determines the descending order of the sites relative to number of sales by site name
step2 <- "
SELECT DISTINCT Sites.City AS Location, COUNT(Orders.Site) AS Sales
FROM Sites, Orders
WHERE Sites.ID = Orders.Site
GROUP BY Location
ORDER BY Sales DESC
"
sqldf(step2)

# Q3: Finds the number of purchases each specific customer made by customer name
step3 <- "
SELECT Cust.Fname AS First, Cust.Lname AS Last, COUNT(Orders.Customer) AS Purchases
FROM Customers AS Cust, Orders
Where Cust.ID = Orders.Customer
GROUP BY First, Last
ORDER BY Last
"
sqldf(step3)

# Q4: Shows the ice creams and toppings problematic for a person with a nut allergy
step4 <- "
SELECT Flavor AS Hazard
FROM Flavor
WHERE Nuts = 'Y'

UNION

SELECT Name
FROM Toppings
WHERE Nuts = 'Y'
"
sqldf(step4)

# Q5: Finds every order that counts as a sundae (has at least one topping)
step5 <- "
SELECT ID AS SundaeOrders
FROM Orders
WHERE NOT(Topping1 = '' AND Topping2 = '' AND Topping3 = '')
"
sqldf(step5)

# Q6: 6.	Find all customers by name that are not from ME or NH
step6 <- "
SELECT Fname as First, Lname AS Last, State
FROM Customers
WHERE State != 'ME' AND State != 'NH'
"
sqldf(step6)

# Q7: Finds the number of types of desserts that there are and how many of each type
step7 <- "
SELECT Type, COUNT(*) AS Frequency
FROM Flavor
WHERE Type = Type
GROUP BY Type
"
sqldf(step7)

# Q8: Lists out all of the possible orders consisting only of ice cream (Type = 'I', no toppings)
step8 <- "
SELECT Flavor1.Flavor AS Scoop1, NULL AS Scoop2, NULL AS Scoop3
FROM Flavor AS Flavor1
WHERE Flavor1.Type = 'I'

UNION

SELECT Flavor1.Flavor AS Scoop1, Flavor2.Flavor AS Scoop2, NULL AS Scoop3
FROM Flavor AS Flavor1, Flavor AS Flavor2
WHERE Flavor1.Type = 'I' AND Flavor2.Type = 'I'

UNION

SELECT Flavor1.Flavor AS Scoop1, Flavor2.Flavor AS Scoop2, Flavor3.Flavor AS Scoop3
FROM Flavor AS Flavor1, Flavor AS Flavor2, Flavor AS Flavor3
WHERE Flavor1.Type = 'I' AND Flavor2.Type = 'I' AND Flavor3.Type = 'I'
"
sqldf(step8)

# Q9: Finds the most frequently ordered flavor
step9 <- "
SELECT Flavor.Flavor, COUNT(*) AS OrderCount
FROM Flavor, Orders
WHERE (Flavor.ID = Orders.Scoop1) OR (Flavor.ID = Orders.Scoop2) OR (Flavor.ID = Orders.Scoop3)
GROUP BY Flavor.ID
ORDER BY OrderCount DESC
LIMIT 1
"
sqldf(step9)

# Q10: Finds all towns customers came from that do not have site in them.
step10 <- "
SELECT City
FROM Customers

EXCEPT

SELECT City
FROM Sites
"
sqldf(step10)
