# Part 1

# Initializes the data frames for the color sensing data and prompts user to select files
color_calb <- read.csv(file.choose(),header=TRUE)
rgb_dat <- read.csv(file.choose(),header=TRUE)

# Below code creates the function modelCompare to determine which models passed have
# the highest values for each term of interest
modelCompare <- function(summ.1,summ.2,summ.3=NULL,colorSet){
  # determines if the summ.3 arguement is defaulted
  if (is.null(summ.3)){
    # If summ.3 is left to the default value of NULL, instead makes it a duplicate
    # of summ.2
    summ.3 <- summ.2
    # Sets the summary.counts variable to 2 for later use
    summary.counts <- 2
  } else {
    # if summ.3 is NOT the default NULL value, sets the summary.counts variable to 3
    summary.counts <- 3
  }
  # Determines the number of rows the function will need to iterate over
  iterations <- round(length(summ.1)/11,0)
  # Initialized the reportlist data frame. This will be used to return values
  reportlist<- data.frame(matrix(rep(0),nrow=iterations,ncol=9))
  
  # Main body of the function. Iterates from 1 to 'iterations'
  for (i in 1:iterations){
    # Determines how to split the summ.X lists passed to function
    partioner <- (i-1)*11
    # Creates the upper and lower bounds for each section of summ.X
    upperB <- 11 + partioner
    lowerB <- 1 + partioner
    # Takes color i from colorSet arguement for reporting
    colorI <- colorSet[i]
    # (Re)initializes the best.list vector used in determining the best performing regression
    best.list <- c("")
    # Partitions the summ.X arguements into temporary variables for compairison
    summ.1.part <- summ.1[lowerB:upperB]
    summ.2.part <- summ.2[lowerB:upperB]
    summ.3.part <- summ.3[lowerB:upperB]
    # Creates vectors with values for intercept and independent variable t scores, r squared
    # and adjusted r squared values for each partition of summ.X arguements
    summ.intercept.t <- c(summ.1.part$coefficients[1,3],summ.2.part$coefficients[1,3],summ.3.part$coefficients[1,3])
    summ.independent.t <- c(summ.1.part$coefficients[2,3],summ.2.part$coefficients[2,3],summ.3.part$coefficients[2,3])
    summ.r.squared <- c(summ.1.part$r.squared,summ.2.part$r.squared,summ.3.part$r.squared)
    summ.adj.r.squared <- c(summ.1.part$adj.r.squared,summ.2.part$adj.r.squared,summ.3.part$adj.r.squared)
    # Determines the highest value for each set of regression descriptors
    summ.max <- c(max(summ.intercept.t),
                  max(summ.independent.t),
                  max(summ.r.squared),
                  max(summ.adj.r.squared))
    # Iterates summary.counts times to detemine which regression had the highest descriptor
    # values for each category of interest. 
    for(j in 1:summary.counts){
      # Determines the best performing intercept t value
      if(summ.max[1] == summ.intercept.t[j]){
        # Creates the name of the test
        model.name <- paste(colorI,j,sep=".")
        # Assigns row i, column 1 with the value of model.name and records the model name to the best list
        # for later calculation
        reportlist[i,1] <- summ.max[1]
        reportlist[i,2] <- model.name
        best.list <- append(best.list,model.name)
      }
      # Determines the best performing independent variable t value
      if(summ.max[2] == summ.independent.t[j]){
        # Creates the name of the test
        model.name <- paste(colorI,j,sep=".")
        # Assigns row i, column 1 with the value of model.name and records the model name to the best list
        # for later calculation
        reportlist[i,3] <- summ.max[2]
        reportlist[i,4] <- model.name
        best.list <- append(best.list,model.name)
      }
      # Determines the best performing r squared value
      if(summ.max[3] == summ.r.squared[j]){
        # Creates the name of the test
        model.name <- paste(colorI,j,sep=".")
        # Assigns row i, column 1 with the value of model.name and records the model name to the best list
        # for later calculation
        reportlist[i,5] <- summ.max[3]
        reportlist[i,6] <- model.name
        best.list <- append(best.list,model.name)
      }
      # Determines the best performing adjusted r squared value
      if(summ.max[4] == summ.adj.r.squared[j]){
        # Creates the name of the test
        model.name <- paste(colorI,j,sep=".")
        # Assigns row i, column 1 with the value of model.name and records the model name to the best list
        # for later calculation
        reportlist[i,7] <- summ.max[4]
        reportlist[i,8] <- model.name
        best.list <- append(best.list,model.name)
      }
    }
    # Determines which test had the most occurrences of significant descriptors
    for(k in length(best.list)){
      # Determines the frequency of term k in best.list
      best.freq <- sum(best.list==best.list[k])
      # Determines if the number of occurrences of item k is greater than the currently recorded term
      if (best.freq > reportlist[i,9]){
        # If item k has more occurrences, replaces the occurrence value in the reportlist
        # data frame with the frequency of item k
        reportlist[i,9] <- best.freq
        # Records model k as the best performing model
        best.model <- best.list[k]
      }
    }
    # Adds the determined best model to column 5 of row i in the reportlist data frame
    reportlist[i,9] <- best.model
  }
  # Returns the reportlist data frame
  return(reportlist)
}

# Performs linear regressions of data from the rgb_dat data frame
rgb.red.1 <- lm(RA~R1,data=rgb_dat) # Determines the effect of R1 on RA
rgb.red.2 <- lm(RA~R2,data=rgb_dat) # Determines the effect of R2 on RA
rgb.red.3 <- lm(R1~R2,data=rgb_dat) # Determines the effect of R2 on R1

rgb.green.1 <- lm(GA~G1,data=rgb_dat) # Determines the effect of G1 on GA
rgb.green.2 <- lm(GA~G2,data=rgb_dat) # Determines the effect of G2 on GA
rgb.green.3 <- lm(G1~G2,data=rgb_dat) # Determines the effect of G2 on G1

rgb.blue.1 <- lm(BA~B1,data=rgb_dat) # Determines the effect of B1 on BA
rgb.blue.2 <- lm(BA~B2,data=rgb_dat) # Determines the effect of B2 on BA
rgb.blue.3 <- lm(B1~B2,data=rgb_dat) # Determines the effect of B2 on B1

# Stores the summaries of the above regressions in respective variables
rgb.red.1.summ <- summary(rgb.red.1)
rgb.red.2.summ <- summary(rgb.red.2)
rgb.red.3.summ <- summary(rgb.red.3)

rgb.green.1.summ <- summary(rgb.green.1)
rgb.green.2.summ <- summary(rgb.green.2)
rgb.green.3.summ <- summary(rgb.green.3)

rgb.blue.1.summ <- summary(rgb.blue.1)
rgb.blue.2.summ <- summary(rgb.blue.2)
rgb.blue.3.summ <- summary(rgb.blue.3)

# Performs linear regressions of data from the color_calb data frame
calb.red.1 <- lm(R~R.Red.,data=color_calb) # Determines the effect of R.Red. on R
calb.red.2 <- lm(R~R.LED.on.,data=color_calb) # Determines the effect of R.LED.on. on R

calb.green.1 <- lm(G~G.Green.,data=color_calb) # Determines the effect of G.Red. on G
calb.green.2 <- lm(G~G.LED.on.,data=color_calb) # Determines the effect of G.LED.on. on G

calb.blue.1 <- lm(B~B.Blue.,data=color_calb) # Determines the effect of B.Red. on B
calb.blue.2 <- lm(B~B.LED.on.,data=color_calb) # Determines the effect of B.LED.on. on B

# Stores the summaries of the above regressions in respective variables
calb.red.1.summ <- summary(calb.red.1)
calb.red.2.summ <- summary(calb.red.2)

calb.green.1.summ <- summary(calb.green.1)
calb.green.2.summ <- summary(calb.green.2)

calb.blue.1.summ <- summary(calb.blue.1)
calb.blue.2.summ <- summary(calb.blue.2)


# Creates a vector containing the colors of interest
colors.review <- c('red','green','blue')
# Creates composite summaries of regressions 1 (X~X1), 2 (X~X2), and 3 (X1~X2) for the rgb regression set
review.1 <- c(rgb.red.1.summ,rgb.green.1.summ,rgb.blue.1.summ)
review.2 <- c(rgb.red.2.summ,rgb.green.2.summ,rgb.blue.2.summ)
review.3 <- c(rgb.red.3.summ,rgb.green.3.summ,rgb.blue.3.summ)
# Runs the modelCompare function on review sets 1, 2, and 3 and stores the results in the rgb.summary data frame
rgb.summary <- modelCompare(summ.1 = review.1,summ.2 = review.2,summ.3 = review.3,colorSet = colors.review)
# Labels the rows and columns of the rgb_summary data frame
rownames(rgb.summary) <- colors.review
colnames(rgb.summary) <- c('intercept.t.value','intercept.t.test',
                           'independent.t.value','independent.t.test',
                           'r.squared','r.squared.value',
                           'adj.r.squared','adj.r.squared.test',
                           'best.model')

# After running the modelCompare function on the three RGB regressions it becomes
# apparent that the third model format (X1~X2) has the strongest relationship.
# This is based on the .3 regression format having the highest r value, adjusted
# r squared, and independent variable t-score across all colors. However, since
# we are more interested in determining the strength of a model that can match a
# known RGB value to the RGB values determined by a sensor, this relationship is
# not incredibly useful. Instead, we will perform the modelCompare operation again
# but this time only on regression models 1 and 2. This should tell us whether the
# X1 or X2 method of sensor arrangement is better equipped to perceive the RGB values
# of the paint we are studying. 

# Runs the modelCompare function for regression model types 1 and 2 ONLY
rgb.summary.1.2 <- modelCompare(summ.1 = review.1,summ.2 = review.2,colorSet = colors.review)
# Labels the rows and columns of the rgb.summary.1.2 data frame
rownames(rgb.summary.1.2) <- colors.review
colnames(rgb.summary.1.2) <- c('intercept.t.value','intercept.t.test',
                               'independent.t.value','independent.t.test',
                               'r.squared','r.squared.value',
                               'adj.r.squared','adj.r.squared.test',
                               'best.model')

# In the new data frame (rgb.summary.1.2) we can see now that regression test 2
# (X~X2) provides the more compelling model. Regression model 3 (X1~X2) measured
# the ability to determine a sensor reading (X1) from another sensor reading (X2).
# This can be useful in determining if the sensor is operating reliably, but if we
# are instead interested in a regression model that allows us to determine a paint
# color from a sensor reading then regression model 2 provides use with that. The
# adjusted r squared values for model 2 are:
#    red   -> 0.8769735
#    green -> 0.9343078
#    blue  -> 0.8273811
# These adjusted r squared values indicate that test 2 is a strong predictor of
# the true color of paint presented to the sensor. Additionally, the model 2 r
# squared values (below) similarly show that the model is a good fit for the 
# data that we provided:
#    red   -> 0.8808181
#    green -> 0.9363607
#    blue  -> 0.8327755
# (This summary information is all available within the rgb.summary.1.2 data frame)
# All this data implies that black hose washer providing light shielding to the 
# sensor improves its ability to detect colors accurately.

# Reassigns the review.1 and review.2 lists to now represent the color_calb models
review.1 <- c(calb.red.1.summ,calb.green.1.summ,calb.blue.1.summ)
review.2 <- c(calb.red.2.summ,calb.green.2.summ,calb.blue.2.summ)
# Runs the modelCompare function on the color_calb regression models and stores the results in calb.summary
calb.summary <- modelCompare(summ.1 = review.1,summ.2 = review.2,colorSet = colors.review)
# # Labels the rows and columns of the calb.summary data frame
rownames(calb.summary) <- colors.review
colnames(calb.summary) <- c('intercept.t.value','intercept.t.test',
                            'independent.t.value','independent.t.test',
                            'r.squared','r.squared.value',
                            'adj.r.squared','adj.r.squared.test',
                            'best.model')
# Finally, after running the modelComapre function on the color_calb associated
# models we can determine that regression model 2 (X~X.LED.on.) performs better
# than model 1 (X~X.[Color]). The adjusted r squared values for the colors tested
# are:
#    (Adjusted R Squared)
#    red   -> 0.8496817
#    green -> 0.9223584
#    blue  -> 0.8044472
# This adjusted r squared values imply that the models are able to predict the actual
# paint color relatively well, and when combined with their r squared values (below)
# we can see that the model is the more robust option.
#    (R Squared)
#    red   -> 0.8533480
#    green -> 0.9242521
#    blue  -> 0.8092168
# (Summary information provided above is all availible in the calb.summary data frame)
# Since we are trying to determine the best set up method for the light sensor on
# the arduino board, the regression models created here imply that the white light
# LED is better for allowing the sensor to determine the paint color presented
# to it. 

#*******************************************************************************

# Part 2

# Loads in the dominance analysis library
library(dominanceanalysis)
# Creates the bc.data data frame and prompts the user to import data via file path
bc.data <- read.csv(file.choose(),header=TRUE)
# Transforms the BenignMalignant column to be in terms of 0 or 1
bc.data$BenignMalignant <- (bc.data$BenignMalignant/2)-1

# Runs a linear regression on BenignMalignant
bc.bm <- glm(BenignMalignant~ClumpThickness+CellSizeUniformity+CellShapeUniformity+MarginalAdhesion+
             SingleEpithelialCell+BareNuclei+BlandChromatin+NormalNucleoli+Mitoses, 
             data=bc.data, family=binomial(link='logit'))
# Performs dominance analysis on the regression
dom.bm <- dominanceAnalysis(bc.bm)
# Creates the average r2.m contribution of each term of bm.bm
bm.avgCont <- averageContribution(dom.bm,fit.functions = "r2.m")

# Creates a dominance matrix for complete dominance, conditional dominance, and general dominance
bm.complete <- dominanceMatrix(dom.bm, type="complete",fit.functions = "r2.m" )
bm.conditional <- dominanceMatrix(dom.bm, type="conditional",fit.functions = "r2.m")
bm.general <- dominanceMatrix(dom.bm, type="general",fit.functions = "r2.m")

# Initializes the dom.factors vectors  
dom.factors <- c('')

# Iterates through each row and column of the complete dominance matrix
for (i in 1:nrow(bm.complete)){
  for (j in 1:ncol(bm.complete)){
    # If a factor is completely dominant over another, proceed
    if (bm.complete[i,j]==1.0){
      # Create a string with the dominant factor, the type of dominance, and the submissive factor
      temp <- paste(rownames(bm.complete)[i],"has complete dominance over",colnames(bm.complete)[j])
      # Adds the string to the dom.factor vector
      dom.factors <- append(dom.factors,temp)
      
    }
  }
}
# Repeats above for conditional dominance
for (i in 1:nrow(bm.conditional)){
  for (j in 1:ncol(bm.conditional)){
    if (bm.conditional[i,j]==1){
      temp <- paste(rownames(bm.conditional)[i],"has conditional dominance over",colnames(bm.conditional)[j])
      dom.factors <- append(dom.factors,temp)
    }
  }
}
# Repeats the above for general dominance
for (i in 1:nrow(bm.general)){
  for (j in 1:ncol(bm.general)){
    if (bm.general[i,j]==1){
      temp <- paste(rownames(bm.general)[i],"has general dominance over",colnames(bm.general)[j])
      dom.factors <- append(dom.factors,temp)
    }
  }
}
# Printe the dom.factors vector and the average contribution of each factor
print(dom.factors)
print(bm.avgCont)

# Based on the results of the dominance analysis performed on the bc.data data frame
# the most influencital factors on malignancy are:
# 1) Bare Nuclei            (0.144)    
# 2) Cell Shape Uniformity  (0.130)
# 3) Cell Size Uniformity   (0.125)
# 4) Clump Thickness        (0.111)
# 5) Bland Chromatin        (0.105)
# 6) Marginal Adhesion      (0.077)
# 7) Normal Nucleoli        (0.076)
# 8) Single Epithelial Cell (0.073)
# 9) Mitoses                (0.034)
# Additionally, parsing the information in the bm.complete, bm.conditional, and
# bm.general matrices will give information on the dominance of factors in the 
# model (1 means dominant, 0 means submissive,0.5 means not determinable). Alternatively,
# the dom.factors vector is a list of factors and their dominance status over other
# factors in the model. Either review method will yield the user more information
# regarding factor dominance in the model. 