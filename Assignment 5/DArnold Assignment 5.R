# Allows for the import of the M&M CSV for use in later calculations
mms <- read.csv(file.choose(),header=TRUE)

#Useful for later color coding and iterations as necessary
mms_types <- unique(mms$type)
mms_colors <- unique(mms$color)

# Q1 - What is the mean and STD DEV for the following
# A - Type
# Creates a data frame with the mean diameters by M&M type
type_dia_means <- aggregate(mms$diameter,by=list(mms$type),FUN=mean)
# Creates a data frame with the standard deviation of diameters by M&M type
type_dia_sd <- aggregate(mms$diameter,by=list(mms$type),FUN=sd)
# Combines the mean and standard deviation data frames into one summary frame
type_dia_summary <- merge(type_dia_means,type_dia_sd,by='Group.1')
# Labels the columns accordingly
colnames(type_dia_summary) <- c('Type','Mean','StdDev')
# Repeats the above code for mass
type_mass_means <- aggregate(mms$mass,by=list(mms$type),FUN=mean)
type_mass_sd <- aggregate(mms$mass,by=list(mms$type),FUN=sd)
type_mass_summary <- merge(type_mass_means,type_mass_sd,by='Group.1')
colnames(type_mass_summary) <- c('Type','Mean','StdDev')

# B - Type and Color
# Creates a data frame with the mean diameters by M&M type and color
color_dia_means <- aggregate(mms$diameter,by=list(mms$type,mms$color),FUN=mean)
# Creates a data frame with the standard deviation of diameters by M&M type and color
color_dia_sd <- aggregate(mms$diameter,by=list(mms$type,mms$color),FUN=sd)
# Combines the mean and standard deviation data frames into one summary frame
color_dia_summary <- merge(color_dia_means,color_dia_sd,by=c('Group.1','Group.2'))
# Labels the summary data frame columns accordingly
colnames(color_dia_summary) <- c('Type','Color','Mean','StdDev')
#Repeats the above code for mass
color_mass_means <- aggregate(mms$mass,by=list(mms$type,mms$color),FUN=mean)
color_mass_sd <- aggregate(mms$mass,by=list(mms$type,mms$color),FUN=sd)
color_mass_summary <- merge(color_mass_means,color_mass_sd,by=c('Group.1','Group.2'))
colnames(color_mass_summary) <- c('Type','Color','Mean','StdDev')


# Q2 - What is the proportion of color for each type of M&M?
# Creates a matrix to hold the results of a tapply function of the M&M type
# subjected to a function that measures the length of x (being the specific type/color
# combination) and divides that by the number of observations of a specific type of M&M
mms_props <- tapply(X=mms$type,INDEX=list(mms$type,mms$color),FUN=function(x) length(x)/sum(x==mms$type))


# Q3 - Make a bar chart summarizing the information from Q2 (colored)
# Creates a transposed matrix from the mms_props matrix to pass to the barplot function
mms_plot <- matrix(mms_props,nrow=ncol(mms_props),ncol=nrow(mms_props),byrow=TRUE)
# Assigns the appropriate row and column names to the plotting matrix
rownames(mms_plot) <- colnames(mms_props)
colnames(mms_plot) <- rownames(mms_props)
# Creates the bar plot showing the proportion of color by M&M type, using the matrix
# row names to dictate the colors of the bars
barplot(mms_plot,beside=TRUE,col=c(rownames(mms_plot)),
        main="M&M Color Proportions \nby Type of Candy")


# Q4 - Are the colors of M&M's evenly distributed in each type of M&M?
# Creates a matrix containing the counts of each given M&M type and color
mms_counts <- tapply(X=mms$type,INDEX=list(mms$type,mms$color),FUN=length)
# Isolates each type of M&M into a vector to perform the chi square test on
q4_plain <- mms_counts[3,]
q4_peanut <- mms_counts[1,]
q4_pb <- mms_counts[3,]
# Performs a chi square test for each M&M type
chisq.test(x=q4_plain)
chisq.test(x=q4_peanut)
chisq.test(x=q4_pb)
# Based on the results of the above tests, the plain and peanut butter M&Ms do not have a
# high correlation with the hypothesis of even distribution. Peanut M&M, however, are
# more evenly distributed (but still does not pass a hypothesis test with alpha < ~0.1758)


# Q5 - Are the M&Ms of different types the same weight?
summary(aov(mass~type,data=mms))
# Because the above anova test shows that there is strong correlation between M&M type
# and the mss of the M&M we can say that each M&M type has different weights. If type
# was not a strong predictor of weight then we would see that type was not strongly
# correlated with weight.


# Q6 - Are the M&Ms of different types the same diameter?
summary(aov(diameter~type,data=mms))
# As with Q5, since there is a strong correlation between type of M&M and the diameter of
# the M&M we can say that M&Ms of different types do NOT have the same diameter.


# Q7 - Now we are going to make random bags of M&Ms assuming that color has no correlation
#      with mass or diameter. This will be done for part A and B

# The following function will be used for questions 7,8, and 9
mmBagMaker <- function(extraBag=FALSE,byColor=FALSE,exceed=FALSE){
  # Creates blank data frames to act as M&M bags
  pl_bag <- data.frame(matrix(ncol=3,nrow=0))
  pn_bag <- data.frame(matrix(ncol=3,nrow=0))
  pb_bag <- data.frame(matrix(ncol=3,nrow=0))
  extra_pl_bag <- data.frame(matrix(ncol=3,nrow=0))
  extra_pn_bag <- data.frame(matrix(ncol=3,nrow=0))
  extra_pb_bag <- data.frame(matrix(ncol=3,nrow=0))
  # Assigns names to the columns of the bag data frames
  colnames(pn_bag) <- c('Color','Diameter','Mass')
  colnames(pb_bag) <- c('Color','Diameter','Mass')
  colnames(pl_bag) <- c('Color','Diameter','Mass')
  colnames(extra_pn_bag) <- c('Color','Diameter','Mass')
  colnames(extra_pb_bag) <- c('Color','Diameter','Mass')
  colnames(extra_pl_bag) <- c('Color','Diameter','Mass')
  # Itializes the limit variables
  pl_lim <- 396.9
  pn_lim <- 396.9
  pb_lim <- 360
  # Itializes the loop condition variable
  composite <- TRUE
  # Creates indicators for whether each bag has satisfied the weight condition
  pl_done <- FALSE
  pn_done <- FALSE
  pb_done <- FALSE

  extra_pl_done <- FALSE
  extra_pn_done <- FALSE
  extra_pb_done <- FALSE
  # Initializes the loop counter variable to hard break loop execution after 10,000 iterations
  loop_counter <- 0
  pl_counter <- 0
  pn_counter <- 0
  pb_counter <- 0

  while(composite){
    # Sets temporary variables i and j to the color and type of the randomly generated
    # M&M, respectively
    j <- mms_types[((loop_counter%%length(mms_types))+1)]
    i <- sample(mms_colors,1)


    if (byColor){
      # Pulls the mean and standard deviation for the mass of color i type j M&Ms from
      # the data frames created in Q2 and stores them for later use
      mm_mass_mean <- color_mass_summary$Mean[color_mass_summary$Type==j & color_mass_summary$Color==i]
      mm_mass_std <- color_mass_summary$StdDev[color_mass_summary$Type==j & color_mass_summary$Color==i]
      # Pulls the mean and standard deviation for the diameter of  color i type j M&Ms
      # from the data frames created in Q2 and stores them for later use
      mm_dia_mean <- color_dia_summary$Mean[color_dia_summary$Type==j & color_dia_summary$Color==i]
      mm_dia_std <- color_dia_summary$StdDev[color_dia_summary$Type==j & color_dia_summary$Color==i]
    } else {
      mm_mass_mean <- type_mass_summary$Mean[type_mass_summary$Type==j]
      mm_mass_std <- type_mass_summary$StdDev[type_mass_summary$Type==j]
      # Pulls the mean and standard deviation for the diameter of  color i type j M&Ms
      # from the data frames created in Q2 and stores them for later use
      mm_dia_mean <- type_dia_summary$Mean[type_dia_summary$Type==j]
      mm_dia_std <- type_dia_summary$StdDev[type_dia_summary$Type==j]
    }


    # Generates the mass and diameter for color i type j M&Ms
    generated_mm_mass <- rnorm(n=1,mm_mass_mean,mm_mass_std)
    generated_mm_diameter <- rnorm(n=1,mm_dia_mean,mm_dia_std)
    # Determines the mass of all M&Ms already in each bag and adds the mass of the
    # generated M&M that sum
    pl_mass <- sum(pl_bag$Mass) + (generated_mm_mass * sum(!(exceed)))
    pn_mass <- sum(pn_bag$Mass) + (generated_mm_mass * sum(!(exceed)))
    pb_mass <- sum(pb_bag$Mass) + (generated_mm_mass * sum(!(exceed)))

    # Logical operations for adding an M&M to a bag, as well as determining when a bag is full, follows
    # Logic for plain M&Ms
    if (j=='plain' & !(pl_done)){
      # Determines if the plain M&M bag plus the generated M&M would exceed the weight limit
      # Also skips the below code if the plain M&M bag is complete
      if (pl_mass < pl_lim){
        # if the weight limit is NOT exceeded, stores the color, diameter, and mass values for
        # the generated M&M in the temp_df data frame
        temp_df <- data.frame(i,generated_mm_diameter,generated_mm_mass)
        # Applies the appropriate names to the temp_df columns
        colnames(temp_df) <- c('Color','Diameter','Mass')
        # Add the temp_df rows to the pl_bag
        pl_bag <- rbind(pl_bag,temp_df)
      } else {
        # Only executes if pl_limit is exceeded by pl_mass
        if (!(exceed) & ((pl_lim-pl_mass-generated_mm_mass) <= (mm_mass_mean - 2*mm_mass_std))){
          # If exceed is FALSE and the weight limit would be exceeded by adding the generated M&M, then the
          # above if statement determines if the remaining room in the bag is less than or equal to
          # two standard deviations below the mean. If it is, the code considers the bag to be full
          # and changes the pl_done value to TRUE
          if (!(extraBag)){pl_done <- TRUE}
        } else if (exceed) {
          # If exceed is true and the mass of the plain bag is greater than the limit
          # then pl_done is set to TRUE
          if (!(extraBag)){pl_done <- TRUE}
        }
      }
      # If the function is running in extra bag mode, the following code will execute
      # once the bag is full and the extra bag hasn't been completed
      if (extraBag & !(pl_done)){
        # Calculates the weight of the extra bag without the generated M&M for exceed mode, and with it otherwise
        extra_pl_mass <- sum(extra_pl_bag$Mass) + (generated_mm_mass * sum(!(exceed)))
        # If the current wieght of the bag is below the limit, add the M&M
        if (extra_pl_mass < pl_lim){
          # Adds the generated M&M to the extra bag
          extra_pl_bag <- rbind(extra_pl_bag,temp_df)
          # Starts a new plain M&M bag
          pl_bag <- data.frame(matrix(ncol=3,nrow=0))
          colnames(pl_bag) <- c('Color','Diameter','Mass')
          # Increments the counter for the number of other full bags that contributed to
          # the extra bag
          pl_counter <- pl_counter + 1
        } else {
          # Only executes if pn_lim is exceeded by extra_pn_mass
          if (!(exceed) & ((pl_lim-extra_pl_mass-generated_mm_mass) <= (mm_mass_mean - 2*mm_mass_std))){
            # If exceed is FALSE and the weight limit would be exceeded by adding the generated M&M, then the
            # above if statement determines if the remaining room in the bag is less than or equal to
            # two standard deviations below the mean. If it is, the code considers the bag to be full
            # and changes the pl_done value to TRUE
            pl_done <- TRUE
          } else if (exceed) {
            # If exceed is true and the mass of the plain bag is greater than the limit
            # then pl_done is set to TRUE
            pl_done <- TRUE
          }
        }
      }
    } else if (j=='peanut' & !(pn_done)){
      # Above logic for plain type M&Ms repeats for peanut type M&Ms
      if (pn_mass < pn_lim){
        # if the weight limit is NOT exceeded, stores the color, diameter, and mass values for
        # the generated M&M in the temp_df data frame
        temp_df <- data.frame(i,generated_mm_diameter,generated_mm_mass)
        # Applies the appropriate names to the temp_df columns
        colnames(temp_df) <- c('Color','Diameter','Mass')
        # Add the temp_df rows to the pn_bag
        pn_bag <- rbind(pn_bag,temp_df)
      } else {
        # Only executes if pn_lim is exceeded by pn_mass
        if (!(exceed) & ((pn_lim-pn_mass-generated_mm_mass) <= (mm_mass_mean - 2*mm_mass_std))){
          # If exceed is FALSE and the weight limit would be exceeded by adding the generated M&M, then the
          # above if statement determines if the remaining room in the bag is less than or equal to
          # two standard deviations below the mean. If it is, the code considers the bag to be full
          # and changes the pl_done value to TRUE
          if (!(extraBag)){pn_done <- TRUE}
        } else if (exceed) {
          # If exceed is true and the mass of the plain bag is greater than the limit
          # then pl_done is set to TRUE
          if (!(extraBag)){pn_done <- TRUE}
        }
      }
      # If the function is running in extra bag mode, the following code will execute
      # once the bag is full and the extra bag hasn't been completed
      if (extraBag & !(pn_done)){
        # Calculates the weight of the extra bag before adding the extra M&M
        extra_pn_mass <- sum(extra_pn_bag$Mass) + (generated_mm_mass * sum(!(exceed)))
        # If the current wieght of the bag is below the limit, add the M&M
        if (extra_pn_mass < pn_lim){
          # Adds the generated M&M to the extra bag
          extra_pn_bag <- rbind(extra_pn_bag,temp_df)
          # Starts a new peanut M&M bag
          pn_bag <- data.frame(matrix(ncol=3,nrow=0))
          colnames(pn_bag) <- c('Color','Diameter','Mass')
          # Increments the counter for the number of other full bags that contributed to
          # the extra bag
          pn_counter <- pn_counter + 1
        } else {
          # Only executes if pn_lim is exceeded by extra_pn_mass
          if (!(exceed) & ((pn_lim-extra_pn_mass-generated_mm_mass) <= (mm_mass_mean - 2*mm_mass_std))){
            # If exceed is FALSE and the weight limit would be exceeded by adding the generated M&M, then the
            # above if statement determines if the remaining room in the bag is less than or equal to
            # two standard deviations below the mean. If it is, the code considers the bag to be full
            # and changes the pl_done value to TRUE
            pn_done <- TRUE
          } else if (exceed) {
            # If exceed is true and the mass of the plain bag is greater than the limit
            # then pl_done is set to TRUE
            pn_done <- TRUE
          }
        }
      }
    } else if (j=='peanut butter' & !(pb_done)){
      # Above logic for peanut type and plain type M&Ms repeats for peanut butter type M&Ms
      if (pb_mass < pb_lim){
        # if the weight limit is NOT exceeded, stores the color, diameter, and mass values for
        # the generated M&M in the temp_df data frame
        temp_df <- data.frame(i,generated_mm_diameter,generated_mm_mass)
        # Applies the appropriate names to the temp_df columns
        colnames(temp_df) <- c('Color','Diameter','Mass')
        # Add the temp_df rows to the pb_bag
        pb_bag <- rbind(pb_bag,temp_df)
      } else {
        # Only executes if pb_lim is exceeded by pb_mass
        if (!(exceed) & ((pb_lim-pb_mass-generated_mm_mass) <= (mm_mass_mean - 2*mm_mass_std))){
          # If exceed is FALSE and the weight limit would be exceeded by adding the generated M&M, then the
          # above if statement determines if the remaining room in the bag is less than or equal to
          # two standard deviations below the mean. If it is, the code considers the bag to be full
          # and changes the pl_done value to TRUE
          if (!(extraBag)){pb_done <- TRUE}
        } else if (exceed) {
          # If exceed is true and the mass of the plain bag is greater than the limit
          # then pl_done is set to TRUE
          if (!(extraBag)){pb_done <- TRUE}
        }
      }
      # If the function is running in extra bag mode, the following code will execute
      # once the bag is full and the extra bag hasn't been completed
      if (extraBag & !(pb_done)){
        # Calculates the weight of the extra bag before adding the extra M&M
        extra_pb_mass <- sum(extra_pb_bag$Mass) + (generated_mm_mass * sum(!(exceed)))
        # If the current wieght of the bag is below the limit, add the M&M
        if (extra_pb_mass < pb_lim){
          # Adds the generated M&M to the extra bag
          extra_pb_bag <- rbind(extra_pb_bag,temp_df)
          # Starts a new peanut butter M&M bag
          pb_bag <- data.frame(matrix(ncol=3,nrow=0))
          colnames(pb_bag) <- c('Color','Diameter','Mass')
          # Increments the counter for the number of other full bags that contributed to
          # the extra bag
          pb_counter <- pb_counter + 1
        } else {
          # Only executes if pb_lim is exceeded by extra_pb_mass
          if (!(exceed) & ((pb_lim-extra_pb_mass-generated_mm_mass) <= (mm_mass_mean - 2*mm_mass_std))){
            # If exceed is FALSE and the weight limit would be exceeded by adding the generated M&M, then the
            # above if statement determines if the remaining room in the bag is less than or equal to
            # two standard deviations below the mean. If it is, the code considers the bag to be full
            # and changes the pl_done value to TRUE
            pb_done <- TRUE
          } else if (exceed) {
            # If exceed is true and the mass of the plain bag is greater than the limit
            # then pl_done is set to TRUE
            pb_done <- TRUE
          }
        }
      }
    }
    # Evaluates the status of each bag
    if (pl_done & pn_done & pb_done){
      # If all bags (or extra bags if extraBags=TRUE) are done then the composite
      # loop variable is set to FALSE to exit the loop
      composite <- FALSE
    } else if (loop_counter > 100000) {
      # Hard stops the loop if the loop_counter variable exceeds 100,000
      composite <- FALSE
    }

    # Iterates the loop_counter variable
    loop_counter <- loop_counter + 1
  }
  # Creates reports the weights of each bag type to the console as well as the number
  # of full bags needed to create an extra bag if run in extra bag mode
  if (extraBag){
    # Creates a storage term per type for sum extra bag mass
    pl_bag_mass <- round(sum(extra_pl_bag$Mass),digits=2)
    pn_bag_mass <- round(sum(extra_pn_bag$Mass),digits=2)
    pb_bag_mass <- round(sum(extra_pb_bag$Mass),digits=2)
    # Creates report strings for each bag stating the number of other bags required to skim the excess M&M
    pl_report <- paste("After", pl_counter, "full bags 1 excess plain bag was made with mass", pl_bag_mass)
    pn_report <- paste("After", pn_counter, "full bags 1 excess peanut bag was made with mass", pn_bag_mass)
    pb_report <- paste("After", pb_counter, "full bags 1 excess peanut butter bag was made with mass", pb_bag_mass)

  } else {
    # Creates a storage term per type for sum bag mass
    pl_bag_mass <- round(sum(pl_bag$Mass),digits=2)
    pn_bag_mass <- round(sum(pn_bag$Mass),digits=2)
    pb_bag_mass <- round(sum(pb_bag$Mass),digits=2)
    # Calculates the percent of posted weight the bag is
    pl_percent <- round(pl_bag_mass/pl_lim,digits = 4)*100
    pn_percent <- round(pn_bag_mass/pn_lim,digits = 4)*100
    pb_percent <- round(pb_bag_mass/pb_lim,digits = 4)*100
    # Creates report strings containing the above information
    pl_report <- paste("Created plain M&M bag with weight", pl_bag_mass, "for a fill rate of", pl_percent)
    pn_report <- paste("Created peanut M&M bag with weight", pn_bag_mass, "for a fill rate of", pn_percent)
    pb_report <- paste("Created peanut butter M&M bag with weight", pb_bag_mass, "for a fill rate of", pb_percent)
  }
# Prints the reports to the console
  print(pl_report)
  print(pn_report)
  print(pb_report)
}

# A - As a maximization of profit, do not exceed the weight of the bag posted but do NOT
#     leave room for another random candy.
mmBagMaker(extraBag=FALSE,byColor=FALSE,exceed=FALSE)


# B - Because we value the customer, make sure that the bags at a minimum have the weight posted,
#     but do not kill our profits by putting any extra beyond the limit.
mmBagMaker(extraBag=FALSE,byColor=FALSE,exceed=TRUE)


# Q8 - Now we are going to revise the procedure:  color matters.  As you add M&Ms to the bag,
#      you must first generate their color randomly and then use the parameters from that color
#      (calculated in Step 1 B) to specify the values for weight and diameter randomly using a
#      distribution.  Use the same assumptions for your previous bag samples.
# A
mmBagMaker(extraBag = FALSE,byColor = TRUE,exceed=FALSE)

# B
mmBagMaker(extraBag=FALSE,byColor=TRUE,exceed=TRUE)

# Q9 - Use your procedure in 8 to now try to create an excess bag – how many bags
#      of M&M’s did you have to remove the extra M&M from to fill an additional bag?
#      When you jettison the last M&M, put it in a bag you are filling up with the
#      excess only.  This gives you an idea of how much Mars would save (as a percentage)
#      if they were to use strategy A as opposed to strategy B.  Do this for each type of M&M.
# A - with the extra bag just not exceeding the posted weight
mmBagMaker(extraBag=TRUE,byColor=TRUE,exceed=FALSE)

# B - with the extra bag just exceeding the posted weight
mmBagMaker(extraBag=TRUE,byColor=TRUE,exceed=TRUE)
