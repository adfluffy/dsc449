# Initializes the igraph library
library(igraph)
# Imports flight an airport data from csv files. Both are included with my submission
# and are named 'airports' and 'flights', respectively. Airport data was taken from
# the Global Airport Database (http://www.partow.net/miscellaneous/airportdatabase/index.html)
# and was cleaned before application to the assignment.
airports <- read.csv(file.choose(),header=TRUE)
flights <- read.csv(file.choose(),header=TRUE)

# Creates a graph from the flight data data frame
flight_graph <- graph_from_data_frame(flights)
# Displays the vertices and edges of the flight graph
V(flight_graph)
E(flight_graph)

# Creates the label offset for the circular graph plot
label_offset <- c(1:58)
# Determines the appropriate angle to offset the plot's labels
label_offset <- (-pi*label_offset)/30
# Creates a plot of flight_graph with the IATA codes for each airport
plot(flight_graph,layout=layout.circle,vertex.label=V(flight_graph)$name,vertex.label.dist = 2,vertex.label.degree = label_offset[V(flight_graph)],vertex.size=4,edge.width = 1)

# Creates a tkplot of the flight_graph
tkplot(flight_graph,canvas.height = 450,canvas.width = 450)
# Using the Kamada-Kawai method in tkplot results in a flight map that is similar in
# shape to the geographical map of the flights

# Creates a table of flight origination frequency and orders it in descending order
originator_list <- sort(table(flights$Outbound),decreasing = TRUE)
originator_list
# Assigns the name of the most frequent originating airport to the 'originator' variable
originator <- attr(originator_list[1],"name")
originator
# Creates a table of flight arrival frequency and orders it in descending order
arrival_list <- sort(table(flights$Inbound),decreasing = TRUE)
arrival_list
# Assigns the name of the most frequent receiving airport to the 'arrival' variable
arrival <- attr(arrival_list[1],"name")
arrival

# Determines if there are differences in outbound/inbound numbers for each airport
difference_list <- originator_list - arrival_list
difference_list
# All of the airports have the same number of arrivals as departures

# Creates a data frame to store the list of cities the airports are located in
city_list <- data.frame()

# For each airport, determines their IATA marker and uses that to find the associated
# city from the airports data frame. The code then takes the marker + city and adds
# it as a row to the city_list data frame. 
for (i in 1:length(arrival_list)){
  marker <- attr(arrival_list[i],"name")
  city <- airports$City[airports$IATA==marker]
  
  city_list <- rbind(city_list,c(marker,city))
}
# Applies appropriate names to the city_list data frame columns
colnames(city_list) <- c("IATA","City")

# Uses the city_list data frame to add an attribute 'cityname' to the vertices of
# flight_graph
for (i in 1:length(V(flight_graph))){
  marker <- V(flight_graph)$name[i]
  
  V(flight_graph)$cityname[i] <- airports$City[airports$IATA==marker]
}
# Publishes the newly applied city names to the console
V(flight_graph)$cityname

# Adds weight values to the edge list of flight_graph
for (i in 1:length(E(flight_graph))){
  # Finds the inbound airport by marker for edge i
  inb <- get.edgelist(flight_graph)[i,1]
  # Finds the outbound airport by marker for edge i
  outb <- get.edgelist(flight_graph)[i,2]
  # Applies the time in minutes from the flight data frame where the inbound and outbound
  # airports match edge i 
  E(flight_graph)$weight[i] <- flights$Minutes[flights$Inbound==inb & flights$Outbound==outb]
}
# Publishes the weights to the console
E(flight_graph)$weight

# Creates a data frame to house the data on least travel time to each airport
least_flight <- data.frame()
# Populates the data frame with the travel time information
least_flight <- distances(flight_graph)

# Determines if the new graph is connected on all vertices
is_connected(flight_graph)

# Asks for the CSV file that has the data for new Inbound, OUtbound, and Minutes for
# the new flights I created. Information is containted in 'my flights.csv' included 
# with my assignment submission
new_flights <- read.csv(file.choose(),header=TRUE)


# The below code is how I created the information I recorded in the 'my flights' csv
#--------------------------------------------------------------------------------------
# Creates a list of possible destinations from the USA and not in the list of current flights
# possible_destinations <- airports$IATA[airports$Country=="USA"]
# possible_destinations <- possible_destinations[!possible_destinations%in%city_list$IATA]

# Chooses a set of 5 of the possible airports
# new_destinations <- sample(possible_destinations,5)

# Initializes the new_flights data frame 
# new_flights <- data.frame()
# Determines all possible destinations (new airports + current airports)
# all_destinations <- c(new_destinations,city_list$IATA)

# Creates 10 new flights. Randomly chooses if a new airport is the source or destination
# for (i in 1:10){
#   random_decider <- sample(1:2,1)
  
#   if (random_decider==1){
#     inbound <- sample(all_destinations,1)
#     outbound <- sample(new_destinations,1)
#   } else{
#     inbound <- sample(new_destinations,1)
#     outbound <- sample(all_destinations,1)
#   }
   
#   new_flights <- rbind(new_flights,c(inbound,outbound))
# }

# Names the columns of the new_flights data frame
# colnames(new_flights) <- c("Inbound","Outbound")

# Manually looked up the flight times of the 10 generated flights using
# https://www.flighttimecalculator.org/
# new_flights$Minutes[1] <- (6*60)+2
# new_flights$Minutes[2] <- (18*60)+43
# new_flights$Minutes[3] <- (22*60)+41
# new_flights$Minutes[4] <- (8*60)+6
# new_flights$Minutes[5] <- (1*60)+2
# new_flights$Minutes[6] <- (2*60)+44
# new_flights$Minutes[7] <- (1*60)+26
# new_flights$Minutes[8] <- (4*60)+20
# new_flights$Minutes[9] <- (2*60)+44
# new_flights$Minutes[10] <- (3*60)+1
#-------------------------------------------------------------------------------

# Adds the old flights to the list of flights I generated
new_flights <- rbind(new_flights,flights)
# Creates a new graph based on the new flight data
new_flight_graph <- graph_from_data_frame(new_flights)

# As above, adds the flight times to the edge list as the weight metric
for (i in 1:length(E(new_flight_graph))){
  inb <- get.edgelist(new_flight_graph)[i,1]
  outb <- get.edgelist(new_flight_graph)[i,2]
  
  E(new_flight_graph)$weight[i] <- new_flights$Minutes[new_flights$Inbound==inb & new_flights$Outbound==outb]
}
# Displays the results of above code
E(new_flight_graph)$weight

# Creates a new data frame to house the least time to get from one vertex to another
new_least_flight <- data.frame()
# Populates the data frame with time information
new_least_flight <- distances(new_flight_graph)

# Creates a data frame to house the reduced flight data (removal of 3 FLL routes)
less_flights <- data.frame(flights)
# Creates a data frame that will contain the data on the 3 removed FLL flights for 
# debugging and informational purposes
removed_flights <- data.frame()

# Performs 3 FLL Row deletions
for (i in 1:3){
  # Randomly samples a flight coming from or going to FLL to delete (does this for each iteration
  # to prevent trying to call a row whose number has changed due to row deletion)
  delete_row <- sample(which(less_flights$Inbound=="FLL" | less_flights$Outbound=="FLL"),1)
  # Adds the data from the row to be deleted to the moved_flights data frame
  removed_flights <- rbind(removed_flights,less_flights[delete_row,])
  # Removes the specified row form the less_flights data frame
  less_flights <- less_flights[-delete_row,]
}
# Creates a graph based on the reduced flight list created above
less_flight_graph <- graph_from_data_frame(less_flights)

# Determines if the new graph is connected on all vertices
is_connected(less_flight_graph)

# As above, adds the flight times to the edge list as the weight metric
for (i in 1:length(E(less_flight_graph))){
  inb <- get.edgelist(less_flight_graph)[i,1]
  outb <- get.edgelist(less_flight_graph)[i,2]
  
  E(less_flight_graph)$weight[i] <- less_flights$Minutes[less_flights$Inbound==inb & less_flights$Outbound==outb]
}
# Displays the result of above code
E(less_flight_graph)$weight

# Creates a new data frame to house the least time to get from one vertex to another
less_least_flight <- data.frame()
# Populates the data frame with time information
less_least_flight <- distances(less_flight_graph)