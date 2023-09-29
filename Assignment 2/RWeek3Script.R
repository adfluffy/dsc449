#Call in data from coordinates.txt.  In this case, I am allowing you to
#specify where the file is.  You could write the address in place of
#file.choose().  Also, this file has headers, namely the first line serve as
#labels.

data=read.table(file.choose(),header=TRUE)
data

#You now have data for 200 random points in 3-space.  Let's declare
#which octant they belong to.

for(row in 1:nrow(data)){
#Find which sign each one is
xsign=data$X[row]>0
ysign=data$Y[row]>0
zsign=data$Z[row]>0

#Figure out which octant based on that
if(xsign & ysign & zsign){
data$Octant[row] = 1
}
else {
if(xsign & zsign & !ysign){
data$Octant[row] = 4
}
else {
if(xsign & ysign & !zsign){
data$Octant[row] = 5
}
else {
if(xsign & !ysign & !zsign){
data$Octant[row] = 8
}
else {
if(!xsign & ysign & zsign){
data$Octant[row] = 2
}
else {
if(!xsign & zsign & !ysign){
data$Octant[row] = 3
}
else {
if(!xsign & ysign & !zsign){
data$Octant[row] = 6
}
else {
if(!xsign & !ysign & !zsign){
data$Octant[row] = 7
} #if close
} #else7 close
} #else6 close
} #else3 close
} #else2 close
} #else8 close
} #else5 close
} #else4 close
} #for loop ends

data

#Let's get the row sums.  In this case, they are inconsequential.
data$Octant=as.numeric(data$Octant)
data$CoordinateSum=apply(X=data,MARGIN=1,FUN=sum)

data

#Let's find the row at which all octants have been covered randomly.
holder=c(F,F,F,F,F,F,F,F)
index = 0
while(sum(holder)<8){
holder[data$Octant[index+1]]=T
index = index + 1
print(holder)
}

index

#Replace the octant classification
for(item in 1:length(data$Octant)){
data$Octant[item]=switch(as.numeric(data$Octant[item]),"I","II","III","IV","V","VI","VII","VIII")
}

data

#Sum by octants
tapply(data$X,INDEX=data$Octant,FUN=sum)
tapply(data$Y,INDEX=data$Octant,FUN=sum)
tapply(data$Z,INDEX=data$Octant,FUN=sum)
tapply(data$CoordinateSum,INDEX=data$Octant,FUN=sum)

