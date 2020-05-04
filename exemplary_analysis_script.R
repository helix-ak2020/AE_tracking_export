#First specify Working Directory
setwd("E:/Tracking Paper/Zoology Publication/Output") #specify working directory

#Functions ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to read numeric values, requires message what data to enter as input parameter
readnumeric <- function(input_message){
  print(input_message)
  n <- readLines(con=stdin(), n=1)#readline()
  if(!grepl("^[0-9]+(\\.[0-9]+)?$",n))
    {
      print("This is not a numeric value!")
      return(readnumeric(input_message))
    }
  return(as.numeric(n))
}

# Function to read text strings, requires message what data to enter and a regular expression to match as input parameter
readtext <- function(input_message,regtomatch){
  cat(input_message)
  n <- readLines(con=stdin(), n=1)
  if(!grepl(regtomatch,n))
  {
    print("Invalid entry!")
    return(readtext(input_message,regtomatch))
  }
  return(as.character(n))
}

#Functon to calculate euclidean distance between two successive points in a data frame. requires data frame as input, 
#returns distance as vector
euclidean_dist <- function(input_data) {
  distance <- NA
  for(i in (1:(nrow(input_data)-1))){
    distance[i] = sqrt((input_data[i+1,1]-input_data[i,1])^2+(input_data[i+1,2]-input_data[i,2])^2)
  }
  return(distance)
}


#Core Piece of the script. This Function performs a LOESS to smooth the data
#Smoothing is controlled by the user during the process
data_analysis <- function(input_count, input_distance) {
  
  track_accept <- F
  current_span <- 1
  input_df <- data.frame(input_count, input_distance)
  
  while (track_accept==F) {
    plot(input_count, input_distance, xlab="Time[s]", ylab="Travelled Distance[m]", pch=3, cex=0.5)
    print("Original data is plotted in black")
    #Plot the last prediction if it exists
    if(exists("current_prediction")==T) {
      points(input_count, current_prediction, col="steelblue2", pch=1, cex=0.7)
      print("Last smoothing attempt, plotted in light blue")
    }
    #Make LOESS prediction
    current_model <- loess(input_distance ~ input_count, data=input_df, span=current_span)
    current_prediction <- predict(current_model)
    #Plot new prediction
    points(count,current_prediction, col="blue", pch=8, cex=0.5)
    print("New smoothing attempt, plotted in blue")
    #print(current_model$residuals)
    print(summary(current_prediction))
    
    #Function to calculate Sum of Squared Errors (SSE), requires Model to calculate
    #residuals from and returns SSE
    calculate_SSE <- function(x){
      loessMod <- try(loess(input_distance ~ input_count, data=input_df, span=x), silent=T)
      res <- try(loessMod$residuals, silent=T)
      if(class(res)!="try-error"){
        if((sum(res, na.rm=T) > 0)){
          sse <- sum(res^2)  
        }
      }else{
        sse <- 99999
      }
      return(sse)
    }
    sse <- calculate_SSE(current_span)
    ssenam <- paste("Current Sum of Squares Error (SSE)=", sse, sep="")
    print(ssenam)
    
    #Asking for veryfication
    match_message <- "Does this curve match your data? Type 'YES' or 'NO'"
    match_reg <- "^(YES)|(NO)$"
    input_var<-readtext(match_message,match_reg)
    #Asking the User to set a new span value
    if (input_var == "YES") {
      track_accept=T
    } else {
    span_info <- paste("Current span value", current_span, sep=" ")
    print(span_info)
    span_message <- "Enter a new span value. Decreasing the span value increases accuracy"
    current_span <- readnumeric(span_message)
    }
    
  }

  #creating derivatives of the curve
  return(current_prediction)
}

#Function calculating first and second derivative
deriv_analysis <- function(input_dataX, input_dataY) {
  input_message_deriv = "Calculate first and second order derivatives (velocity and acceleration)?"
  match_reg <- "^(YES)|(NO)$"
  deriv_YN <- readtext(input_message_deriv, match_reg)
  if (deriv_YN=="YES") {
    velocity <- diff(input_dataY)/diff(input_dataX)
    acceleration <- diff(velocity)/diff(input_dataX[1:length(velocity)])
    maxIP <- max(input_dataY)
    maxV <- max(velocity)
    maxA <- max(acceleration)
    plot(input_dataX, input_dataY, col="blue")
    scale_factorV <- (maxIP/maxV)
    scale_factorA <- (maxIP/maxA)
    print(length(velocity))
    print(length(acceleration))
    print(maxA)
    print(maxV)
    print(mean(acceleration))
    points(input_dataX[1:length(velocity)], (velocity*scale_factorV), col="orange")
    points(input_dataX[1:length(acceleration)], (acceleration*scale_factorA), col="black")
    finished_df=data.frame(input_dataX[1:length(acceleration)],input_dataY[1:length(acceleration)],velocity[1:length(acceleration)],acceleration)
    colnames(finished_df)=c("time [s]","travelled distance [m]","velocity [m/s]","acceleration [m/s^2]")
  }else {
    finished_df=data.frame(count,input_dataY)
    colnames=c("time [s]","travelled distance [m]")
    }

  return(finished_df)
}

#Function to save the File
save_file <- function(input_df) {
  message_filename <- "Type Filename. Data will be saved to working directory"
  filename_reg <- "^[a-zA-Z0-9_]+$"
  filename <- readtext(message_filename, filename_reg)
  nam <- paste(filename, ".csv", sep="")
  write.csv(input_df, file=nam)
  print("File succesfully saved!")
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# The script allows you to enter values for Framerate, and reference scales via the command prompt
#Messages for input

input_messageFR <-"Please enter the framerate in [Frames/s]:"
input_messagescalePX <-"Please enter the length of your reference object in [pixel]:"
input_messagescalem <-"Please enter the length of your reference object in [mm]:"
input_messagecorrect <-"Is this input correct? Type YES or NO:"
input_correctREG <-"^(YES)|(NO)$"
input_correct <- F

inData <- read.csv2(choose.files(), sep= ",", header=T ) #GUI, select your tracker

#Here the script is asking for input. After all data is entered, the script shows the user the entries and
#asks for confirmation prior to proceeding
while (input_correct==F) {
  framerate <- readnumeric(input_messageFR)
  scalePX <- readnumeric(input_messagescalePX)
  scalem <- (readnumeric(input_messagescalem)/1000)
  print("You entered the following:")
  print(c("Framerate",as.character(framerate), "frames per second"))
  print(c("Reference length:",as.character(scalePX), "pixel"))
  print(c("Reference length:",as.character((scalem*1000)), "mm"))
  input_confirmation <- readtext(input_messagecorrect,input_correctREG)
  if (input_confirmation=="YES") {
    input_correct=T
    print("Input correct! You can now proceed with running the script until the next stop line.")
    }
  
}

#ONLY RUN SRIPT TIll HERE++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Proceed after previous lines have been executed

Sfac=(scalem/scalePX) #Calculate scaling factor
#Convert text strings from text file into numeric data
X<- (as.numeric(as.character(inData[, 1]))*Sfac) # isolate X coordinates and scale them
Y<- (as.numeric(as.character(inData[, 2]))*Sfac) # isolate Y coordinates and scale them
lDat <- data.frame(X,Y) #Bundling data to single DF

point_distance <- euclidean_dist(lDat) #Calculate euclidean distance between individual tracking coordinates

#Calculate the timestep and an a vector which adds a timestamp to every obtained value
timesc <- seq(0,(nrow(lDat)*1/framerate), 1/framerate)
cCTR <- seq(1, (nrow(lDat)-1), 1)
count <- timesc[cCTR]

#Reconstruct travelled distance from distance between points
travelled_distance <-0 
for (i in 2:length(point_distance)){
  travelled_distance[i]<-travelled_distance[i-1]+point_distance[i]
}

#Fitting the curve
smooth_location <- data_analysis(count, travelled_distance) # See respective Function

#ONLY RUN SRIPT TIll HERE++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Proceed after previous lines have been executed

processed_data <- deriv_analysis(count, smooth_location) # See respective Function
View(processed_data)

#ONLY RUN SRIPT TIll HERE++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Proceed after previous lines have been executed

save_file(processed_data) #Data Save. Filename supports letters, numbers and "_"

mean(processed_data$`velocity [m/s]`)
mean(processed_data$)