#First specify your Working Directory
setwd("C:/user/working_directory") #specify your working directory

#Functions ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to read numeric values, requires message what data to enter as input parameter
readnumeric <- function(input_message){
  print(input_message)
  n <- readLines(con=stdin(), n=1)#readline()
  if(!grepl("^[0-9]+(\\.[0-9]+)?$",n)) #regular expression to read numeric value 
    {
      print("This is not a numeric value!") #if input does not match reg ex, an error is displayed
      return(readnumeric(input_message))
    }
  return(as.numeric(n)) #returns input as numeric
}

# Function to read text strings, requires message what data to enter and a regular expression to match as input parameter
readtext <- function(input_message,regtomatch){
  cat(input_message)
  n <- readLines(con=stdin(), n=1)
  if(!grepl(regtomatch,n))
  {
    print("Invalid entry!") #if input does not match reg ex, an error is displayed
    return(readtext(input_message,regtomatch))
  }
  return(as.character(n)) #returns input as character string
}

#Functon to calculate euclidean distance between two successive points in a data frame. requires data frame as input, 
#returns distance as vector
euclidean_dist <- function(input_data) {
  distance <- vector() #creates empty vector to write data to
  for(i in (1:(nrow(input_data)-1))){
    #calculation of euclidean distance between points
    distance[i] = sqrt((input_data[i+1,1]-input_data[i,1])^2+(input_data[i+1,2]-input_data[i,2])^2) 
  }
  return(distance) #returns distance between points as a new vector
}


#Following functions are the core Piece of the script. 

#Function making a loess prediction, which is later fed into optimization function in data analysis
#requires a span value (the parameter to be optimised and an input data frame), 
#comes with plotting included and returns resdiual sum of squares! 
model_fun <- function(current_span, input_df) {
  print("Current Span:") 
  print(current_span) #Print the span value passed by optimization function
  
  current_model <- try(loess(input_distance ~ input_time_axis, data=input_df, span=current_span)) #try to generate loess model
  if(class(current_model)!="try-error"){
    current_prediction <- predict(current_model) #if successful make prediction and plot it
    #Plot new prediction
    lines(input_df$input_time_axis,current_prediction, lty=3, col="lightskyblue") #plot prediction (by adding to empty plot generated in "data analysis")
  }
  res <- try(current_model$residuals, silent=T) # calculating residudals of current prediction
  if(class(res)!="try-error"){
    if((sum(res^2, na.rm=T) > 0)){
      sse <- sum(res^2)  #calculate Residual Sum of Squares
    }else{
      print("SSE_reset")
      sse <- 999999999 #If calculation is not succesful set it to a ridicolusly high value so optim discards invalid solutions
    }
  } else {
    print("try_error")
    return(999999999) #also catching errors and sets SSE ridicolusly high
  }
  print("SSE:") 
  print(sse) # prints SSE value to console
  print("---------------") #spacer for console
  return(sse) #Function returns SSE
}

#incorporates the previous model function and minimizes SSE using general optimization. 
#requires time-distance data as seperate vectors. returns loess prediction.
data_analysis <- function(input_time_axis, input_distance) {
  
  input_data <- data.frame(input_time_axis, input_distance) # load input data frame
  
  plot(input_time_axis, input_distance, xlab="Time[s]", ylab="Travelled Distance[m]", pch=3, cex=0.5) #open clean plot to later fill fits in
  print("Original data is plotted in black") 
  print("Fitting attempts are plotted in blue")
  
  #Make LOESS prediction
  #Uses optim to minimize SSE of model Fun LOESS function using "Brent" method
  opt_output <- optim(par=1, lower=0.01, upper=1, fn=model_fun, input_df=input_data, method = "Brent")
  print(opt_output) # Print summary of optimization
  opt_model <- loess(input_distance ~ input_time_axis, data=input_data, span=opt_output$par) #generate lin. Model using Par. from Optimization
  final_prediction <- predict(opt_model)#Make prediction
  lines(input_data$input_time_axis,final_prediction, col="olivedrab", lwd=2) # Plot prediction
  
  print("Final fit is plotted in green")
  return(final_prediction) #returns final prediction
  
}


#Function calculating first and second derivative
deriv_analysis <- function(input_dataX, input_dataY) {
  input_message_deriv = "Calculate first and second order derivatives (velocity and acceleration)?"
  match_reg <- "^(YES)|(NO)$"
  deriv_YN <- readtext(input_message_deriv, match_reg)
  if (deriv_YN=="YES") {
    #Calculate Velocity as first derivative of travelled_distance
    velocity <- diff(input_dataY)/diff(input_dataX)
    #Calculate acceleration as first derivative of velocity
    acceleration <- diff(velocity)/diff(input_dataX[1:length(velocity)]) 
    
    #Printing maxima and plotting data
    maxIP <- max(input_dataY)
    maxV <- max(velocity)
    maxA <- max(acceleration)
    plot(input_dataX, input_dataY, col="blue", xlab= "time", ylab="parameters")
    scale_factorV <- (maxIP/maxV)
    scale_factorA <- (maxIP/maxA)
    print("-------------------------")
    print("max. acceleration [m/s^2] (imprecise):")
    print(maxA)
    print("max. velocity [m/s] (imprecise):")
    print(maxV)
    print("mean. acceleration [m/s^2]:")
    print(mean(acceleration))
    
    points(input_dataX[1:length(velocity)], (velocity*scale_factorV), col="orange")
    points(input_dataX[1:length(acceleration)], (acceleration*scale_factorA), col="black")
    
    #Building data frame
    finished_df=data.frame(input_dataX[1:length(acceleration)],input_dataY[1:length(acceleration)],velocity[1:length(acceleration)],acceleration)
    colnames(finished_df)=c("time","travelled_distance","velocity","acceleration")
    
    
  } else {finished_df=data.frame(input_dataX,input_dataY)
    colnames=c("time [s]","travelled distance [m]")
  }
  return(finished_df) #return finished DF
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
time_axis <- timesc[cCTR]

#Reconstruct travelled distance from distance between points
travelled_distance <-0 
for (i in 2:length(point_distance)){
  travelled_distance[i]<-travelled_distance[i-1]+point_distance[i]
}

#Fitting the curve
smooth_location <- data_analysis(time_axis, travelled_distance) # See respective Function

#ONLY RUN SRIPT TIll HERE++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Proceed after previous lines have been executed

processed_data <- deriv_analysis(time_axis, smooth_location) # See respective Function


#ONLY RUN SRIPT TIll HERE++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Proceed after previous lines have been executed

View(processed_data) # Opens Data frame for inspection
save_file(processed_data) #Data Save. Filename supports letters, numbers and "_"
