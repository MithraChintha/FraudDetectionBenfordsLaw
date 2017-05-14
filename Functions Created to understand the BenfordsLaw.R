########################################################################
########### Z- Statistic Calcuation ####################################
########################################################################

# Created Function to calculate Z-Statistic for First Order and Second Order Digits  
zScoring = function(data,order=1){
  
  # Get total number of records in N 
  N = length(data)
  
  # Use getBfd function to get benford statistics dataframe 
  df <- getBfd(benford(data))
  
  # If order = 1 then get first order frequency or else get second order frequency 
  if (order==1) {
    EFreq <- round(df$benford.dist.freq)
    Actual_Freq <- df$data.dist.freq
  } else {
    EFreq <- round(df$benford.so.dist.freq)
    Actual_Freq <- df$data.second.order.dist.freq
  }
  
  # Get Digits 
  Digit <- df$digits
  
  # Get Expected Frequency 
  EP <- round(EFreq/N,5)
  
  # Get Actual Frequency 
  AP <- round(Actual_Freq/N,5)
  
  Numerator <- abs(AP-EP)-(1/(2*N))
  denom <- sqrt(EP*(1-EP)*(1/N))
  
  # Calculate Z-stat
  ZStat <- Numerator/denom
  
  # Store data columns into data frame
  DF <- as.data.frame(cbind(Digit,Actual_Freq,AP,EP,EFreq,ZStat))
  
  
  DF$Digit <- factor(DF$Digit)
  # Create Flag column 
  DF$Flag <- ifelse(DF$ZStat > 1.96 | DF$ZStat < -1.96,1,0)
  DF$Flag <- factor(DF$Flag)
  
  return(DF)  
}

# Created Function to calculate Z-Statistic for First Digits  
zScoringFirstDigit = function(data){
  
  # Get total number of records in N 
  N = length(data)
  
  # Use getBfd function to get benford statistics dataframe 
  df <- getBfd(benford(data,1))
  
  EFreq <- round(df$benford.dist.freq)
  Actual_Freq <- df$data.dist.freq
  
  
  # Get Digits 
  Digit <- df$digits
  
  # Get Expected Frequency 
  EP <- round(EFreq/N,5)
  
  # Get Actual Frequency 
  AP <- round(Actual_Freq/N,5)
  
  Numerator <- abs(AP-EP)-(1/(2*N))
  denom <- sqrt(EP*(1-EP)*(1/N))
  
  # Calculate Z-stat
  ZStat <- Numerator/denom
  
  # Store data columns into data frame
  DF <- as.data.frame(cbind(Digit,Actual_Freq,AP,EP,EFreq,ZStat))
  
  
  DF$Digit <- factor(DF$Digit)
  # Create Flag column 
  DF$Flag <- ifelse(DF$ZStat > 1.96 | DF$ZStat < -1.96,1,0)
  DF$Flag <- factor(DF$Flag)
  
  return(DF)  
}

########################################################################
########### Get Summation Distribution #################################
########################################################################

# Created Function to populate data frame with summation statistics 
SumDist = function(data){
  
  
  # Use getBfd function to get benford statistics dataframe 
  df <- getBfd(benford(data))
  
  # Get Digits 
  Digit <- df$digits
  
  # Get summation 
  ASumDigits <- df$data.summation
  
  # Get Total 
  Total <- sum(ASumDigits)
  
  # Create Dataframe 
  DF <- as.data.frame(cbind(Digit,ASumDigits))
  DF$ESumDigits <- round(Total/90,5)
  DF$ASumP <- round(ASumDigits/Total,5)
  DF$ESumP <- round(1/90,5)
  DF$Digit <- factor(DF$Digit)
  # Create Cutoff (4% Actual Sum Proportion)
  DF$Flag <- ifelse(DF$ASumP*100>4,1,0)
  DF$Flag <- factor(DF$Flag)
  return(DF)
  
}


########################################################################
###########DATA PROFILING###############################################
########################################################################

#This function is used for the data profiling of the data to be used for the Benford's Law
DataProfiling=function(column){
  #Validate the input given
  if(!is.numeric(column) & !is.integer(column)){
   return(warning("Input should be  numerical data"))
  }

  #create a data frame with the one column given as input
  data <- data.frame(column)
  #create a data profile data frame with required upper and lower bounds
  dataProfile <- data.frame()
  dataProfile[1,c("From")] <- rbind(10)#,Inf)
  dataProfile[1,"To"] <-Inf
  dataProfile[2,c("From","To")] <- rbind(0.01,9.99)
  dataProfile[3,c("From","To")] <- rbind(0)
  dataProfile[4,c("From","To")] <- rbind(-9.99,-0.01)
  dataProfile[5,c("From","To")] <- rbind(-Inf,-10)
  dataProfile[6,c("From","To")] <- rbind(0.01,50)
  dataProfile[7,c("From","To")] <- rbind(100000,Inf)
  #Previous steps creates a From column as matrix of 2 columns
  dataProfile$From<-  dataProfile[,1][1:7]
  
  #for loop to calculate the sum , count and percentages of sum and count
  for(i in seq(1:nrow(dataProfile))){
    #access the lower bound and save it in from
    from=  dataProfile[i,1]
    #access the upper bound and save it in to
    to= dataProfile[i,2]   
    #create temp data frame 
    temp=data.frame()
    #For infinity considering max values 
    if(to==Inf)
    {to=max(data)}
    if(to==-Inf)
    {to=max(-data)}
    #filter and get all the values satisfying the upper and lower bounds are assigned into the temp
    temp=data[data$column>= from & data$column<= to, ]
    #count is length of the temp
    dataProfile[i,"Count"]=length(temp)
    #percent TOtal is the count of the temp / count of data
    dataProfile[i,"PercentTotal"]=100*(length(temp)/nrow(data))
    #sum of the temp
    dataProfile[i,"SumTotal"]=sum(temp)
    #Percent sum of the temp / total of the data
    dataProfile[i,"PercentSum"]=(sum(temp)/sum(data))
  }
  row.names(dataProfile)[6] <- "Lower Values Between(0.01 and 50)"
  row.names(dataProfile)[7] <- "Higher Values Between(100,000 and over)"
  #remove the temporarily created temp data frame
  temp <- NULL
  #return the profiled data as a return type of the function
  return(dataProfile)
}


###############################################################################################
#################Benford Frequency Distribution for first digit##########################
#############################################################################################
#This function is used to give benfords frequency distribution for the first digit
benfordFrequencyDistribution = function(numericData, positive = 1) {
  #numericData <- corporate.payment$Amount
  #COnvert the single feature provided in data as data frame
  numbers <- as.data.frame(numericData)
  #rename column name as numbers
  colnames(numbers)[1] <- "numbers"
  #convert the numbers as string to access the first digit
  numbers$numbers <- as.character(numbers$numbers)
  #positive=0 means consider negative as well as positive
  if (positive == 0) {
    #if number is negative then take the second digit otherwise take first digit
    numbers$FirstDigit <- ifelse(substring(numbers$numbers, 1, 1) == "-", substring(numbers$numbers, 2, 2), substring(numbers$numbers, 1, 1))
  } else {
    #1 means consider only positive data
    if (positive == 1) {
      #take all positive numbers
      numbers <- as.data.frame(numbers[substring(numbers$numbers, 1, 1) != "-",])
      #name the column name as numbers as the name of the column in the above changes
      colnames(numbers)[1] <- "numbers"
      #get the FirstDigits of all the numbers
      numbers$FirstDigit <- substring(numbers$numbers, 1, 1)
    }
  }
  #To eliminate all the values which has 0
  numbers <- numbers[numbers$FirstDigit != "0",]
  #save the frequency of digits in benfordDist
  benfordDist <- data.frame(100 * prop.table(table(numbers$FirstDigit)))
  #rename the columns as first digit and actual
  colnames(benfordDist)[1] <- "FirstDigit"
  colnames(benfordDist)[2] <- "ActualProportions"
  #frequencyCounts 
  actualfreq <- as.data.frame(table(numbers$FirstDigit))
  benfordDist$ActualCounts <- actualfreq[, 2]
  actualfreq <- NULL
  totalLength <- sum(benfordDist$ActualCounts)
  #for loop to calculate the actual benford distribution as per the formulae
  
  for (i in seq(1, 9)) {
    benfordDist[benfordDist$FirstDigit == i, "BenfordsProportion"] = 100 * log10(1 + (1 / i))
    benfordDist[benfordDist$FirstDigit == i, "BenfordsCount"] = round(totalLength * log10(1 + (1 / i)))
  }
  #Absolute Difference
  benfordDist$AbsDiff <- abs(benfordDist$ActualCounts - benfordDist$BenfordsCount)
  #Graph of the expected Vs Benefords
  plot(benfordDist$FirstDigit, benfordDist$ActualProportions, col = "blue", xlab = "First Digit", ylab = "Proportions")
  lines(benfordDist$FirstDigit, benfordDist$BenfordsProportion, col = "red", pch = 20)
  legend(6, 30, # places a legend at the appropriate place 
         c("Actual", "Benfords"), # puts text in the legend                           
         lty = c(1, 1), # gives the legend appropriate symbols (lines)               
         lwd = c(2.5, 2.5), col = c("black", "red")) # gives the legend lines the correct color and width
  #returns data frame which contains frequency count, proportions as per the actual data and the benefords law for first digit
  
  if (positive == 1) {
    data = data.frame(numericData[numericData > 0])
  } else {
    data = data.frame(numericData)
  }
  #get mantissa from the column and store it in the mantissa column
  data$mantissa <- cbind(data$numericData - floor(data$numericData))
  #get row order to sort the data based on mantissa in increasing order
  roworder <- order(data$mantissa)
  #use row order to order the data
  data <- data[roworder, 1:ncol(data)]
  #add rank column and add all values
  data$rank <- 1:nrow(data)
  #get the max mantissa value and store it in maxmant
  maxmant <- max(data$mantissa)
  #generate beneford mantissa set using max mantissa and increment it based on max mantissa and number of rows
  benfordSet <- seq(0, maxmant, by = maxmant / nrow(data))
  #taking care to get same length as data to assign 
  data$benefordMantissa <- benfordSet[1:nrow(data)]
  #return the result of the kolmogrov-smirnov test which gives the mantissa follows uniform distribution or not
  marc <- suppressWarnings(ks.test(data$mantissa, data$benefordMantissa))
  #chisquare test on actual counts and benfords count
  chisqr <- chisq.test(as.matrix(benfordDist[, c("ActualCounts", "BenfordsCount")]))
  #assign total number of observations to NumberofObs
  NumberofObs <- c("Number of Observations" = sum(benfordDist$ActualCounts))
  #Add all the output objects to the list
  output <- list(NumberofObs,"BenefordDist"=benfordDist, "Mantissa Arc Test"=marc,"Chi-Squared Test"=chisqr,"MantissaData"=data)
  
  #change the class of the object as BenfordLaw
  class(output) <- "BenfordLaw"
  #returns the benfordlaw object
  return(output)
  
}





