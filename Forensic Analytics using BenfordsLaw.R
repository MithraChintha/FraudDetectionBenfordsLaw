#Include benfor.analysis package
install.packages("benford.analysis")
library(benford.analysis)

#Include corporate payment in benford.analysis package to the environment
data("corporate.payment")

source(file.choose()) # select Functions_by_Team_Monsters.R file to add the functions to environment

##############################################################
#Use of Data Profiling Function
##############################################################
#get the data profile of the amount of the corporate payments
dataProfiled <- DataProfiling(corporate.payment$Amount) 

#returns the details of the values distributed
dataProfiled[1:5,]

#returns the lower values
dataProfiled[6,]

#returns the higher values
dataProfiled[7,]

###############################################################
#Use of Benford Frequency Distribution for first digit
################################################################
#BenfordLaw object for the corporate payment amount 
benfordCorporate <- benfordFrequencyDistribution(corporate.payment$Amount,1)

#Chi-square test for corporate payment amount
benfordCorporate$`Chi-Squared Test`

#chi-square difference
data <- benfordCorporate$BenefordDist
#chisquare calculation 
chisquare <- ((data$BenfordsCount - data$ActualCounts) ^ 2) / data$BenfordsCount
#chisquare difference plot
plot(data$FirstDigit,chisquare , xlab = "First Digit", ylab = "Chi-Square difference",pch=5)


#Benford Distribution of first digit
benfordCorporate$BenefordDist

#Mantissa distribution
head(benfordCorporate$MantissaData,100)

#PLot of actual mantissa and benford mantissa with respect to rank
plot(benfordCorporate$MantissaData$mantissa,benfordCorporate$MantissaData$rank,ylab="Rank",xlab = "Mantissa Data")
lines(benfordCorporate$MantissaData$benefordMantissa, benfordCorporate$MantissaData$rank, col = "red", pch = 20)
legend(0.6, 50000, # places a legend at the appropriate place 
       c("Actual Mantissa", "Benfords Mantissa"), # puts text in the legend                           
       lty = c(1, 1), # gives the legend appropriate symbols (lines)               
       lwd = c(2.5, 2.5), col = c("black", "red")) # gives the legend lines the correct color and width


############################################################################
#Different methods in benford analysis
############################################################################

#benford function gives benford object
benfordCorp <- benford(corporate.payment$Amount,number.of.digits = 1,sign = "both")

#Mantissa Arc Test Results
benfordCorp$stats$mantissa.arc.test
#Chi-Square Results
benfordCorp$stats$chisq
#Mean Absolute Deviation in benford object
benfordCorp$MAD
#Distortion Factor in benford object
benfordCorp$distortion.factor

#gives the the statistics of the first Digits of a benford object
getBfd(benfordCorp)

#get the suspected records from the data frame
getSuspects(benfordCorp,corporate.payment)

#gives a data frame with the first digits and the differences from Benford's Law in decreasing order
suspectsTable(benfordCorp, by="absolute.diff")
suspectsTable(benfordCorp, by="difference")
suspectsTable(benfordCorp, by="abs.excess.summation")

#Plot the benford object
#gives digits distribution with respect to frequency, summation and chisquare
plot(benfordCorp)



############################################################################
#Plot First Order, Second Order and Summation Test Results 
############################################################################

install.packages("ggplot2")
install.packages("plotly")
library(ggplot2)
library(plotly)

data("corporate.payment")
DF1 <- zScoring(corporate.payment$Amount,1)
DF2 <- zScoring(corporate.payment$Amount,2)
DF3 <- SumDist(corporate.payment$Amount)
DF4 <- zScoringFirstDigit(corporate.payment$Amount)



# plot First Order Test Results 
p1 <- ggplot(DF1, aes(x=Digit, y=Actual_Freq)) + geom_bar(aes(fill=Flag,text=paste("Expected_Freq:",EFreq)),stat="identity") +
  geom_line(aes(x=as.numeric(Digit),y=EFreq),colour="blue")
p1 <- p1+labs(x="Digits",y="Freq",title="First Order Test")
ggplotly(p1)


# Plot Second Order Test Results 

p2 <- ggplot(DF2, aes(x=Digit, y=Actual_Freq)) + geom_bar(aes(fill=Flag,text=paste("Expected_Freq:",EFreq)),stat="identity") +
  geom_line(aes(x=as.numeric(Digit),y=EFreq),colour="blue")
p2 <- p2+labs(x="Digits",y="Freq",title="Second Order Test")
ggplotly(p2)


# Plot Summation Test Results 
p3 <- ggplot(DF3, aes(x=Digit, y=ASumDigits)) + geom_bar(aes(fill=Flag,text=paste("Actual_Proportion:",ASumP*100,"%")),stat="identity") +
  geom_line(aes(x=as.numeric(Digit),y=ESumDigits),colour="Red")
p3 <- p3+labs(x="Digits",y="Summation",title="Sum Distribution by Digit")
ggplotly(p3)



# plot First Digit Test Results 
p4 <- ggplot(DF4, aes(x=Digit, y=Actual_Freq)) + geom_bar(aes(fill=Flag,text=paste("Expected_Freq:",EFreq)),stat="identity") +
  geom_line(aes(x=as.numeric(Digit),y=EFreq),colour="blue")
p4 <- p4+labs(x="Digits",y="Freq",title="First Order Test")
ggplotly(p4)