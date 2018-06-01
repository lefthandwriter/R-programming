## 2. Find the best hospital in a state ##
## Spent 2 hours debugging:
## Wrong answer -> checked minimum wasn't correct -> data-type reason -> changed data-type of column to numeric
best <- function(state, outcome){	
	state = toupper(state) # make upper case
	outcome = tolower(outcome) # make lower case
	
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
	
	## Check that state and outcome are valid
	vStates = unique(data[,"State"])
	if(sum(state==vStates)<1){
		stop("invalid state")
	}
	vOutcomes = c("heart attack", "heart failure", "pneumonia")
	if(sum(outcome==vOutcomes)<1){
		stop("invalid outcome, valid outcomes: heart attack, heart failure, pneumonia")
	}
	
	## Return hosptal name in that state with lowest 30-day death rate
	if(outcome==vOutcomes[1]){
		outCol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	}
	else if (outcome==vOutcomes[2]){
		outCol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	}
	else{
		outCol = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	}
	#print(outCol)
	#minHospState = tapply(data[,outCol], data[,"State"], min, na.rm=T)
	#idxHospState = tapply(data[,outCol], data[,"State"], which.min)
	#idxHosp = which(minHospState==min(minHospState))
	#c(names(idxHosp), data[as.integer(minHospState[idxHosp]), "Hospital.Name"])
	
	subData = data[data[,"State"]==state, ]
	subData = subData[!subData[,outCol]=="Not Available",]
	subData[,outCol] <- sapply(subData[,outCol], as.numeric) # without this, min acts strangely, because it was a char column!
	idxMin = which(subData[,outCol]==min(subData[,outCol]))
	minHosp = subData[idxMin,"Hospital.Name"]
	#print(minHosp)
	sort(minHosp)[1]
}
