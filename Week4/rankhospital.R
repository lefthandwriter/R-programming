## Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num="best"){
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
	if(outcome==vOutcomes[1]){
		outCol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	}
	else if (outcome==vOutcomes[2]){
		outCol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	}
	else{
		outCol = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	}	
	
	## Return the hospital name in that state with the given rank 30-day death rate
	if(num=="best"){
		best(state, outcome)
	}
	else{
		subData = data[data[,"State"]==state,]
		subData = subData[!subData[,outCol]=="Not Available",]
		subData[,outCol] <- sapply(subData[,outCol], as.numeric) # without this, min acts strangely, because it was a char column!
		sorted = subData[order(subData[,outCol], subData$Hospital.Name),] ## where ties exist, Hospital Name to break tie		
		#print(sorted[1:4,"Hospital.Name"])
		if(num=="worst"){
			sorted[nrow(subData),"Hospital.Name"]	
		}
		else if(is.numeric(num)){
			sorted[num,"Hospital.Name"]
		}
		else{
			stop('invalid num')
		}
	}
}

