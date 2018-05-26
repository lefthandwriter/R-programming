complete <- function(directory, id=1:332){
	## 'directory': folder name indicating location of csv files
	## 'id' : integer vector indicating the monitor ID numbers to be used
	## Returns a dataframe of the form:
	## id nobs
	## 1 117
	## 2 1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the number of complete cases
	directory = paste(directory, "/", sep="")
	files = dir(directory)
	flag = TRUE
	for(i in id){
		filename = paste(directory,files[i], sep="")
		data = read.csv(filename)
		validrows = !is.na(data[,"nitrate"]) & !is.na(data[,"sulfate"])
		if(flag==TRUE){
			nobs = sum(validrows)
			flag = FALSE
		}
		else{
			nobs = rbind(nobs, sum(validrows))
		}
	}
	df = data.frame(id, nobs, row.names=1:length(id))
	df
}