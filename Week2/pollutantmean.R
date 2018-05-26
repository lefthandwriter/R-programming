pollutantmean <- function(directory, pollutant, id=1:332){
	## 'directory' : a character vector of length 1 indicating the location of the csv files
	## 'pollutant' : a character vector of length 1 indicating the name of the pollutant for which we will calculate the mean; either "sulfate" or "nitrate"
	## 'id' : an integer vetor indicating the monitor ID numbers to be used
	## Return the mean of the pollutant across all monitors list in the 'id' vector (ignore NA values)	
	directory = paste(directory, "/", sep="")
	files = dir(directory)
	flag = TRUE
	for(i in id){
		filename = paste(directory,files[i], sep="")
		data = read.csv(filename)
		validrows = !is.na(data[,pollutant])
		if(flag==TRUE) {
			num_elements = sum(validrows)
			id_mean = mean(data[,pollutant], na.rm=TRUE)
			flag = FALSE
		}
		else{
			num_elements = cbind(num_elements, sum(validrows))
			id_mean = cbind(id_mean, mean(data[,pollutant], na.rm=TRUE))	
		}
	}
	weighted.mean(id_mean, num_elements)
}