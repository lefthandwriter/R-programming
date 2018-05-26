corr<- function(directory, threshold=0){
	## 'directory': folder name indicating location of csv files
	## 'threshold': a numeric vector of length 1 indicating the number of completely observed
	##				observations (on all variables) required to compute the correlation between
	##				nitrate and sulfate
	## Returns a numeric vector of correlations
	correlations = c()
	flag = TRUE
	directory = paste(directory, "/", sep="")
	files = dir(directory)
	for(f in files){
		data = read.csv(paste(directory,f, sep=""))
		nobs = sum( !is.na(data[,"sulfate"]) & !is.na(data[,"nitrate"])) 
		corvalue = cor(data[,"sulfate"], data[,"nitrate"], use="pairwise.complete.obs")
		if(nobs>=threshold){
			if (flag==TRUE){
				correlations = corvalue
				flag = FALSE
			}
			else{
				correlations = c(correlations, corvalue)
			}
		}
	}
	correlations
}