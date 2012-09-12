# R
#### Stat243
#### Problem Set 2
#### 2. Regular expression

rm(list=ls(all=TRUE)) #remove all objects
load('IPs.RData')

#	POXIS style
# "([[:digit:]]{1,3}\\.){3}[[:digit:]]{1,3}"
patIP <- "(\\d{1,3}\\.){3}\\d{1,3}" #Perl style

#getIPnum <- function (t) { return (length(gregexpr(patIP,t,perl = TRUE)[[1]])) }
#ipNum <- sapply(text, getIPnum, USE.NAMES = FALSE)

ips <- NULL
for (i in 1:length(text)){
	x <- gregexpr(patIP,text[i],perl = TRUE)[[1]]
	if (length(x) == 1) {
		if (x != -1 && is.na(x) == FALSE) {
			ips <- c(ips, substring(text[i],x,x+attr(x,"match.length")-1)) 
		}
	}
	else {
		for (j in 1:length(x)){
			ips <- c(ips, substring(text[i],x[j],x[j]+attr(x,"match.length")[j]-1))
		}
	}
}

#print(length(ips))
