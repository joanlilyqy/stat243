# R
#### Stat243
#### Problem Set 2
#### 2. Regular expression

rm(list=ls(all=TRUE)) # remove all objects
load('IPs.RData') # import text data

#patIP <- "([[:digit:]]{1,3}\\.){3}[[:digit:]]{1,3}" #POXIS sytle pattern for IP
patIP <- "((\\d{1,3}\\.){3}\\d{1,3})" #Perl style pattern for IP

getIPnum <- function (t) { if (length(grep(patIP,t,perl=TRUE)) == 0) {return (0)}
                           else {
                           return (length(gregexpr(patIP,t,perl=TRUE)[[1]]))
                           }
                         } # get # of IPs per text element
getIPidx <- function (t) { return (gregexpr(patIP,t,perl=TRUE)[[1]]) } # get index of IPs in text element
getIPsub <- function (i) { return (substring(text[i],ipIdx[[i]],ipIdx[[i]]+attr(ipIdx[[i]],"match.length")-1))} # use idx to get IP strings

ipNum <- sapply(text, getIPnum, USE.NAMES = FALSE) # apply to the character vector
ipIdx <- lapply(text, getIPidx) # maintain list structure of index for each IP log
ipStr <- sapply(1:length(text), getIPsub)  # obtain IPs in list, multiple IPs per element stored in list items

names(ipStr) <- text
cat("The results of # of IPs in each text element (1:100):\n")
print(ipNum[1:100])
cat("The first 10 lines of results:\n")
print(head(ipStr, n= 10))
