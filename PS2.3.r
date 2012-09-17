# R
#### Stat243
#### Problem Set 2
#### 3. President speech analysis

rm(list=ls(all=TRUE)) #remove all objects
#system("mkdir html")
setwd("./html")

# Download all html files
#system("wget -q -O 'index_pres.html' 'http://www.presidency.ucsb.edu/sou.php#axzz265cEKp1a'")
#system("fromdos index_pres.html")
indexPres <- readLines('index_pres.html',warn=FALSE)
# get speech text source
patUrl1 <- '\\s{16}<td width=\\"\\d{2}\\" align=\\"center\\" class=\\"doclist\\"><a href=\\"'
indexPres <- indexPres[grep(patUrl1,indexPres,perl = TRUE)]
indexPres <- sapply(indexPres, function(x){gsub(patUrl1,"",x)},USE.NAMES=FALSE)
patUrl2 <- '\\">\\d{4}<\\/a>(\\*|)<\\/td>'
indexPres <- sapply(indexPres, function(x){gsub(patUrl2,"",x)},USE.NAMES=FALSE)
# get file id from the source url
patUrl3 <- 'http:\\/\\/www\\.presidency\\.ucsb\\.edu\\/ws\\/index\\.php\\?pid='
fileid <- sapply(indexPres, function(x){gsub(patUrl3,"",x)},USE.NAMES=FALSE)
# download all files and convert to unix
#sapply(1:length(fileid),
#       function(i){system(paste("wget -q -O '",fileid[i],"' '",indexPres[i],"'",sep=""));
#                   system(paste("fromdos ",fileid[i],sep=""))})




# one case for test OR test all files
#ff <- readLines('44541.html')
ff <- sapply(fileid, function(x){readLines(paste(x,".html",sep=""), warn=FALSE)})

# get the president name
patName <- "^<title>(.*?)<\\/title>"
namePres <- ff[grep(patName,ff,perl = TRUE)]
namePres <- sapply(namePres, function(x){gsub(patName,"\\1",x)},USE.NAMES=FALSE)
namePres <- sapply(namePres, function(x){return(unlist(strsplit(x, ":"))[1])},USE.NAMES=FALSE)
names(namePres) <- fileid
# get the talk date
patDate <- '^.*<span class=\\"docdate\\">(.*?)<\\/span>.*$'
dateTalk <- ff[grep(patDate,ff,perl = TRUE)]
dateTalk <- sapply(dateTalk, function(x){gsub(patDate,"\\1",x)},USE.NAMES=FALSE)
dateTalk <- sapply(dateTalk, function(x){gsub("^.*\\s","",x)},USE.NAMES=FALSE)
names(dateTalk) <- fileid
# get the speech content text and prune for nice-format print
patText <- '^.*<span class=\\"displaytext\\">(.*?)<\\/span>.*$'
speechVec <- ff[grep(patText,ff,perl = TRUE)]
speechVec <- sapply(speechVec, function(x){gsub(patText,"\\1",x)},USE.NAMES=FALSE) # grab speech text
speechVec <- sapply(speechVec, function(x){gsub("(<p.*?>|<\\/p>|<br>)","\n",x)},USE.NAMES=FALSE) # for line ending
speechVec <- sapply(speechVec, function(x){gsub("<.*?>","",x)},USE.NAMES=FALSE) # remove all html format
speechVec <- sapply(speechVec, function(x){x <- gsub("&mdash;"," -- ",x);
                                           x <- gsub("&nbsp;","  ",x);
                                           x <- gsub("&lsquo;"," ' ",x);
                                           x <- gsub("&#8226;"," \\. ",x);
                                           x <- gsub("&lt;"," < ",x);
                                           x <- gsub("&deg;"," degree ",x);
                                           x <- gsub("&pound;"," pound ",x);                                           
                                           x <- gsub("&fra.*?;"," 1/2 ",x);                                           
                                           x <- gsub("&O.*?;","O",x);
                                           x <- gsub("&e.*?;","e",x);                                     
                                          },USE.NAMES=FALSE) # html special char
names(speechVec) <- fileid
# remove audience response tags (laughter & applause)
patLau <- '\\[.*?(Laughter|laughter).*?\\]'
patApp <- '\\[.*?(Applause|applause).*?\\]'
getlauNum <- function(x) {
          if (length(gregexpr(patLau,x,perl=TRUE)[[1]]) == 1 &&
              gregexpr(patLau,x,perl=TRUE)[[1]] == -1)         { return(0) }
          else { return(length(gregexpr(patLau,x,perl=TRUE)[[1]]))}
          }
getappNum <- function(x) {
          if (length(gregexpr(patApp,x,perl=TRUE)[[1]]) == 1 &&
              gregexpr(patApp,x,perl=TRUE)[[1]] == -1)         { return(0) }
          else { return(length(gregexpr(patApp,x,perl=TRUE)[[1]]))}
          }
#getStr <- function(i){
#          return(substring(speechVec[i],attr(tagIdx[[i]],"capture.start"),
#                    attr(tagIdx[[i]],"capture.start")+attr(tagIdx[[i]],"capture.length")-1))
#          }
lauNum <- sapply(speechVec, getlauNum)
appNum <- sapply(speechVec, getappNum)
#tagIdx <- lapply(speechVec, function(x){return(gregexpr(patTag,x,perl=TRUE)[[1]])})
#tagStr <- sapply(1:length(speechVec),getStr)

speechVec <- sapply(speechVec, function(x){x <- gsub(patLau,"",x,perl=TRUE);
                                           x <- gsub(patApp,"",x,perl=TRUE)})

#debug
#print(namePres)
#print(dateTalk)
#talk.len <- sapply(speechVec, function(x){nchar(as.character(x))})
#print(talk.len)
print(lauNum)
print(appNum)
#print(tagStr[1:25])


speechVec <- sapply(speechVec, function(x){iconv(x,from="WINDOWS-1251",to="UTF-8",sub = " ")})
sapply(1:length(speechVec), function (i){cat("START:",fileid[i],"\n",speechVec[i],"\n\nEND\n\n",file="all.html",append=TRUE)}, USE.NAMES=FALSE)



setwd("../")
#system("rm -rf html")
