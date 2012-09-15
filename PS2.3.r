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
index.pres <- readLines('index_pres.html')
# get speech text source
patTxt <- '\\s{16}<td width=\\"\\d{2}\\" align=\\"center\\" class=\\"doclist\\"><a href=\\"'
index.pres <- index.pres[grep(patTxt,index.pres,perl = TRUE)]
index.pres <- sapply(index.pres, function(x){gsub(patTxt,"",x)},USE.NAMES = FALSE)
patTxt2 <- '\\">\\d{4}<\\/a>(\\*|)<\\/td>'
index.pres <- sapply(index.pres, function(x){gsub(patTxt2,"",x)},USE.NAMES = FALSE)
# get file id from the source url
patUrl <- 'http:\\/\\/www\\.presidency\\.ucsb\\.edu\\/ws\\/index\\.php\\?pid='
fileid <- sapply(index.pres, function(x){gsub(patUrl,"",x)},USE.NAMES = FALSE)
fileid <- sapply(fileid, function(x){paste(x,".html",sep="")})
# download all files and convert to unix
#sapply(1:length(fileid),
#       function(i){system(paste("wget -q -O '",fileid[i],"' '",index.pres[i],"'",sep=""));
#                   system(paste("fromdos ",fileid[i],sep=""))})

# one case for test OR test all files
#ff <- readLines('44541.html')
ff <- sapply(fileid, function(x){readLines(x)})

# get the president name
patName <- "^<title>(.*)<\\/title>"
name.pres <- ff[grep(patName,ff,perl = TRUE)]
name.pres <- sapply(name.pres, function(x){gsub(patName,"\\1",x)},USE.NAMES = FALSE)
name.pres <- sapply(name.pres, function(x){return(unlist(strsplit(x, ":"))[1])},USE.NAMES = FALSE)
# get the talk date
patDate <- '^.*<tr><td.*><span.*>.*<\\/span><br><span class=\\"docdate\\">(.*)<\\/span><\\/td>'
date.talk <- ff[grep(patDate,ff,perl = TRUE)]
date.talk <- sapply(date.talk, function(x){gsub(patDate,"\\1",x)},USE.NAMES = FALSE)
date.talk <- sapply(date.talk, function(x){gsub("^.*\\s","",x)},USE.NAMES = FALSE)
# get the speech content text
patText <- '^\\s{3}<.*><span class=\\"displaytext\\">(.*)<\\/span><.*><.*><i.*>'
speechVec <- ff[grep(patText,ff,perl = TRUE)]
speechVec <- sapply(speechVec, function(x){gsub(patText,"\\1<p>",x)},USE.NAMES = FALSE)

#debug
#cat(length(name.pres)," names\n",head(name.pres),"\n")
#cat(length(date.talk)," dates\n",head(date.talk),"\n")
#cat(length(speechVec)," speeches\n")
#print(speechVec[1]
#for (i in 1:length(fileid)){
#  ff <- readLines(fileid[i])
#  print(i)
#  print(grep(patText,ff,perl = TRUE))
#}

patTag <- '\\[<i>(\\w*)<\\/i>\\]'
getNum <- function(x) {
          if (gregexpr(patTag,x,perl=TRUE)[[1]] == -1){ return(0) }
          else { return(length(gregexpr(patTag,x,perl=TRUE)[[1]]))}
          }
getStr <- function(i){
          return(substring(speechVec[i],attr(tagIdx[[i]],"capture.start"),
                    attr(tagIdx[[i]],"capture.start")+attr(tagIdx[[i]],"capture.length")-1))
          }
tagNum <- sapply(speechVec, getNum,USE.NAMES=FALSE)
tagIdx <- lapply(speechVec, function(x){return(gregexpr(patTag,x,perl=TRUE)[[1]])})
tagStr <- sapply(1:length(speechVec),getStr)

#print(tagNum)
#print(head(tagStr))

speechVec <- sapply(speechVec, function(x){iconv(x,from="WINDOWS-1251",to="UTF-8")})
speechVec <- sapply(speechVec, function(x){gsub(patTag,"",x,perl=TRUE)},USE.NAMES=FALSE)
speechVec <- sapply(speechVec, function(x){gsub("<p>","\n",x)},USE.NAMES=FALSE)
# still have html special char

cat(speechVec[2])

setwd("../")
#system("rm -rf html")
