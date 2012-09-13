# R
#### Stat243
#### Problem Set 2
#### 3. President speech analysis

rm(list=ls(all=TRUE)) #remove all objects

system("mkdir html")
setwd("./html")
system("wget -q -O 'index_pres.html' 'http://www.presidency.ucsb.edu/sou.php#axzz265cEKp1a'")
system("fromdos index_pres.html")
index.pres <- readLines('index_pres.html')

patTxt <- '\\s{16}<td width=\\"\\d{2}\\" align=\\"center\\" class=\\"doclist\\"><a href=\\"'
index.pres <- index.pres[grep(patTxt,index.pres,perl = TRUE)]
index.pres <- sapply(index.pres, function(x){gsub(patTxt,"",x)},USE.NAMES = FALSE)
patTxt2 <- '\\">\\d{4}<\\/a>(\\*|)<\\/td>'
index.pres <- sapply(index.pres, function(x){gsub(patTxt2,"",x)},USE.NAMES = FALSE)
patUrl <- 'http:\\/\\/www\\.presidency\\.ucsb\\.edu\\/ws\\/index\\.php\\?pid='
fileid <- sapply(index.pres, function(x){gsub(patUrl,"",x)},USE.NAMES = FALSE)

print(head(index.pres))
print(head(fileid))

sapply(1:length(fileid),
       function(i){system(paste("wget -q -O '",fileid[i],".html' '",index.pres[i],"'",sep=""))})

setwd("../")
#system("rm -rf html")
