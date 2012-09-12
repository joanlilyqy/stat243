# R
#### Stat243
#### Problem Set 2
#### 1. list object size

rm(list=ls(all=TRUE)) #remove all objects

#### Generate objects test cases
ob0 <- rnorm(1)
ob1 <- rnorm(10)
ob2 <- rnorm(100)
ob3 <- rnorm(1000)
ob4 <- rnorm(10000)
ob5 <- rnorm(100000)
ob6 <- rnorm(1000000)
ob7 <- rnorm(10000000)

# Aux Printing Fun:
printUnit <- function (obj) { # print out file size according to auto-match units
            	units = c("", "k", "M", "G"); digit = 10
            	i = 1; while (obj >= 1024) { obj = obj / 1024; i = i + 1;}
            	if (i == 1) { cat(format(obj,width = digit,justify = 'right'),"\n",sep = "") }
            	else { cat(format(as.integer(obj),width = digit - 1,justify = 'right'),units[i],"\n",sep = "") }
            }
           
printObj <- function (objList) { # print out the object list nicely
            	wid = 20; digit = 10
            	cat(format("object",width = wid),format("bytes",width = digit,justify = 'right'),"\n",sep = "")
         	    for (i in 1:length(objList)) {
         	  		cat(format(names(objList)[i],width = wid),format(objList[i],width = digit,justify = 'right'),"\n",sep = "")
         	    }
            }

printObjPretty <- function (objList) { # print out the object list NICELY&PRETTY
                  	wid = 20; digit = 10
                  	cat(format("object",width = wid),format("bytes",width = digit,justify = 'right'),"\n",sep = "")
         	          for (i in 1:length(objList)) {
         	          	cat(format(names(objList)[i], width = wid))
#        	          	print(object.size(get(names(objList)[i])), units = "auto") #class built-in method, messy
         	          	printUnit(objList[i]) #self-defined method, exact match for PS eg
         	          }
                  }


# Main Func:
lsObj <- function (numls = 100, sizeB = 0, pretty = FALSE) {	
					listLs <- ls(envir = parent.frame()) # object list
         	tmp <- sapply(listLs, function(x){object.size(get(x,envir = parent.frame(n = 4)))}) #get all object sizes from in the called frame
                                                                                              # FUN -> sapply -> lsObj -> call(lsObj) | n=4
					
					tmpEnv <- NULL # store object sizes within a certain user-defined ENV
					nameEnv <- NULL # store the ENV name and object names within ENV
					tmpFun <- NULL # store object sizes within a function closure
					nameFun <- NULL # store the FUN name and object names within FUN
					
					for (item in listLs){
					  objitem <- get(item, envir = parent.frame())
						if (is.environment(objitem)) { # get user-defined ENV
							listEnv <- ls(envir = objitem)
							for (tmpItem in listEnv) { # objects in user-defined ENV
                 tmpEnv <- c(tmpEnv, object.size(get(tmpItem,envir = objitem)))
							}
							nameEnv <- c(nameEnv, paste(item,"$",listEnv,sep = ""))
						}
						if (is.function(objitem) && identical(environment(objitem), parent.frame())== FALSE ){ # get FUN closure
							listFun <- ls(envir = environment(objitem))
							for (tmpItem in listFun) { # objects in FUN closure
								tmpFun <- c(tmpFun, object.size(get(tmpItem,envir = environment(objitem))))
							}
							nameFun <- c(nameFun, paste(item,"$",listFun,sep = ""))
						}
					}
					names(tmpEnv) <- nameEnv
					names(tmpFun) <- nameFun
					
         	if (length(tmpEnv) > 0) { tmp <- c(tmp,tmpEnv) } #if there is use-defined ENV
        	if (length(tmpFun) > 0) { tmp <- c(tmp,tmpFun) } #if there is FUN closure
         	tmp <- tmp[tmp[] > sizeB] # find objects larger than sizeB(bytes)
         	if (numls > length(tmp)) { numls = length(tmp) }
         	tmp <- sort(tmp, decreasing = TRUE)[1:numls] # find the numls largest objects
         	
         	if (pretty == TRUE) { printObjPretty(tmp) } # print PRETTY
         	else { printObj(tmp) }
         }



#### Test case
cat("**********Test Case 1*************\nlsObj(5,64)\n")
lsObj(5,64)
cat("**********Test Case 2*************\nlsObj(sizeB = 1000)\n")
lsObj(sizeB = 1000)
cat("**********Test Case 3*************\nlsObj(numls = 3)\n")
lsObj(numls = 3)
cat("**********Test Case 4*************\nlsObj(pretty = TRUE)\n")
lsObj(pretty = TRUE)
cat("**********Test Case 5*************\nlsObj() in testFunc()\n")
testFunc <- function () {
            	data <- rnorm(10000)
            	data2 <- rnorm(100000)
            	myFun <- function(theta) { dist <- rnorm(theta); return(exp(dist / theta)); }
	lsObj()
}
testFunc()
cat("**********Test Case 6*************\nlsObj() in test testEx()\n")
testEx <- function () {
          	x <- rnorm(100000)
          	data <- rnorm(1000)
          	e <- new.env(); e$a <- x # an object hidden in an environment
          	e2 <- new.env(); e2$a2 <- x; e2$b2 <- 52
          	myFun <- function(theta) { dist <- rnorm(theta); return(exp(dist / theta)); }
          	myFun1 <- function(theta) { dist <- rnorm(theta); return(exp(dist / theta)); }
          	myFun2 <- with(list( data = x ), # an object hidden in a closure
                          function(theta) { dist <- rdist(data); return(exp(dist / theta)); } )
            myFun3 <- with(list( data2 = x ),
                          function(theta2) { dist <- rdist(data); return(exp(dist / theta2)); } )
	lsObj()
}
testEx()
