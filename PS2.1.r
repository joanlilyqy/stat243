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
# Define printing control global parameter
wid = 20		# object name priting width
digit = 10	# byte size printing digits

# print out file size according to auto-match units
printUnit <- function (obj) { 
            	units = c("", "k", "M", "G")
            	i = 1; while (obj >= 1024) { obj = obj / 1024; i = i + 1;} # get proper units
            	if (i == 1) { 
            		cat(format(obj,width = digit,justify = 'right'),"\n",sep = "") 
            	}
            	else { 
            		cat(format(as.integer(obj),width = digit - 1,justify = 'right'),units[i],"\n",sep = "") 
            	}
            }
# print out the object list nicely           
printObj <- function (objList, objName, pretty) { 
            	cat(format("object",width = wid),format("bytes",width = digit,justify = 'right'),"\n",sep = "")
            	if (pretty == FALSE) {
            		sapply(1:length(objList),
            			function(i){cat(format(objName[i],width=wid),format(objList[i],width=digit,justify='right'),"\n",sep="")})
            	}
            	else{
            		sapply(1:length(objList),
            			function(i){cat(format(names(objList)[i], width = wid));printUnit(objList[i])})
            	}
            }


# Main Func:
lsObj <- function (numls = 100, sizeB = 0, pretty = FALSE) {	
					listLs <- ls(envir = parent.frame()) # object list
         	sizeLs <- sapply(listLs, 
         	       	  function(x){object.size(get(x,envir = parent.frame(n = 4)))}) # get all object sizes from in the called frame
                                                                                  # FUN -> sapply -> lsObj -> call(lsObj) | n=4

					tmpEnv <- NULL; nameEnv <- NULL # store object sizes and names within a certain user-defined ENV
					tmpFun <- NULL; nameFun <- NULL # store object sizes and names within a function closure
					for (item in listLs){ # elborate objects to find hidden ones
					  objitem <- get(item, envir = parent.frame())
					  # get user-defined ENV
						if (is.environment(objitem)) { 
							tt <- sapply(ls(envir = objitem), 
							             function(x){object.size(get(x,envir = objitem))})
							tmpEnv <- c(tmpEnv,tt) 
							nameEnv <- c(nameEnv, paste(item,"$",ls(envir = objitem),sep = ""))
						}
						# get FUN closure
						if (is.function(objitem) && identical(environment(objitem), parent.frame())== FALSE ){ 
							tt <- sapply(ls(envir = environment(objitem)), 
							             function(x){object.size(get(x,envir = environment(objitem)))})
							tmpFun <- c(tmpFun,tt)
							nameFun <- c(nameFun, paste(item,"$",ls(envir = environment(objitem)),sep = ""))
						}
					}
					names(tmpEnv) <- nameEnv; names(tmpFun) <- nameFun
         	if (length(tmpEnv) > 0) { sizeLs <- c(sizeLs,tmpEnv) } #if there is use-defined ENV
        	if (length(tmpFun) > 0) { sizeLs <- c(sizeLs,tmpFun) } #if there is FUN closure
         	
         	sizeLs <- sizeLs[sizeLs[] > sizeB] # find objects larger than sizeB(bytes)
         	if (numls > length(sizeLs)) { numls = length(sizeLs) }
         	sizeLs <- sort(sizeLs, decreasing = TRUE)[1:numls] # find the numls largest objects
         	nameLs <- names(sizeLs) # get the name of vector

          printObj(sizeLs,nameLs,pretty)
         }



#### Test case
cat("**********Test Case 1*************\nlsObj(5,128)\n")
lsObj(5,128)
cat("**********Test Case 2*************\nlsObj(sizeB = 1024)\n")
lsObj(sizeB = 1024)
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
