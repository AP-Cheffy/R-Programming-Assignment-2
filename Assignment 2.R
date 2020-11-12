

## The makeCachematrix creates a list of needed matrix required to complete the inversion. 
##First, we create the value of the matrix and obtain its value. 
## Second, we create and define the inverse matrix and the obtain inversion matrix 
## The four list that are create 
makeCachematrix <- function(x = matrix()){
    m <- NULL
    set <- function(y){
        
        x <<- y
        m <<- NULL
    
    }
    getfun <- function() x
    setinversefunc <- function(inv) m <<- inv
    getinvfunc <- function() m
    list(set = set, get = getfun, setinv = setinversefunc, getinv = getinv)
}

## The CacheInv takes checks if the inverse matrix has been calcultated. If 'm' is NOT empty 
CacheInv <- function(x,...){
    
    m <- x$getinvfunc()
    
    ## If m is not empty (null), we get the message that R is searching for the previously 
    # define "m" matrix. 
    if (!is.null(m)){
        message("getting cached data")
    return(m)
    }
    ## If "m" is indeed null the computer will calculate the inverse matrix m 
    else{
    xdta <- x$getfun
    m <-inv(xdta,...)
    x$setinversefunc
    m
    }
    
}
