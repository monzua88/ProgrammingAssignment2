## This function takes a "matrix" object (a list) as an
## argument and calculates its inverse.
## If the inverse has been calculated before, the function
##returns the inverse stored in cache.

## This function constructs a list ("matrix") that uses 
## as cacheSolve parameter to calculate the inverse of the matrix.
makeCacheMatrix <- function(x=matrix()) { 
        m <- NULL #clear m
        set <- function(y){  
                x <<- y #asign y to x (parent enviroment)
                m <<- NULL #clear m (parent enviroment)
        }
        get <- function() x #ftakes x from parent enviroment
        setinverse <- function(inverse) m <<-inverse #assign inverse to m (parent enviroment)
        getinverse <- function() m 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        #make a list with the functions to acces to them with $ 
}  

## cacheSolve calcula la inversa del objeto construido por makeCacheMatrix. 
##Si lo ha calculado antes, muestra el inverso guardado en cache.                              
cacheSolve <- function(x,...) { 
        m <- x$getinverse() #takes getinverse from makeCacheMatrix
        if(!is.null(m)) { 
                message("getting catched data") 
                return(m) 
        }
        data <- x$get() #assign x to data
        m <- solve(data,...)#assign the inverse to m
        x$setinverse(m)
        return(m)
}