## The two function together creat a special object for storing inverse of a matrix
## The inverse is stored in memory and can be retrieved at any time
## It does not haveto be calculated every time you need it

## This function creates a special object containing the functions to
## set and get the value of a matrix, and set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL 			## reserves a variable for the inverse
      set<-function(y){
            x <<- y    			## sets the value of x in parent environment
            i <<- NULL 			## resets inverse
      }
      get <- function() x                         	## retrieves x
      setinverse <- function(inverse) i<<-inverse 	## sets inverse
      getinverse <- function() i                  	## retrieves inverse
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse) 			## names for the functions
}


## The second function uses the sub-functions in makeCacheMatrix, calling them by name
## It calculates the inverse of a matrix, unless the value is already cached
## In that case it retrieves it from memory

cacheSolve <- function(x, ...) {
      i <- x$getinverse() 			## gets inverse from the makeCacheMatrix object
      if(!is.null(i)) {   			## checks if the inverse is cached, if true, returns it
            message("getting cached data")
            return(i)
      }                  			## if the inverse is not cached:
      data <- x$get()    			## gets the matrix
      i <- solve(data, ...) 			## assings its inverse to i
      x$setinverse(i)    			## stores i in cache
      i                  			## and returns it
}
