## The following code contain two functions. The first function creates a matrix (set) and can cache its inverse (setinverse).
##The second function calculates the value of the inverse if it has not already been calculated or
##retrieve the inverse from the cache

## #The following  function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
	i<- NULL
	set <- function(y) { #to set the value of the matrix
		x <<- y 
		i <<- NULL
		}
		get <- function() {
			x #Returns original matrix
			}
		setinverse<- function(solve){ #to set the value of the inverse for the given matrix
			i <<- solve
			}
		getinverse <- function() { #Returns matrix inverse
			i
			}
	
	list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
}



## The following function computes the inverse of the matrix returned by the makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
	
	#In the case that the inverse has already been calculated and the matrix has not changed, cachesolve retrieve the inverse from the cache
	if(!is.null(i)) { #verify that the inverse of the matrix has already been calculated
		message("getting cached data")
		return(i) #return the value of the inverse
		}
	
	#In the case that the inverse has NOT been calculated, the function calculates the inverse	
	data <- x$get() #gets the matrix of the makeCacheMatrix function
	i <- solve(data, ...) #computes the inverse
	x$setinverse(i) #to set the value of the inverse
	i #returns the value of the inverse
	}
