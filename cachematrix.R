## Here we have two functions used to cache the inverse matrix 
## to the function input matrix. On repeating call of inverese 
## matrix to the same input matrix, is the result read from cache memory
## without need of calculation.

## Function creates a object that can cache matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function computes the inverse of matrix x returned by makeCacheMatrix 
## If the inverse has already been calculated for same matrix, then 
## function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() #get inverse of matrix x (null if not calculated before)
  if(!is.null(inv)) { #if not null (which means it was calculated before (once))
    message("getting cached inverse matrix") #print info message
    return(inv) #return before calculated value from chache memory
  } #next lines are executed only if inv == null (so wasn't just returned becouse isn't calculated yet)
  data <- x$get() #get input matrix used to calcualation
  inv <- solve(data) #calculate inverse matrix to matrix x
  x$setinverse(inv) #set calculated inverse matrix, so it can be read from cache in future without need of calcualtion
  inv #return inverse matrix
}
