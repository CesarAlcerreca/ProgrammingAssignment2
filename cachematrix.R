## Put comments here that give an overall description of what your functions do
# These two functions work together to store in the cache memory a Matrix and its
# inverse, with the purpose of not calculating it again and again if we have already
# done it previously.


## Write a short comment describing this function
# Here in the MakeCacheMatrix we are storing a new Matrix in the cache,
# (instant memory), with the variable named "cache". The function has 4 subfunctions
# one sets the cache, one gets the cache, one sets the inverse of the cache
# and another one gets the inverse of the cache. The functions come together to
# make the cacheSolve function work.


makeCacheMatrix <- function(x = matrix()) {
      cache <- NULL
      set <- function(y){
            x <<- y
            cache <<- NULL
      }
      
      get <- function() {x}
      setinverse <- function(inverse) { cache <<- inverse}
      getinverse <- function() {cache}
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
#The cacheSolve function first checks if the matrix it took as an argument
#has already been solved before, if yes it skips the calculation and gives the 
# inverse, if not it makes the calculation and sets the inverse in the makeCacheMatrix
# for further use. :)

cacheSolve <- function(x, ...) {
      cache <- x$getinverse()
      if(!is.null(cache)) {
            message("getting cached data")
            return(cache)
      }
      mat <- x$get()
      cache <- solve(mat, ...)
      x$setinverse(cache)
      cache
}
        ## Return a matrix that is the inverse of 'x'
