## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## a function to cache the inverse of a matrix


#function
makeCacheMatrix <- function(x = matrix()) {
      
            #stores cache value      
            cache <- NULL
            
            #matrix for working environment
            set <- function(y) {
                  x <<- y
                  cache <<- NULL
            }
            
            #get the matrix
            get <- function() x
            
            #set inverse of matrix in cache
            setinverse <- function(inverse){
                  #inverse the matrix
                  inverseMatrix <- solve(inverse)%*%inverse
                  #set inverse in cache
                  cache <<- inverseMatrix
            }
            
            #get inverse of matrix from cache (note: no arguments in function)
            getinverse <- function() cache
            
            list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)


}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      #set inverse of matrix in cache
      setinverse <- function(inverse){
            #inverse the matrix
            inverseMatrix <- solve(inverse)%*%inverse
            #set inverse in cache
            cache <<- inverseMatrix
      }
      
      getinverse <- function() cache
      
      cache <- x$getinverse()
      
      #if data is cached
      if(!is.null(x)){
            
            print("Getting cached Data.")
            
            return(cache)
            
      }
      #if data is not cached
      else{
            setinverse(cache)
            x$setinverse(cache)
            
      }
}

