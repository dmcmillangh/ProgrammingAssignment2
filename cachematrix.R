## First function creates a list of functions, performing no real work
## expect a matrix, square, and not ill-conditioned

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ##matrix(NULL,nrow=nrow(x),ncol(x))
  
##  print(inv)
    set <- function(y=matrix()) {
        x <<- y
        inv <<- NULL ##matrix(NULL,nrow=nrow(x),ncol(x))
    }
    get <- function() x
  
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
  
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## use cacheSolve() instead of native function solve(), as it caches the solution at the first run, 
## merely retrieving it in subsequent calls

## this function decides whether to do the real work, 
## or just retreive work (calc'ing the inverse) already done

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
  ## Return a matrix that is the inverse of 'x'
  ## First see if we already have it  
    if(!is.null(inv)){
      message("retrieving Inverse")
      return (inv)
  }
## otherwise, calculate it
  xmat<-x$get()
  inv<-solve(xmat)
  x$setinv(inv)
  return(inv)
  }
