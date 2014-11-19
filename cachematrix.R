makeCacheMatrix <- function(m = matrix()) {
  inv_m <- NULL ##init value for the inverse matrix
  set <- function(y) {
    m <<- y ##matrix to be stored
    inv_m <<- NULL ##value in cache for the inverse matrix
  }
  get <- function() m ##gets the matrix to be used
  setmatrix <- function(solve) inv_m <<- solve ##gives the computed inverse of m to inv_m *cached*
  getmatrix <- function() inv_m ##retrieves the value of the inverse matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)## get the functions as a list so they may or not be called in cacheSolve
}

cacheSolve <- function(m=matrix(), ...){
  inv_m<-m$getmatrix() ##retrieves the value of inv_m if it is NOT null then it gives the cached data
  if(!is.null(inv_m)){
    message("getting cached data")
    return(inv_m)
  }
  data <- m$get() ##gets the matrix to be computed
  inv_m <- solve(data, ...) #finds inverse matrix
  m$setmatrix(inv_m) #assigns inv_m to object (cached)
  inv_m 
}
