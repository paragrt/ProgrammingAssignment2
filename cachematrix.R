## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setmatinv <- function(matinv) m_inv <<- matinv
  getmatinv <- function() m_inv
  list(set = set, get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)
}



## calculates & caches the inverse using the "solve" function

cacheSolve <- function(x, ...) {
  m_inv <- x$getmatinv()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setmatinv(m_inv)
  m_inv
}
