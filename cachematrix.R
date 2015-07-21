## Put comments here that give an overall description of what your
## functions do

## This function returns an object (supported by a R list data structure), 
## that will be used to store a matrix as well as its inverse.
makeCacheMatrix <- function(baseMatrix = matrix()) {
  
  # used to store the inverted Matrix
  invertedMatrix <- NULL
  
  # Method used to store the un-inverted matrix into the data structure
  setMatrix <- function(bMatrix) {
    baseMatrix <<- bMatrix
    invertedMatrix <<- NULL
  }
  
  # Method used to get the un-inverted matrix from the data structure
  getMatrix <- function() {
    baseMatrix
  }

  # Method used to store the inverted matrix into the data structure
  setInvertedMatrix <- function(iMatrix) {
    invertedMatrix <<- iMatrix
  }

  # Method used to get the inverted matrix from the data structure
  getInvertedMatrix <- function() {
    invertedMatrix
  }
  
  # Returning a list object with all the defined functions inside.
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInvertedMatrix = setInvertedMatrix,
    getInvertedMatrix = getInvertedMatrix
  )

}


## This function returs the inverted matrix for the matrix stored on the cacheMatrix. 
## If the inverted matrix was already calculated, the previous calculated value will 
## be returned. Otherwise it will calculate the inverted matrix, using R's solve function 
## stored it in the cacheMatrix and return the inverted matrix as the return value.
cacheSolve <- function(cachedMatrix) {
  
  # Getting that cached value
  invertedMatrix <- cachedMatrix$getInvertedMatrix()
  
  if (is.null(invertedMatrix)) {
    # If was not already calculated, calculate the matrix's inverse, store it on the
    # data structure.
    baseMatrix <- cachedMatrix$getMatrix()
    invertedMatrix <- solve(baseMatrix)
    cachedMatrix$setInvertedMatrix(invertedMatrix)
  } else {
    # Otherwise show a message to the user
    message("Getting Cached data.")
  }
  return(invertedMatrix)
}


## Test cases...
test.getMatrix <- function() {
  bMatrix <- matrix(1:4, 2, 2)
  cMatrix <- makeCacheMatrix(bMatrix)
  checkEquals(bMatrix, cMatrix$getMatrix())
}

test.setMatrix <- function() {
  bMatrix <- matrix(1:4, 2, 2)
  cMatrix <- makeCacheMatrix()
  cMatrix$setMatrix(bMatrix)
  checkEquals(bMatrix, cMatrix$getMatrix() )
}

test.noInverse <- function() {
  bMatrix <- matrix(1:4, 2, 2)
  cMatrix <- makeCacheMatrix(bMatrix)
  checkIdentical(cMatrix$getInvertedMatrix(), NULL)
}

test.cachingInverse <- function() {
  bMatrix <- matrix(1:4, 2, 2)
  
  cMatrix <- makeCacheMatrix(bMatrix)
  checkIdentical(cMatrix$getInvertedMatrix(), NULL)
  
  solved <- cacheSolve(cMatrix)
  checkIdentical(cMatrix$getInvertedMatrix(), solved)
}

## Testing...
## library(RUnit)
##
## tSuite  <- defineTestSuite("Cache Matrix Test Suite", dirs=".", testFileRegexp = "^.+\\.[rR]$")
## tResult <- runTestSuite(tSuite)
## printTextProtocol(tResult, showDetails = TRUE)
## 
## Inspecting some tests
##
## track <- tracker()
## track$init()
## inspect(test.cachedInverse(), track=track)
## resTrack <- track$getTrackInfo()
## printHTML.trackInfo(resTrack) ; ## create HTML sites
## 
