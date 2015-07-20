## Put comments here that give an overall description of what your
## functions do
library(RUnit)

## This function create a object supported by a R list data structure, that will be used to store a matrix and its inverse.
makeCacheMatrix <- function(baseMatrix = matrix()) {
  #used to store the inverted Matrix
  invertedMatrix <- NULL
  setMatrix <- function(bMatrix) {
    baseMatrix <<- bMatrix
    invertedMatrix <<- NULL
  }
  getMatrix <- function() {
    baseMatrix
  }
  setInvertedMatrix <- function(iMatrix) {
    invertedMatrix <<- iMatrix
  }
  getInvertedMatrix <- function() {
    invertedMatrix
  }
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInvertedMatrix = setInvertedMatrix,
    getInvertedMatrix = getInvertedMatrix
  )
}


## Write a short comment describing this function

cacheSolve <- function(cachedMatrix) {
  invertedMatrix <- cachedMatrix$getInvertedMatrix()
  if (!is.null(invertedMatrix)) {
    message("Getting Cached data.")
    return(invertedMatrix)
  }
  baseMatrix <- cachedMatrix$getMatrix()
  invertedMatrix <- solve(baseMatrix)
  cachedMatrix$setInvertedMatrix(invertedMatrix)
  invertedMatrix
  ## Return a matrix that is the inverse of 'x'
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
