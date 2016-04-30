## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function will do the following:
# 1. Set the Value of Matrix
# 2. Get the Value of Matrix
# 3. Set the Inverse Matrix 
# 4. Get the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
	#-- Inittialize the inversed matrix
	inverseMat <- NULL
	
	#== the Set function, which can be used to pass a base matrix and initialize the Invesed Matrix	
	set <- function (baseMat) {
		x<<-baseMat
		inverseMat<<-NULL
	}	
	
	#== The Get gunction, which can be used to get the value of matrix to be inversed
	get <- function () x
	
	#== The below function will set the Value of Inversed Matrix
	setInverseMatrix <- function (iX) inverseMat <<- iX
	
	#== The function below will return the value of Inversed Matrix
	getInverseMatrix <- function () inverseMat
	
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)	

}


## Write a short comment describing this function

#it will check if the inversed matrix exist and is cached, otherwise it will calculate it and then cache the result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #== Get the Inversed Matrix of x. 
	inverseMat<-x$getInverseMatrix()
	
	#== If the Inversed Matrix is not calculated yet from this function, it will return NULL since the setter of function makeCacheMatrix sets it to NULL
	if(!is.null(inverseMat)){
		message("getting cached data")
		return(inverseMat)
	}
	
	#== If the execution comes at this point, that means we dont have the inverse matrix in the cache so we need to calculate it
	#== Get the Base Matrix 
	matrix<-x$get()
	
	#== Call solve function to calculate the Inverse Matrix
	inverseMat<-solve(matrix, ...)
	
	#== the below code will set the calculate Inverse Matrix to object x and cache it
	x$setInverseMatrix(inverseMat)
	
	#return the inversed matrix
	inverseMat        
}
