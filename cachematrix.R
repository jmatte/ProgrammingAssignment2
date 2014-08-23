## makeCacheMatrix is a function that stores a matrix as an imput.
## The result of this function is a list of 3 elements, which can store inverted matrix 
## if this have been calculated. To acces the matrix use "object"$get()

makeCacheMatrix <- function(x = matrix()) {     # input x is a matrix
        m <- NULL                               # m is the inverse of the matrix. Reset each time 
                                                # makeCacheMatrix is called
        set <- function(y) {                    # modification of an stored matrix
                x <<- y
                m <<- NULL                      # m is rest so the inverse matrix can be calculated
        }
        get <- function() {                     # store the original matrix
                x
        }
        setinv <- function(inv) {               # this is called by cacheSolve() the first time
                m <<- inv                       # store the inversed matric using superassignment
        }
        getinv <- function() {                  # store the cached inversed matrix
                m
        }
        list(get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {                # input is an object created by makeCacheMatrix
        m <- x$getinv()                         # accesses the object 'x' to get inversed matrix
        if(!is.null(m)) {                       # TRUE if the inversed matrix was created before
                message("getting cached data")  
                return(m)                       # print the inversed matrix and exit cacheSolve
        }                                       # in FALSE, the inversed matrix will be calculated
        data <- x$get()                         # the original matrix is store in data
        m <- solve(data)                        # the inversed matrix is calculated
        x$setinv(m)                             # the inversed matrix is stored in the list 
                                                # created by makeCacheMatrix
        m                                       # the inversed matrix is printed
}

#####################################################################################################

## Here is a test to check if the function returns an inversed matrix

# invertible matrix example

n <- makeCacheMatrix(matrix(c(1,1,1,3,4,3,3,3,4),3,3))
n$get()
cacheSolve(n)
# > Inversed matrix
cacheSolve(n)
# > "getting cached data" and inversed matrix

# test if inversed matrix is correct, M * M-1 = I. Should result in the identity matrix
n$get() %*% n$getinv()

# or using this function will return TRUE if M * M-1 = I
CheckInv <- function(mx){
        mdim <- dim(mx$get())[1]
        Identity <- matrix(0,mdim,mdim)
        for(i in 1:mdim){
                for(j in 1:mdim){
                        if(i==j){
                                Identity[i,j] <- 1
                        } else {
                                Identity[i,j] <- 0
                        }
                }
        }
        all.equal(mx$get() %*% mx$getinv(), Identity)
}

CheckInv(n)
