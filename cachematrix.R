#####Cache Inverse of Matrix#####

## Part 1
# set up a function 'makeMatrix' that will store other functions in the cache
# for later use

makeMatrix <- function(x = matrix()) { 
    # 'makeMatrix' is a function that takes argument x, where x is a matrix
    
    m <- NULL # create an empty variable that will filled later
    
    set <- function(y) { 
        x <<- y
        m <<- NULL
        # the function 'set' is used if you want to change the specified matrix.
        # it changes the original matrix x to matrix y, and resets 'm' as NULL
    }
    
    get <- function() x # function 'get' calls the matrix x
    
    setinverse <- function(solve) m <<- solve 
    # 'setinverse' stores variable 'm' in the cache (which will be the value of 
    # the inverse matrix once it is calculated in cacheinverse and stored in 
    # the cache)
    
    getinverse <- function() m # return the value m
    
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse) 
    # 'makeMatrix' returns a list of all four functions, where the first object 
    # of the list is 'set', second object of list is 'get', 3rd object is 
    # 'setinverse', and 4th is 'getinverse'. 
   
} 
# we do all this so that when we assign makeMatrix, the object will
# have all four functions, so that they can be called later in cacheinverse
# using the subset '$'


## Part 2
# set up a function that will either return the inverse (m) of a matrix 
# (say, matrix x) that has already been cached or, if m is NULL (empty), it will
# calculate the inverse of the new matrix (say, matrix y) and store that inverse  
# in the cache. so when you call cacheinverse(y), R doesn't have to calculate  
# the inverse again, it can look it up in the cache

cacheinverse <- function(x, ...) {
    # function 'cacheinverse' takes argument x
    
    m <- x$getinverse() #calls the function 'getinverse' to return value m
    
    if(!is.null(m)) { 
        message("getting cached data")
        return(m) 
        #if the value m is not NULL (has a value), then return the message 
        #"getting cached data" and return the value of m 
    }
    
    # but what if m is NULL?
    # if m is NULL, the inverse of the matrix hasn't been calculated and stored
    # in the cache yet
    
    data <- x$get()
    #if the value m is NULL (inverse has not been calculated or cached), then 
    #x$get() calls the function get() from 'makeMatrix', which calls the 
    # specified matrix and stores it in variable 'data' 
    
    m <- solve(data, ...) 
    # the inverse of 'data' is calculated and stored as the variable 'm'
    
    x$setinverse(m) 
    # stores the new value 'm' of 'cacheinverse(new matrix)' in the cache. this 
    # is because we are calling the function 'setinverse' from 'makeMatrix',
    # which stores 'm' in the cache. this makes it possible for R to look for 
    # 'm' (where 'm' is a previously calculated and stored value) in the cache 
    # instead of calculating the inverse again
    
    m # return 'm'
}

## Example:
# c <- rbind(c(1, -1/4), c(-1/4, 1))
# x <- makeMatrix(c)
# cacheinverse(x)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667