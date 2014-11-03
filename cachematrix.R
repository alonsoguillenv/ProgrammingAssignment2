makeCacheMatrix <- function( mat = matrix() ) 
{

    inv_mat <- NULL

    set <- function( matrix ) 
    {
            mat <<- matrix
            inv_mat <<- NULL
    }

    set_inverse <- function( inverse ) 
    {
        inv_mat <<- inverse
    }



    get <- function() 
    {
        return( mat )
    }

    get_inverse <- function() 
    {
        return( inv_mat )
    }

    list( set = set, get = get , set_inverse = set_inverse , get_inverse = get_inverse )
}



cacheSolve <- function( x_mat , ... ) 
{

    mat <- x_mat$get_inverse()

    if( !is.null( mat ) ) 
    {
        return( mat )
    }

    data <- x_mat$get()

    mat <- solve(data) %*% data

    x_mat$set_inverse(mat)

    return( mat )
}