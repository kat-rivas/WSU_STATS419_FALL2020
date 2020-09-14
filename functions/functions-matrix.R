# functions-matrix

# transposeMatrix
#
# transposes a matrix
#
# @param mat matrix
# @return matrix, transposed
transposeMatrix=function(mat)
{ 
  t(mat);
}


# rotateMatrix90
#
# rotates a 3x3 matrix 90 degrees
# - uses/multiplied with a transformation matrix
# - and transpose function
#
# @param mat matrix
# @return rotated matrix
rotateMatrix90=function(mat)
{
  tMatrix=matrix(c(0,0,1,
                   0,1,0,
                   1,0,0), nrow=3, byrow=T)
  x=tMatrix%*%mat
  transposeMatrix(x)
}


# rotateMatrix180
#
# rotates a 3x3 matrix 180 degrees
# - first uses rotateMatrix90 function
# - then uses/multplied with a transformation matrix
# - and transpose function 
# 
# @param mat matrix
# @return rotated matrix
rotateMatrix180=function(mat)
{
  ninety=rotateMatrix90(mat)
  tMatrix=matrix(c(0,0,1,
                   0,1,0,
                   1,0,0), nrow=3, byrow=T)
  x=tMatrix%*%ninety
  transposeMatrix(x)
}

# rotateMatrix270
#
# rotates a 3x3 matrix 270 degrees
# - first uses the rotateMatrix180 function
# - then uses/multiplied with transformation matrix
# - lastly transpose function
#
# @param mat matrix
# @return rotated matrix
rotateMatrix270=function(mat)
{
  oneEighty=rotateMatrix180(mat)
  tMatrix=matrix(c(0,0,1,
                   0,1,0,
                   1,0,0), nrow=3, byrow=T)
  x=tMatrix%*%oneEighty
  transposeMatrix(x)
}
