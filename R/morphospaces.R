# utils
# m is a matrix (eg of rotation). Must have colnames
# s is a named vector. Names must found among colnames(m)
# Prepare a diagonal matrix with provided elements of s
# In the end multiplies each column of m with matching element of s
.mprod <- function(m, s){
  .check(all(names(s) %in% colnames(m)),
         '.mprod: all "s" names must be present in "m"')
  # 0 numeric vector
  m2 <- vector("numeric", ncol(m))
  # named after m
  names(m2) <- colnames(m)
  # fill cols based on s names
  m2[names(s)] <- s
  # matrix multiplication
  res <- (m %*% diag(m2))
  # copy names
  colnames(res) <- colnames(m)
  res
}

