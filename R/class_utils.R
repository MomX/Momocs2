# class() helpers -----------------------------------------

# help add class tag in front of others, if not already
.append_class <- function(x, newclass){
  classes <- class(x)
  if (!(newclass %in% classes) && newclass != classes[1])
    class(x) <- c(newclass, classes)
  x
}

# # help add class tag in front of others, if not already
.replace_class <- function(x, oldclass, newclass){
  class(x) <- class(x) %>% stringr::str_replace(oldclass, newclass)
  x
}

# tests if an object is (somewhere) of class
# kind of more explicit inherits(x, "classname")
.is_class <- function(x, class){
  class %in% class(x)
}

# tests if an object is (firstly) of class
.is_class1 <- function(x, class){
  class == class(x)[1]
}
