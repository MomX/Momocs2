# Momocs2 2.0.0
* Everything is tidyverse ready and in tibbles.
* Classes have been refreshed and are described in the vignette 'architectures and classes'
* Coo with `$coo`, `$fac`, etc. and Coe with `$coe`, `$fac`, etc. are deprecated for single tibbles. Now Momocs main objects are tibbles with attributes (but you will probably not use them). Single coo (formerly referred as shapes) (`coo`) and coefficients (`coe`) are also tibbles.
* All graphics are `ggplot2` ready. Including morphospaces.
* Only `PCA` and `LDA` are maintained in Momocs. Other statistical and graphical methods will be part of `Momstats`.
* Importing functions will be part of `Momit`, to be released in spring 2020.
* tfourier and rfourier are deprecated on `Out`. I do not think they are worth the effort. The core functions are maintained.
* Testing coverage has been massively enlarged and improved
* fixed a bug in `coo_centsize` (!!). Thanks to Almond-S on github.

* overall style is improved, more consistent and follows most tidyverse recommendations. First argument is always `x`; arguments no longer use `-` but `_`, and the internal code is styled with `styler` package

### Breaking changes
* `[[` and `$` methods on shape collection are depreacted. `bot[[1]]` becomes `bot %>% pick(1)`. `$` to access `fac$` is now useless since everything is now a tibble
* `coo_nb` now counts the number of `coo_list` columns (and not the number of points within a shape which is a getter now, called `get_nb`)
# Momocs2 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
