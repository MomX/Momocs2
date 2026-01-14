# Check and clean coordinates

Remove problematic rows (NA values, consecutive duplicates, infinite
values) and warn about potential issues.

## Usage

``` r
coo_check(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- .ldk_col:

  Character. Name of landmark column. If `NULL`, uses `colname_ldk`.

- remove_na:

  Logical. Remove rows with NA values. Default TRUE.

- remove_duplicates:

  Logical. Remove consecutive duplicate points. Default TRUE.

- remove_infinite:

  Logical. Remove rows with Inf/-Inf values. Default TRUE.

- min_points:

  Integer. Warn if fewer than this many points remain. Default 3.

- warn_collinear:

  Logical. Warn if all points are collinear. Default TRUE.

## Value

- If `x` is a single matrix: returns the cleaned matrix

- If `x` is a list: returns a list of cleaned matrices

- If `x` is a tibble: returns the tibble with cleaned coo column(s)

## Details

Performs the following checks and cleaning operations:

1.  **Remove NA rows**: Rows containing NA in either coordinate

2.  **Remove consecutive duplicates**: Identical adjacent points

3.  **Remove infinite values**: Rows containing Inf or -Inf

4.  **Warn if too few points**: Less than `min_points` remain

5.  **Warn if collinear**: All points lie on a line

Messages indicate which shapes were affected and what was removed.
Landmarks are synced - if a point is removed, its landmark is also
removed.

## See also

[`coo_sample()`](https://momx.github.io/Momocs2/reference/coo_sample.md)
for resampling

## Examples

``` r
# Clean a single shape
coo_check(shapes$cat)
#> <xy [120 x 2]>
#>       [,1] [,2]
#>  [1,] 200   62 
#>  [2,] 196   56 
#>  [3,] 200   56 
#>  [4,] 206   54 
#>  [5,] 209   48 
#>  [6,] ...  ... 
#>  [7,] 219   45 
#>  [8,] 213   47 
#>  [9,] 208   53 
#> [10,] 204   59 
#> [11,] 202   65 

# Clean all shapes in list
coo_check(bot$coo)
#> ℹ Removed: 28 consecutive duplicates
#> ℹ Removed: 3 consecutive duplicates
#> ℹ Removed: 17 consecutive duplicates
#> ℹ Removed: 51 consecutive duplicates
#> ℹ Removed: 35 consecutive duplicates
#> ℹ Removed: 19 consecutive duplicates
#> ℹ Removed: 22 consecutive duplicates
#> ℹ Removed: 44 consecutive duplicates
#> ℹ Removed: 45 consecutive duplicates
#> ℹ Removed: 45 consecutive duplicates
#> ℹ Removed: 5 consecutive duplicates
#> ℹ Removed: 32 consecutive duplicates
#> $brahma
#> <xy [138 x 2]>
#>       [,1] [,2]
#>  [1,]  37  561 
#>  [2,]  40  540 
#>  [3,]  40  529 
#>  [4,]  43  508 
#>  [5,]  46  487 
#>  [6,] ...  ... 
#>  [7,]  34  656 
#>  [8,]  33  645 
#>  [9,]  33  624 
#> [10,]  34  603 
#> [11,]  35  593 
#> 
#> $caney
#> <xy [168 x 2]>
#>       [,1] [,2]
#>  [1,]  53  535 
#>  [2,]  53  525 
#>  [3,]  54  505 
#>  [4,]  53  495 
#>  [5,]  54  485 
#>  [6,] ...  ... 
#>  [7,]  76  616 
#>  [8,]  67  596 
#>  [9,]  62  585 
#> [10,]  59  575 
#> [11,]  55  555 
#> 
#> $chimay
#> <xy [189 x 2]>
#>       [,1] [,2]
#>  [1,]  49  333 
#>  [2,]  49  325 
#>  [3,]  49  318 
#>  [4,]  50  310 
#>  [5,]  50  302 
#>  [6,] ...  ... 
#>  [7,]  59  379 
#>  [8,]  54  371 
#>  [9,]  51  364 
#> [10,]  49  356 
#> [11,]  48  348 
#> 
#> $corona
#> <xy [129 x 2]>
#>       [,1] [,2]
#>  [1,]  91  426 
#>  [2,]  91  416 
#>  [3,]  90  395 
#>  [4,]  91  385 
#>  [5,]  91  374 
#>  [6,] ...  ... 
#>  [7,] 118  506 
#>  [8,] 110  486 
#>  [9,] 105  477 
#> [10,] 101  466 
#> [11,]  94  447 
#> 
#> $deusventrue
#> <xy [152 x 2]>
#>       [,1] [,2]
#>  [1,]  74  481 
#>  [2,]  70  472 
#>  [3,]  62  454 
#>  [4,]  59  445 
#>  [5,]  52  425 
#>  [6,] ...  ... 
#>  [7,] 100  555 
#>  [8,]  97  545 
#>  [9,]  90  527 
#> [10,]  87  518 
#> [11,]  82  500 
#> 
#> $duvel
#> <xy [133 x 2]>
#>       [,1] [,2]
#>  [1,]  61  315 
#>  [2,]  61  304 
#>  [3,]  61  293 
#>  [4,]  61  282 
#>  [5,]  59  272 
#>  [6,] ...  ... 
#>  [7,]  81  379 
#>  [8,]  74  369 
#>  [9,]  69  358 
#> [10,]  65  348 
#> [11,]  63  337 
#> 
#> $franziskaner
#> <xy [124 x 2]>
#>       [,1] [,2]
#>  [1,]  54  439 
#>  [2,]  54  418 
#>  [3,]  54  408 
#>  [4,]  54  389 
#>  [5,]  54  379 
#>  [6,] ...  ... 
#>  [7,]  60  532 
#>  [8,]  59  511 
#>  [9,]  57  501 
#> [10,]  54  480 
#> [11,]  54  470 
#> 
#> $grimbergen
#> <xy [126 x 2]>
#>       [,1] [,2]
#>  [1,]  42  404 
#>  [2,]  40  394 
#>  [3,]  40  373 
#>  [4,]  40  362 
#>  [5,]  40  352 
#>  [6,] ...  ... 
#>  [7,]  74  487 
#>  [8,]  60  467 
#>  [9,]  54  456 
#> [10,]  50  446 
#> [11,]  44  425 
#> 
#> $guiness
#> <xy [183 x 2]>
#>       [,1] [,2]
#>  [1,]  69  385 
#>  [2,]  69  377 
#>  [3,]  69  369 
#>  [4,]  69  360 
#>  [5,]  69  344 
#>  [6,] ...  ... 
#>  [7,]  73  442 
#>  [8,]  71  433 
#>  [9,]  67  418 
#> [10,]  68  410 
#> [11,]  69  401 
#> 
#> $hoegardeen
#> <xy [190 x 2]>
#>       [,1] [,2]
#>  [1,]  42  544 
#>  [2,]  40  531 
#>  [3,]  40  519 
#>  [4,]  40  506 
#>  [5,]  40  493 
#>  [6,] ...  ... 
#>  [7,]  58  619 
#>  [8,]  54  606 
#>  [9,]  51  594 
#> [10,]  47  581 
#> [11,]  46  568 
#> 
#> $jupiler
#> <xy [156 x 2]>
#>       [,1] [,2]
#>  [1,]  55  515 
#>  [2,]  54  501 
#>  [3,]  54  488 
#>  [4,]  54  474 
#>  [5,]  54  460 
#>  [6,] ...  ... 
#>  [7,]  74  597 
#>  [8,]  69  583 
#>  [9,]  66  570 
#> [10,]  62  556 
#> [11,]  59  542 
#> 
#> $kingfisher
#> <xy [165 x 2]>
#>       [,1] [,2]
#>  [1,]  71  384 
#>  [2,]  71  374 
#>  [3,]  71  365 
#>  [4,]  71  355 
#>  [5,]  71  345 
#>  [6,] ...  ... 
#>  [7,]  81  442 
#>  [8,]  78  433 
#>  [9,]  76  423 
#> [10,]  73  413 
#> [11,]  73  403 
#> 
#> $latrappe
#> <xy [136 x 2]>
#>       [,1] [,2]
#>  [1,]  26  389 
#>  [2,]  25  377 
#>  [3,]  25  364 
#>  [4,]  25  352 
#>  [5,]  25  339 
#>  [6,] ...  ... 
#>  [7,]  28  464 
#>  [8,]  25  451 
#>  [9,]  26  439 
#> [10,]  26  426 
#> [11,]  26  414 
#> 
#> $lindemanskriek
#> <xy [176 x 2]>
#>       [,1] [,2]
#>  [1,]  60  427 
#>  [2,]  60  419 
#>  [3,]  55  404 
#>  [4,]  54  395 
#>  [5,]  54  387 
#>  [6,] ...  ... 
#>  [7,]  77  486 
#>  [8,]  75  478 
#>  [9,]  72  469 
#> [10,]  70  461 
#> [11,]  65  444 
#> 
#> $nicechouffe
#> <xy [146 x 2]>
#>       [,1] [,2]
#>  [1,]  82  361 
#>  [2,]  81  353 
#>  [3,]  77  336 
#>  [4,]  77  327 
#>  [5,]  76  319 
#>  [6,] ...  ... 
#>  [7,]  96  418 
#>  [8,]  93  410 
#>  [9,]  91  401 
#> [10,]  89  394 
#> [11,]  85  378 
#> 
#> $pecheresse
#> <xy [129 x 2]>
#>       [,1] [,2]
#>  [1,]  63  495 
#>  [2,]  61  476 
#>  [3,]  61  467 
#>  [4,]  60  447 
#>  [5,]  58  428 
#>  [6,] ...  ... 
#>  [7,]  86  580 
#>  [8,]  83  570 
#>  [9,]  78  552 
#> [10,]  73  533 
#> [11,]  71  524 
#> 
#> $sierranevada
#> <xy [125 x 2]>
#>       [,1] [,2]
#>  [1,]  61  345 
#>  [2,]  61  333 
#>  [3,]  61  320 
#>  [4,]  61  308 
#>  [5,]  61  296 
#>  [6,] ...  ... 
#>  [7,]  69  418 
#>  [8,]  63  407 
#>  [9,]  61  394 
#> [10,]  59  382 
#> [11,]  61  370 
#> 
#> $tanglefoot
#> <xy [174 x 2]>
#>       [,1] [,2]
#>  [1,]  48  367 
#>  [2,]  48  359 
#>  [3,]  48  351 
#>  [4,]  48  335 
#>  [5,]  49  327 
#>  [6,] ...  ... 
#>  [7,]  52  423 
#>  [8,]  49  415 
#>  [9,]  48  407 
#> [10,]  47  391 
#> [11,]  47  383 
#> 
#> $tauro
#> <xy [174 x 2]>
#>       [,1] [,2]
#>  [1,]  56  515 
#>  [2,]  54  503 
#>  [3,]  54  491 
#>  [4,]  54  478 
#>  [5,]  54  466 
#>  [6,] ...  ... 
#>  [7,]  71  587 
#>  [8,]  68  575 
#>  [9,]  63  563 
#> [10,]  61  551 
#> [11,]  59  538 
#> 
#> $westmalle
#> <xy [141 x 2]>
#>       [,1] [,2]
#>  [1,]  70  394 
#>  [2,]  70  386 
#>  [3,]  67  369 
#>  [4,]  67  361 
#>  [5,]  66  344 
#>  [6,] ...  ... 
#>  [7,]  86  461 
#>  [8,]  83  452 
#>  [9,]  80  436 
#> [10,]  76  427 
#> [11,]  74  411 
#> 
#> $amrut
#> <xy [191 x 2]>
#>       [,1] [,2]
#>  [1,]  57  441 
#>  [2,]  57  431 
#>  [3,]  57  421 
#>  [4,]  57  411 
#>  [5,]  57  401 
#>  [6,] ...  ... 
#>  [7,]  55  501 
#>  [8,]  57  491 
#>  [9,]  57  481 
#> [10,]  57  471 
#> [11,]  57  461 
#> 
#> $ballantines
#> <xy [146 x 2]>
#>       [,1] [,2]
#>  [1,]  38  357 
#>  [2,]  38  341 
#>  [3,]  38  333 
#>  [4,]  38  318 
#>  [5,]  38  310 
#>  [6,] ...  ... 
#>  [7,]  38  428 
#>  [8,]  38  412 
#>  [9,]  38  404 
#> [10,]  38  388 
#> [11,]  38  381 
#> 
#> $bushmills
#> <xy [130 x 2]>
#>       [,1] [,2]
#>  [1,]  72  456 
#>  [2,]  72  441 
#>  [3,]  72  425 
#>  [4,]  72  410 
#>  [5,]  72  394 
#>  [6,] ...  ... 
#>  [7,]  69  548 
#>  [8,]  70  533 
#>  [9,]  70  518 
#> [10,]  70  502 
#> [11,]  70  487 
#> 
#> $chivas
#> <xy [164 x 2]>
#>       [,1] [,2]
#>  [1,]  33  437 
#>  [2,]  31  429 
#>  [3,]  29  412 
#>  [4,]  29  404 
#>  [5,]  29  388 
#>  [6,] ...  ... 
#>  [7,]  81  502 
#>  [8,]  73  495 
#>  [9,]  57  478 
#> [10,]  51  470 
#> [11,]  40  453 
#> 
#> $dalmore
#> <xy [136 x 2]>
#>       [,1] [,2]
#>  [1,]  52  371 
#>  [2,]  47  359 
#>  [3,]  44  346 
#>  [4,]  42  334 
#>  [5,]  42  322 
#>  [6,] ...  ... 
#>  [7,] 113  442 
#>  [8,] 102  430 
#>  [9,]  89  419 
#> [10,]  77  408 
#> [11,]  67  396 
#> 
#> $famousgrouse
#> <xy [147 x 2]>
#>       [,1] [,2]
#>  [1,]  99  313 
#>  [2,]  99  303 
#>  [3,]  99  294 
#>  [4,]  99  284 
#>  [5,]  99  275 
#>  [6,] ...  ... 
#>  [7,]  96  370 
#>  [8,]  97  361 
#>  [9,]  97  351 
#> [10,]  97  342 
#> [11,]  97  332 
#> 
#> $glendronach
#> <xy [153 x 2]>
#>       [,1] [,2]
#>  [1,]  73  435 
#>  [2,]  73  423 
#>  [3,]  74  411 
#>  [4,]  74  399 
#>  [5,]  74  387 
#>  [6,] ...  ... 
#>  [7,]  74  507 
#>  [8,]  72  495 
#>  [9,]  72  483 
#> [10,]  73  471 
#> [11,]  73  459 
#> 
#> $glenmorangie
#> <xy [134 x 2]>
#>       [,1] [,2]
#>  [1,]  53  518 
#>  [2,]  54  501 
#>  [3,]  54  485 
#>  [4,]  55  468 
#>  [5,]  57  451 
#>  [6,] ...  ... 
#>  [7,]  68  616 
#>  [8,]  62  599 
#>  [9,]  58  584 
#> [10,]  54  567 
#> [11,]  53  550 
#> 
#> $highlandpark
#> <xy [124 x 2]>
#>       [,1] [,2]
#>  [1,]  42  371 
#>  [2,]  42  357 
#>  [3,]  42  343 
#>  [4,]  42  329 
#>  [5,]  42  315 
#>  [6,] ...  ... 
#>  [7,]  40  455 
#>  [8,]  41  441 
#>  [9,]  42  427 
#> [10,]  42  413 
#> [11,]  42  399 
#> 
#> $jackdaniels
#> <xy [145 x 2]>
#>       [,1] [,2]
#>  [1,]  63  414 
#>  [2,]  63  401 
#>  [3,]  63  388 
#>  [4,]  63  375 
#>  [5,]  63  362 
#>  [6,] ...  ... 
#>  [7,]  70  493 
#>  [8,]  68  479 
#>  [9,]  65  466 
#> [10,]  64  453 
#> [11,]  65  440 
#> 
#> $jb
#> <xy [174 x 2]>
#>       [,1] [,2]
#>  [1,]  43  531 
#>  [2,]  43  520 
#>  [3,]  42  510 
#>  [4,]  42  488 
#>  [5,]  43  477 
#>  [6,] ...  ... 
#>  [7,]  42  606 
#>  [8,]  40  595 
#>  [9,]  42  585 
#> [10,]  43  563 
#> [11,]  43  552 
#> 
#> $johnniewalker
#> <xy [168 x 2]>
#>       [,1] [,2]
#>  [1,] 133  175 
#>  [2,] 133  171 
#>  [3,] 133  167 
#>  [4,] 133  163 
#>  [5,] 134  155 
#>  [6,] ...  ... 
#>  [7,] 133  203 
#>  [8,] 133  199 
#>  [9,] 133  191 
#> [10,] 133  187 
#> [11,] 133  183 
#> 
#> $magallan
#> <xy [141 x 2]>
#>       [,1] [,2]
#>  [1,]  78  397 
#>  [2,]  78  382 
#>  [3,]  80  375 
#>  [4,]  80  361 
#>  [5,]  81  346 
#>  [6,] ...  ... 
#>  [7,]  75  462 
#>  [8,]  75  455 
#>  [9,]  75  441 
#> [10,]  77  426 
#> [11,]  77  419 
#> 
#> $makersmark
#> <xy [145 x 2]>
#>       [,1] [,2]
#>  [1,]  31  432 
#>  [2,]  23  420 
#>  [3,]  16  408 
#>  [4,]  13  394 
#>  [5,]  10  381 
#>  [6,] ...  ... 
#>  [7,]  92  491 
#>  [8,]  79  487 
#>  [9,]  74  473 
#> [10,]  67  464 
#> [11,]  53  454 
#> 
#> $oban
#> <xy [179 x 2]>
#>       [,1] [,2]
#>  [1,]  74  457 
#>  [2,]  74  448 
#>  [3,]  74  431 
#>  [4,]  74  423 
#>  [5,]  74  414 
#>  [6,] ...  ... 
#>  [7,]  72  517 
#>  [8,]  73  508 
#>  [9,]  73  500 
#> [10,]  73  491 
#> [11,]  73  474 
#> 
#> $oldpotrero
#> <xy [131 x 2]>
#>       [,1] [,2]
#>  [1,]  83  307 
#>  [2,]  77  299 
#>  [3,]  71  291 
#>  [4,]  63  274 
#>  [5,]  60  265 
#>  [6,] ...  ... 
#>  [7,] 119  366 
#>  [8,] 117  358 
#>  [9,] 112  349 
#> [10,] 101  333 
#> [11,]  95  324 
#> 
#> $redbreast
#> <xy [177 x 2]>
#>       [,1] [,2]
#>  [1,] 105  233 
#>  [2,] 103  228 
#>  [3,] 101  224 
#>  [4,]  98  214 
#>  [5,]  98  210 
#>  [6,] ...  ... 
#>  [7,] 137  261 
#>  [8,] 132  257 
#>  [9,] 127  253 
#> [10,] 118  246 
#> [11,] 113  242 
#> 
#> $tamdhu
#> <xy [176 x 2]>
#>       [,1] [,2]
#>  [1,]  49  552 
#>  [2,]  49  540 
#>  [3,]  49  527 
#>  [4,]  49  515 
#>  [5,]  49  503 
#>  [6,] ...  ... 
#>  [7,]  46  625 
#>  [8,]  44  613 
#>  [9,]  45  601 
#> [10,]  46  589 
#> [11,]  47  577 
#> 
#> $wildturkey
#> <xy [185 x 2]>
#>       [,1] [,2]
#>  [1,]  18  569 
#>  [2,]  18  555 
#>  [3,]  18  541 
#>  [4,]  18  527 
#>  [5,]  18  513 
#>  [6,] ...  ... 
#>  [7,]  20  651 
#>  [8,]  18  638 
#>  [9,]  15  625 
#> [10,]  15  611 
#> [11,]  15  597 
#> 
#> $yoichi
#> <xy [123 x 2]>
#>       [,1] [,2]
#>  [1,]  69  384 
#>  [2,]  69  369 
#>  [3,]  69  355 
#>  [4,]  69  340 
#>  [5,]  69  325 
#>  [6,] ...  ... 
#>  [7,]  68  457 
#>  [8,]  68  450 
#>  [9,]  68  435 
#> [10,]  68  421 
#> [11,]  69  406 
#> 
#> attr(,"class")
#> [1] "out"  "coo"  "list"

# Clean all shapes in tibble
coo_check(bot)
#> ℹ Removed: 28 consecutive duplicates
#> ℹ Removed: 3 consecutive duplicates
#> ℹ Removed: 17 consecutive duplicates
#> ℹ Removed: 51 consecutive duplicates
#> ℹ Removed: 35 consecutive duplicates
#> ℹ Removed: 19 consecutive duplicates
#> ℹ Removed: 22 consecutive duplicates
#> ℹ Removed: 44 consecutive duplicates
#> ℹ Removed: 45 consecutive duplicates
#> ℹ Removed: 45 consecutive duplicates
#> ℹ Removed: 5 consecutive duplicates
#> ℹ Removed: 32 consecutive duplicates
#> # A tibble: 40 × 5
#>    id           coo       type   fake  price
#>    <chr>        <out>     <fct>  <fct> <dbl>
#>  1 brahma       (138 x 2) whisky a       3  
#>  2 caney        (168 x 2) whisky a       1.2
#>  3 chimay       (189 x 2) whisky a       3.8
#>  4 corona       (129 x 2) whisky a       2.6
#>  5 deusventrue  (152 x 2) whisky a       1.1
#>  6 duvel        (133 x 2) whisky a       3.1
#>  7 franziskaner (124 x 2) whisky a       2.6
#>  8 grimbergen   (126 x 2) whisky a       2.9
#>  9 guiness      (183 x 2) whisky a       1.2
#> 10 hoegardeen   (190 x 2) whisky a       3.6
#> # ℹ 30 more rows

# Disable specific checks
coo_check(bot, remove_duplicates = FALSE, warn_collinear = FALSE)
#> # A tibble: 40 × 5
#>    id           coo       type   fake  price
#>    <chr>        <out>     <fct>  <fct> <dbl>
#>  1 brahma       (138 x 2) whisky a       3  
#>  2 caney        (168 x 2) whisky a       1.2
#>  3 chimay       (189 x 2) whisky a       3.8
#>  4 corona       (129 x 2) whisky a       2.6
#>  5 deusventrue  (152 x 2) whisky a       1.1
#>  6 duvel        (161 x 2) whisky a       3.1
#>  7 franziskaner (124 x 2) whisky a       2.6
#>  8 grimbergen   (126 x 2) whisky a       2.9
#>  9 guiness      (183 x 2) whisky a       1.2
#> 10 hoegardeen   (193 x 2) whisky a       3.6
#> # ℹ 30 more rows
```
