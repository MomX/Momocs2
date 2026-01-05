# Clear the environment

Remove all objects from the current environment.

## Usage

``` r
rinse(env = .GlobalEnv)
```

## Arguments

- env:

  Environment to clear. Default is the global environment.

## Value

Invisibly returns NULL.

## Details

Removes all user-created objects from the specified environment. Useful
for cleaning up workspace between analyses or starting fresh.

Use with caution: removed objects cannot be recovered.

## Note

I know J Bryan and my old friend T Poisot said it was anti-social in a
famous tweet but I still find it useful.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create some objects
x <- 1
y <- 2
z <- data.frame(a = 1:10)

# Check what's in environment
ls()

# Clean it all up
rinse()

# Environment is now empty
ls()
} # }
```
