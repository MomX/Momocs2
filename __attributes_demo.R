library(tidyverse)
library(Momocs)

x <- iris %>% as_tibble()

select(x, -Species) %>% prcomp() -> p


attributes(x)$pca <- p

attributes(x)$pca$rotation #:-)

# attributes should stick to tbl object
attributes(x)$rotation <- p$rotation
attributes(x)
attributes(x[1:5, ]) # :'( -> sticky
x %>% slice(1:5) %>% filter(Species, Petal.Length) %>% attributes() # :-)

# not to co* columns
x <- x %>% mutate(coe=replicate(150, runif(50), simplify=FALSE))
attributes(x$coe)$rotation <- p$rotation
attributes(x$coe)
attributes(x$coe[1:5]) # :'( -> sticky


df <- tibble(coo=bot$coo, plop=bot$fac$type)

x <- bot2
do_plop <- function(x, f){
  x %>% mutate(coo=map(coo, f)) %>% .add_history("coo_center")
}

do_plip <- function(x, f){
  x %>% mutate(coo=map(coo, f)) %>% .add_history("coo_scale")
}

x %>% do_plop(Momocs::coo_center) %>% do_plip(Momocs::coo_center) %>% attributes()



bot2 %>% do_plop(coo_center) %>% do_plop2(coo_center) %>%  attributes()

df %$% coo %>% Out() %>% stack()
df %>%
  do_plop(coo_center) %$% coo %>% Out() %>% stack()

