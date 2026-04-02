# Much of the content presented today is based on the debugging section of the
# Advanced R book (https://adv-r.hadley.nz/debugging.html)
library(tidyverse)
library(cicalc)

# Reading error messages 
mtcars |> 
  filter(Cyl == 4)

new_obj <- "cyl"






# Reading error with many steps 
mtcars |> 
  filter(cyl == 4) |> 
  mutate(vroom_index = hp*drat/wt) |> 
  summarise(mean(vroom_index))


# Debugging my own code
my_function <- function(x){
  while(x < 10){
    x <- x + 1
  }
  x
}

# Debugging others code 
test <- rbinom(50, 1, 0.6)
ci_prop_wald(test)
debugonce(ci_prop_wald)
debug(mutate)
undebug(mutate)
