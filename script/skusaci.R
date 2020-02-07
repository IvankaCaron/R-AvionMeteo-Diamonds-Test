df <- tibble(
  a = rnorm(10), 
  b = rnorm(10), 
  c = rnorm(10), 
  d = rnorm(10)
)

median(df$a)
median(df$b)
median(df$c)
median(df$d)

output <- vector("double", ncol(df))    # output (arguments: data type + # of elements) 
for (i in seq_along(df)){               # sequence
  output[[i]] <- median(df[[i]])        # body 
}
output
df$a

library(nycflights13)
output <- vector("character", length(flights))
for (i in seq_along(flights)){
  output[[i]] <- typeof(flights[[i]])
}
output 


models <- mtcars %>%
  split(.$cyl) %>%                           # output - a list of three dataframes by cyliner type  
  map(function(df) lm(mpg ~ wt, data = df))  # not additional arguments, instead function definition 
models
