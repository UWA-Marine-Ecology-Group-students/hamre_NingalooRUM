# Cleaning Functions 

## hhmm2dec
## hhmm2dec is a function that turns hh:mm formatted time into decimal time. 
## Arguments 
## x = a time variable formatted hh:mm

hhmm2dec <- function(x) {
  xlist <- strsplit(x,split=":")
  h <- as.numeric(sapply(xlist,"[",1))
  m <- as.numeric(sapply(xlist,"[",2))
  s <- as.numeric(sapply(xlist,"[",3))
  xdec <- h+(m/60)
  return(xdec)
}

# even_weights function
# calculates sampling weights for a column in a data frame where there is meant to be an even distribution across the column.
# eg. morning/afternoon, sampling sites
# ARGUMENTS
# df: data frame
# psuid: primary sampling unit unique identifier eg. if a psu is 1 day, the psuid could be a date col,
# if its is a morning of afternoon shift per day it could be the "date AMPM"
# w_col: the column to be weighted
# Call example: dat <- even_weights(df = dat, psuid = Shifts, w_col = "Time")

even_weights <- function(df, psuid, w_col) {
  
  d <- distinct(df, {{psuid}}, .keep_all = TRUE)
  
  freq_tbl <- as.data.frame(table(d[[w_col]])) %>%
    mutate(w = Freq/sum(Freq)) %>%
    rename({{w_col}} := Var1)
  
  # return(freq_tbl)
  # stop()
  
  if (sum(freq_tbl$w) != 1) {
    
    stop("Sum of weights does not equal 1")
    
  } else {
    
    df %<>% left_join(freq_tbl[, c({{w_col}}, "w")])
    colnames(df)[ncol(df)] <- paste0("w_", w_col)
    
  }
  
  return(df)
}