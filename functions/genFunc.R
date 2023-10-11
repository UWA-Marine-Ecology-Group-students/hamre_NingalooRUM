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

even_nipw <- function(df, psuid, w_col) {
  
  d <- distinct(df, {{psuid}}, .keep_all = TRUE)
  
  # differnt method not sure if its right
  freq_tbl <- as.data.frame(table(d[[w_col]])) %>%
    #   mutate(ipw = 1/Freq) %>% # inverse probability weight
    #   mutate(nipw = ipw/sum(ipw)) %>%  # normalized inverse probability weight
    mutate(target = 1/nrow(.)) %>% # target probability
    mutate(real = Freq/sum(Freq)) %>% # real probability
    mutate(ipw = target/real) %>% # inverse probability weight
    mutate(nipw = ipw/sum(ipw)) %>% # inverse probability weight
    rename({{w_col}} := Var1)
  
  sum_nipw <- sum(freq_tbl$nipw)
  print(paste("Sum of nipw equals:", sum_nipw))
  
  colnames(freq_tbl)[ncol(freq_tbl)] <- paste0(w_col, "_nipw")
  
  return(freq_tbl)
  
}
