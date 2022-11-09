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