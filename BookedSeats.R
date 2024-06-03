library(tidyverse)

run <- function() {
  place_random(1)
  
  for (i in 2:111) {
    result <- place_on_seat(i, i)
    
    if ("is already taken" %in% result) {
      place_random(i)
    }
  }
  
  result <- place_on_seat(112, 112)
  
  if ("is already taken" %in% result) {
    #print("already taken...")
    return("already taken...")
  } else {
    #print("#112 on seat 112!!!")
    return("#112 on seat 112!!!")
  }
}

for (i in 1:100) {
  print(run())
}

