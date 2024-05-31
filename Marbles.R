library(tidyverse)

#set.seed(1235)

marbles <- Bag$new()

number_of_marbles <- marbles$peek() %>% sum()

while (number_of_marbles > 1) {
  two_marbles <- marbles$pick_two()
  
  if (two_marbles[1] == two_marbles[2]) {
    marbles$put_back("white")
  } else {
    marbles$put_back("black")
  }
  
  number_of_marbles <- marbles$peek() %>% sum()
  
  print(number_of_marbles)
  print(marbles$peek())
  print("")
}

last_marble <- marbles$peek()
if (length(last_marble["white"]) > 0) {
  print("Last marble is white")
} else {
  print("Last marble is black")
}


