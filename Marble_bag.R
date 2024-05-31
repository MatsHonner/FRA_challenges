# Define the Bag class
Bag <- setRefClass(
  "Bag",
  fields = list(
    marbles = "character"
  ),
  methods = list(
    initialize = function() {
      marbles <<- c(rep("white", 25), rep("black", 25))
    },

    pick_two = function() {
      if (length(marbles) < 2) {
        stop("Not enough marbles to pick two.")
      }
      picked <- sample(marbles, 2)
      index_to_remove <- which(marbles == picked[1])[1]
      marbles <<- marbles[-index_to_remove]
      index_to_remove <- which(marbles == picked[2])[1]
      marbles <<- marbles[-index_to_remove]
      return(picked)
    },
    
    put_back = function(marble) {
      if (marble != "white" && marble != "black") {
        stop("Invalid marble color. Only 'white' or 'black' are allowed.")
      }
      marbles <<- c(marbles, marble)
    },
    
    peek = function() {
      return(table(marbles))
    }
  )
)



# # Create an instance of the Bag class
# bag <- Bag$new()
# 
# # Example usage:
# # 1. Pick two marbles
# picked_marbles <- bag$pick_two()
# print(paste("Picked marbles:", paste(picked_marbles, collapse = ", ")))
# 
# # 2. Put back one marble (e.g., putting back a white marble)
# bag$put_back("white")
# 
# # 3. Peek into the bag to see the current counts of white and black marbles
# current_counts <- bag$peek()
# print(current_counts)
