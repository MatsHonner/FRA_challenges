# Initialize seats and persons
seats <- rep(NA, 112)  # Vector representing seats, initially all unoccupied (NA)
names(seats) <- 1:112  # Name seats with their seat numbers

# Function to place a person on a random, unoccupied seat
place_random <- function(person) {
  if (person < 1 || person > 112) {
    return("Person number must be between 1 and 112")
  }
  available_seats <- which(is.na(seats))  # Find all unoccupied seats
  if (length(available_seats) == 0) {
    return("No available seats")
  }
  chosen_seat <- sample(available_seats, 1)  # Randomly choose one of the available seats
  seats[chosen_seat] <- person
  return(paste("Person", person, "placed on seat", chosen_seat))
}

# Function to place a person on a specific seat
place_on_seat <- function(person, seat_number) {
  if (person < 1 || person > 112) {
    return("Person number must be between 1 and 112")
  }
  if (seat_number < 1 || seat_number > 112) {
    return("Seat number must be between 1 and 112")
  }
  if (!is.na(seats[seat_number])) {
    return(paste("Seat", seat_number, "is already taken by person", seats[seat_number]))
  }
  seats[seat_number] <- person
  return(paste("Person", person, "placed on seat", seat_number))
}

# Function to read out the seat for a specific person
find_person <- function(person) {
  if (person < 1 || person > 112) {
    return("Person number must be between 1 and 112")
  }
  seat_number <- which(seats == person)
  if (length(seat_number) == 0) {
    return(paste("Person", person, "is not seated"))
  }
  return(paste("Person", person, "is seated on seat", seat_number))
}

# Example usage
print(place_random(5))         # Place person 5 on a random seat
print(place_on_seat(6, 10))    # Place person 6 on seat 10
print(find_person(5))          # Find seat for person 5
print(find_person(6))          # Find seat for person 6
print(place_on_seat(7, 10))    # Attempt to place person 7 on seat 10, which is taken
