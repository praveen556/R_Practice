#Monte Carlo simulation of stick strategy
B <- 10000

sticks <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("Goat","Car","Goat")) # puts prizes in random order
  prize_door <- doors[prize == "Car"] # note which door has prize
  my_pick <- sample(doors,1) # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick,prize_door)],1) # open door with no prize that isn't chosen
  stick <- my_pick # stick with original door
  stick == prize_door# test whether the original door has the prize
})
mean(sticks)

#Monte Carlo simulation of switching the door

B <- 10000
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)

sticks <- replicate(B, {
  winners <- sample(runners, 3)
  (winners[1] %in% "Jamaica" & winners[2] %in% "Jamaica" & winners[3] %in% "Jamaica")
})
mean(sticks)