B <- 10000

set.seed(1)
#Monto Carlo Stick to one door

stick <- replicate(B,{
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat")) # puts prizes in random order
  prize_door <- doors[prize=="car"] # note which door has prize
  my_pick <- sample(doors,1)# note which door is chosen
  show <-sample(doors[!doors %in% c(my_pick,prize_door)],1)  # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)

#Monte Carlo simulation of switch strategy
stick <- replicate(B,{
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat")) # puts prizes in random order
  prize_door <- doors[prize=="car"] # note which door has prize
  my_pick <- sample(doors,1)# note which door is chosen
  show <-sample(doors[!doors %in% c(my_pick,prize_door)],1)  # open door with no prize that isn't chosen
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door    # test whether the original door has the prize
})
mean(stick)

?rep()
