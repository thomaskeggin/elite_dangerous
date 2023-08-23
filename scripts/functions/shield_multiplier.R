# calculates the shield multiplier based on hull mass
# https://cmdrs-toolbox.com/shield-tester

shielding <- function(data_shield,hull_mass,target_shield,base_shield){

  target_shield <- filter(data_shield, id == target_shield)
  
  if(dim(target_shield)[1] == 0){
    print("shield does not exist")
    stop()
  }
  
  max_mass  <- target_shield$max_mass
  opt_mass  <- target_shield$opt_mass
  min_mass  <- target_shield$min_mass
  
  min_mult <- 1.5
  opt_mult <- 2
  max_mult <- 2.5
  
  hull_mass <- hull_mass #+ target_shield$Mass
  
  # norm. hull mass
  normalised_hull_mass <- min(c(1,
                                (max_mass-hull_mass)/(max_mass-min_mass)))
  
  
  # exponent
  exp_top <- log10((opt_mult-min_mult)/(max_mult-min_mult))
  exp_bot <- log10(min(1,
                       (max_mass-opt_mass)/(max_mass-min_mass)))
  
  exponent <- exp_top/exp_bot
  
  # final shield multiplier
  shield_multiplier <- min_mult + (normalised_hull_mass^exponent)*(max_mult-min_mult)
  
  shileding <- base_shield*shield_multiplier
  
  return(shileding)
}


