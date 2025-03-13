clean_dead <- function(targets){
  targets = targets |>
    filter(alive) |>
    select(any_of(c("army", "species", "name", "HP", "proficiency_bonus", "ability_modifier", "resistances", "vulnerabilities", "immunities")))
  
  return(targets)
}