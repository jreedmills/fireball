fireball <- function(targets, DC = 12, spell_lvl = 3){
  
  save_roll_raw = numeric(nrow(targets))
  save_roll = numeric(nrow(targets))
  saved = logical(nrow(targets))
  dmg_rolls = vector("list", nrow(targets))
  dmg = numeric(nrow(targets))
  
  for (i in 1:nrow(targets)) {
    
    # Saving Throw Calculation
    save_roll_raw[i] = dice_roll(1,20)
    save_roll[i] = save_roll_raw[i] + targets$proficiency_bonus[i] + targets$ability_modifier[i]
    saved[i] = save_roll[i] >= DC  # TRUE if save is successful
    
    # Base Fireball Damage
    dmg_rolls[[i]] = dice_roll(spell_lvl + 5, 6)
    base_dmg = sum(unlist(dmg_rolls[i]))
    
    # Apply Half Damage if Saved
    if (saved[i]) {
      base_dmg = floor(0.5 * base_dmg)
    }
    
    # Check Fire Resistance, Vulnerability, and Immunity
    if ("Fire" %in% targets$immunities[[i]]) {
      dmg[i] = 0  # Immune means no damage
    } else if ("Fire" %in% targets$resistances[[i]]) {
      dmg[i] = floor(0.5 * base_dmg)  # Resistance halves the damage
    } else if ("Fire" %in% targets$vulnerabilities[[i]]) {
      dmg[i] = 2 * base_dmg  # Vulnerability doubles the damage
    } else {
      dmg[i] = base_dmg  # Default damage
    }
    
    # Update Target HP and Stats
    targets$HP_starting[i] = targets$HP[i]
    targets$save_roll_raw[i] = save_roll_raw[i]
    targets$save_roll[i] = save_roll[i]
    targets$DC[i] = DC
    targets$saved[i] = saved[i]
    targets$dmg_rolls[i] = paste(dmg_rolls[i], collapse = ",")
    targets$dmg[i] = as.integer(dmg[i])
    targets$HP[i] = max(0, targets$HP[i] - dmg[i])  # Prevent HP from going below 0
    targets$alive[i] = targets$HP[i] > 0
    
  }
  
  targets <- targets |>
    mutate(dmg = as.numeric(dmg))
  
  return(targets)
}