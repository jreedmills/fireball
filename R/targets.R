targets <- function(n = 1, HP, proficiency_bonus, ability_modifier, name, 
                    resistances = "None", vulnerabilities = "None", immunities = "None") {
  
  # Expand values to match enemy counts
  enemy_counts <- rep(name, n)
  HP_expanded <- rep(HP, n)
  proficiency_bonus_expanded <- rep(proficiency_bonus, n)
  ability_modifier_expanded <- rep(ability_modifier, n)
  
  # Number duplicate names (e.g., "Goblin 1", "Goblin 2", ..., "Orc 1", ...)
  enemy_counts <- ave(enemy_counts, enemy_counts, FUN = function(x) if (length(x) > 1) paste0(x, " ", seq_along(x)) else x)
  
  # Helper function to process resistances, vulnerabilities, and immunities
  parse_traits <- function(input, enemy_total) {
    if (is.null(input)) {
      return(rep(list(character(0)), enemy_total))  # Default to empty list
    }
    input <- as.character(input)
    input <- ifelse(input == "", "None", input)
    trait_list <- lapply(strsplit(input, ",\\s*"), function(x) if ("None" %in% x) character(0) else x)
    return(rep(trait_list, length.out = enemy_total))  # Expand correctly
  }
  
  # Process resistances, vulnerabilities, and immunities
  total_enemies <- sum(n)
  resistances_expanded <- parse_traits(resistances, total_enemies)
  vulnerabilities_expanded <- parse_traits(vulnerabilities, total_enemies)
  immunities_expanded <- parse_traits(immunities, total_enemies)
  
  # Create DataFrame
  combined_targets <- data.frame(
    name = enemy_counts,
    HP = HP_expanded,
    proficiency_bonus = proficiency_bonus_expanded,
    ability_modifier = ability_modifier_expanded,
    resistances = I(resistances_expanded),
    vulnerabilities = I(vulnerabilities_expanded),
    immunities = I(immunities_expanded)
  )
  
  combined_targets <- combined_targets |>
    mutate(species = str_remove(name, "\\s\\d+$"))  # Remove only the last number (after the last space)
  
  
  return(combined_targets)
}