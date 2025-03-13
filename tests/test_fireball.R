# script for tests for fireball 

# Load required libraries
library(testthat)
library(dplyr)

# Source the main fireball functions script
source("../R/fireball.R")  # Adjust the path to point to your fireball script

# ------------------ TEST dice_roll() FUNCTION ------------------ #
test_dice_roll <- function() {
  test_that("dice_roll() returns numeric vector", {
    rolls <- dice_roll(n = 5, d = 20)
    expect_true(is.numeric(rolls))
  })
  
  test_that("dice_roll() returns values within expected range", {
    rolls <- dice_roll(n = 100, d = 12)
    expect_true(all(rolls >= 1 & rolls <= 12))
  })
  
  test_that("dice_roll() returns correct number of rolls", {
    rolls <- dice_roll(n = 10, d = 6)
    expect_length(rolls, 10)
  })
}

# ------------------ TEST targets() FUNCTION ------------------ #
test_targets <- function() {
  test_that("targets() produces a dataframe with expected columns", {
    goblins <- targets(n = 5, name = "Goblin", HP = 10, proficiency_bonus = 2, ability_modifier = 1)
    expect_true("name" %in% colnames(goblins))
    expect_true("HP" %in% colnames(goblins))
    expect_true("proficiency_bonus" %in% colnames(goblins))
    expect_true("ability_modifier" %in% colnames(goblins))
  })
  
  test_that("targets() assigns correct enemy names", {
    goblins <- targets(n = 3, name = "Goblin", HP = 10, proficiency_bonus = 2, ability_modifier = 1)
    expect_match(goblins$name[1], "Goblin 1")
    expect_match(goblins$name[2], "Goblin 2")
    expect_match(goblins$name[3], "Goblin 3")
  })
  
  test_that("targets() handles null or empty resistance lists correctly", {
    goblins <- targets(n = 2, name = "Goblin", HP = 10, proficiency_bonus = 2, ability_modifier = 1, resistances = NULL)
    expect_null(goblins$resistances[[1]])
  })
}

# ------------------ TEST fireball() FUNCTION ------------------ #
test_fireball <- function() {
  test_that("fireball() applies at least some damage", {
    goblins <- targets(n = 5, name = "Goblin", HP = 10, proficiency_bonus = 2, ability_modifier = 1)
    result <- fireball(goblins, DC = 12, spell_lvl = 3)
    expect_true(any(result$dmg > 0))
  })
  
  test_that("fireball() never reduces HP below zero", {
    goblins <- targets(n = 3, name = "Goblin", HP = 5, proficiency_bonus = 2, ability_modifier = 1)
    result <- fireball(goblins, DC = 12, spell_lvl = 5)
    expect_true(all(result$HP >= 0))
  })
  
  test_that("fireball() correctly processes fire immunity", {
    immune_enemy <- targets(n = 1, name = "Fire Demon", HP = 50, proficiency_bonus = 2, ability_modifier = 3, immunities = "Fire")
    result <- fireball(immune_enemy, DC = 12, spell_lvl = 3)
    expect_equal(result$dmg, 0)
  })
  
  test_that("fireball() correctly applies fire vulnerability", {
    vulnerable_enemy <- targets(n = 1, name = "Dry Wood Monster", HP = 50, proficiency_bonus = 2, ability_modifier = 1, vulnerabilities = "Fire")
    result <- fireball(vulnerable_enemy, DC = 12, spell_lvl = 3)
    expect_true(result$dmg > 10)
  })
  
  test_that("fireball() correctly applies fire resistance", {
    resistant_enemy <- targets(n = 1, name = "Fire Spirit", HP = 50, proficiency_bonus = 2, ability_modifier = 1, resistances = "Fire")
    result <- fireball(resistant_enemy, DC = 12, spell_lvl = 3)
    expect_true(result$dmg > 0)
    expect_true(result$dmg < 30)
  })
}

# ------------------ TEST clean_dead() FUNCTION ------------------ #
test_clean_dead <- function() {
  test_that("clean_dead() removes only dead enemies", {
    goblins <- targets(n = 5, name = "Goblin", HP = 10, proficiency_bonus = 2, ability_modifier = 1)
    result <- fireball(goblins, DC = 12, spell_lvl = 5)
    survivors <- clean_dead(result)
    
    expect_true(all(survivors$HP > 0))
    expect_true(nrow(survivors) <= nrow(goblins))
  })
  
  test_that("clean_dead() does not remove living enemies", {
    goblins <- targets(n = 5, name = "Goblin", HP = 50, proficiency_bonus = 2, ability_modifier = 1)
    result <- fireball(goblins, DC = 12, spell_lvl = 1)
    survivors <- clean_dead(result)
    
    expect_equal(nrow(survivors), nrow(goblins))
  })
}

# ------------------ RUN ALL TESTS ------------------ #
run_all_tests <- function() {
  test_dice_roll()
  test_targets()
  test_fireball()
  test_clean_dead()
  message("All tests completed!")
}