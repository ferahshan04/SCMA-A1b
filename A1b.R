install.packages("pacman")
require("pacman")
library("pacman")
library(datasets)
install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
setwd('E:\\VCU\\Summer 2024\\Statistical Analysis & Modeling')
ball_by_ball_data = read.csv('IPL_ball_by_ball_updated till 2024.csv')
salary_data = read_excel('IPL SALARIES 2024.xlsx')
install.packages("tidyverse")
library(tidyverse)
player_stats <- ball_by_ball_data %>%
  group_by(Match.id, Season, Striker, Bowler) %>%
  summarise(
    runs = sum(runs_scored, na.rm = TRUE),
    wickets = sum(ifelse(!is.na(wicket_confirmation) & wicket_confirmation == 1, 1, 0), na.rm = TRUE)
  ) %>%
  ungroup()
top_wicket_takers <- player_stats %>%
  group_by(Season, Bowler) %>%
  summarise(total_wickets = sum(wickets, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Season, desc(total_wickets)) %>%
  group_by(Season) %>%
  slice_head(n = 3)
print("Top Wicket Takers by Round:")
print(top_wicket_takers)
top_run_getters <- player_stats %>%
  group_by(Season, Striker) %>%
  summarise(total_runs = sum(runs, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Season, desc(total_runs)) %>%
  group_by(Season) %>%
  slice_head(n = 3) 
print("Top Run Getters by Round:")
print(top_run_getters)

install.packages("fitdistrplus")
library(fitdistrplus)
  
  for (season in unique(top_run_getters$Season)) {
    cat("\nSeason:", season, "\n")
    cat("Top 3 Batsmen:\n")
    season_data <- top_run_getters %>% filter(Season == season)
    print(season_data)
    print("Top Wicket Takers by Season:")
    for (season in unique(top_wicket_takers$Season)) {
      cat("\nSeason:", season, "\n")
      cat("Top 3 Wicket Takers:\n")
      
      install.packages("ggplot2")
      library(ggplot2)
      last_three_seasons <- c(2022, 2023, 2024)
      filtered_data <- ball_by_ball_data %>%
        filter(Season %in% last_three_seasons)
      player_stats <- filtered_data %>%
        group_by(Match.id, Season, Striker, Bowler) %>%
        summarise(
          runs = sum(runs_scored, na.rm = TRUE),
          wickets = sum(ifelse(!is.na(wicket_confirmation) & wicket_confirmation == 1, 1, 0), na.rm = TRUE)
        ) %>%
        ungroup()
      top_run_getters <- player_stats %>%
        group_by(Season, Striker) %>%
        summarise(total_runs = sum(runs, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(Season, desc(total_runs)) %>%
        group_by(Season) %>%
        slice_head(n = 3)
      top_wicket_takers <- player_stats %>%
        group_by(Season, Bowler) %>%
        summarise(total_wickets = sum(wickets, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(Season, desc(total_wickets)) %>%
        group_by(Season) %>%
        slice_head(n = 3)
      print("Top Run Getters by Season:")
      for (season in unique(top_run_getters$Season)) {
        cat("\nSeason:", season, "\n")
        cat("Top 3 Batsmen:\n")
        season_data <- top_run_getters %>% filter(Season == season)
        print(season_data)
      }
      print("Top Wicket Takers by Season:")
      for (season in unique(top_wicket_takers$Season)) {
        cat("\nSeason:", season, "\n")
        cat("Top 3 Wicket Takers:\n")
        season_data <- top_wicket_takers %>% filter(Season == season)
        print(season_data)
      }
      runs_data <- top_run_getters$total_runs
      wickets_data <- top_wicket_takers$total_wickets
      runs_data_exp <- runs_data[runs_data > 0]
      fit_runs_norm <- fitdist(runs_data, "norm")
      fit_runs_exp <- tryCatch(fitdist(runs_data_exp, "exp"), error = function(e) NULL)
      fit_runs_pois <- fitdist(runs_data, "pois") 
      runs_fits <- list(norm = fit_runs_norm, pois = fit_runs_pois)
      if (!is.null(fit_runs_exp)) {
        runs_fits$exp <- fit_runs_exp
      }
      goftest_runs <- gofstat(runs_fits)
      print(goftest_runs)
      wickets_data_exp <- wickets_data[wickets_data > 0]
      fit_wickets_norm <- fitdist(wickets_data, "norm")
      fit_wickets_exp <- tryCatch(fitdist(wickets_data_exp, "exp"), error = function(e) NULL)
      fit_wickets_pois <- fitdist(wickets_data, "pois")
      wickets_fits <- list(norm = fit_wickets_norm, pois = fit_wickets_pois)
      if (!is.null(fit_wickets_exp)) {
        wickets_fits$exp <- fit_wickets_exp
      }
      goftest_wickets <- gofstat(wickets_fits)
      print(goftest_wickets)
      par(mfrow = c(2, 2))
      plot(fit_runs_norm)
      if (!is.null(fit_runs_exp)) {
        plot(fit_runs_exp)
      }
      plot(fit_runs_pois)
      performance_metrics <- filtered_data %>%
        group_by(Striker) %>%
        summarise(
          total_runs = sum(runs_scored, na.rm = TRUE),
          matches_played = n_distinct(Match.id),
          average_runs = mean(runs_scored, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        rename(Player = Striker)
      bowler_metrics <- filtered_data %>%
        group_by(Bowler) %>%
        summarise(
          total_wickets = sum(ifelse(!is.na(wicket_confirmation) & wicket_confirmation == 1, 1, 0), na.rm = TRUE),
          matches_played = n_distinct(Match.id),
          average_wickets = mean(ifelse(!is.na(wicket_confirmation) & wicket_confirmation == 1, 1, 0), na.rm = TRUE)
        ) %>%
        ungroup() %>%
        rename(Player = Bowler)
      combined_metrics <- full_join(performance_metrics, bowler_metrics, by = "Player")
      performance_salary_data <- left_join(salary_data, combined_metrics, by = "Player")
      performance_salary_data[is.na(performance_salary_data)] <- 0
      rgsharma_data <- performance_salary_data %>% filter(Player == "RG Sharma")
      print("RG Sharma's Performance and Salary Data:")
      print(rgsharma_data)
      ggplot(rgsharma_data, aes(x = Salary)) +
        geom_bar(aes(y = total_runs), stat = "identity", fill = "blue", alpha = 0.7) +
        geom_bar(aes(y = total_wickets), stat = "identity", fill = "red", alpha = 0.7) +
        labs(title = "RG Sharma's Performance Metrics and Salary",
             x = "Salary",
             y = "Performance Metrics",
             fill = "Metric") +
        theme_minimal() +
        scale_y_continuous(sec.axis = sec_axis(~., name = "Total Wickets")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))