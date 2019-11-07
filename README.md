# fantasy_football_example


This repository includes data and code for a fantasy football dashboard tracking indivudal player data for wide receivers and running backs in the 2018 NFL season. Data is sourced using the NFLscrapR package.

There are 3 main files:
1) nfl_data.r - reads in raw play-by-play data, cleans it, creates custom metrics, aggregates data to the player level, and outputs spreadsheets of clean data.
2) nfl_interactive_mod.r - creates UI for dashbaord tracker using new aggregated data by player. Dashboard contains two interactive panels, one for WRs and one for RBs and multiple tabs on each panel containing interactive charts via plotly and summary statistics.
3) ff_app.R - app file that calls the UI and SI to set up the structure. 
