# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

CardScraper is an R-based project for analyzing pro wrestling match data using Bayesian skill modeling. It scrapes WWE match data from Cagematch.net and WrestlingData.com, then applies TrueSkill2 ratings to estimate wrestler performance over time.

## Key Commands

**Initial full data scrape (one-time, takes hours):**
```R
source("cagematch_scrape.R")
```

**Daily incremental update:**
```bash
Rscript daily_update.R
```

**Run analysis and visualization:**
```R
source("analysis.R")
source("dataviz.R")
```

## Architecture

### Three-Layer Design

1. **Scraping Layer** - Fetches raw match data from websites
   - `cagematch_scrape.R` - Full historical scrape (1984-present)
   - `cagematch_helpers.R` - API helper functions for Cagematch queries
   - `daily_update.R` - Incremental updates with new events
   - `wrestlingdata_scrape.R` - TV ratings data

2. **Modeling Layer** - Bayesian skill estimation
   - `trueskill2.R` - Custom TrueSkill2 implementation with Gaussian math
   - `elo_models.R` / `elo_inference.R` - Alternative ELO-based models

3. **Analysis Layer** - Visualization and rankings
   - `analysis.R` - Data analysis framework
   - `dataviz.R` - Interactive plots and rankings

### Data Flow

```
Cagematch.net → fetch_cagematch_events() → Event metadata
    → fetch_card() → Match results + Wrestler lists
    → Team composition parsing → Wide format (w1, w2..., l1, l2...)
    → df.Rds → TrueSkill2 updates → players.Rds + df_lc.Rds
```

### Key Data Files

- `df.Rds` - Main event/match dataset (~130MB nested tibble)
- `df_lc.Rds` - Learning curves (skill history over time)
- `players.Rds` - Current TrueSkill2 player ratings (hash structure)
- `wrestler_names.Rds` - Wrestler ID to name mapping

### Match Result Encoding

- Winners: `1`, Losers: `-1`, Draws/No Decision: `0`
- Wide format columns: `w<id>` for winners, `l<id>` for losers

## Key Patterns

**Nested Tibbles**: Match data is stored as nested lists within tibbles; unnesting happens at analysis time.

**Stateful Model Updates**: Player ratings persist in `players.Rds`. The daily update loads old ratings, updates with new matches, then saves.

**Safe Scraping**: Uses `purrr::safely()` to catch errors without stopping pipeline, with `Sys.sleep()` delays between requests.

**TrueSkill2 Player State**: Each player has μ (skill mean, typically -5 to +5) and σ (uncertainty, typically 2-5).

## Dependencies

R 4.0+ with tidyverse, rvest (scraping), ggplot2/plotly (visualization), and custom TrueSkill2 implementation.
