#!/usr/bin/env python3
"""
Daily update script for CardScraper.
Fetches new WWE events and updates TrueSkill ratings.
"""

import json
from datetime import date, timedelta
from pathlib import Path
import pandas as pd
from cagematch import (
    fetch_cagematch_events_range,
    fetch_card_safe,
    SHOWTYPE_BROADCAST,
    SHOWTYPE_PPVS
)
from trueskill_model import PlayerRatings


DATA_DIR = Path(".")
EVENTS_FILE = DATA_DIR / "events.json"
WRESTLERS_FILE = DATA_DIR / "wrestlers.json"
RATINGS_FILE = DATA_DIR / "players.json"
HISTORY_FILE = DATA_DIR / "skill_history.json"


def load_json(path: Path) -> list | dict:
    """Load JSON file, returning empty list/dict if not found."""
    if path.exists():
        with open(path, "r") as f:
            return json.load(f)
    return [] if "events" in path.name or "history" in path.name else {}


def save_json(data, path: Path) -> None:
    """Save data to JSON file."""
    with open(path, "w") as f:
        json.dump(data, f, indent=2, default=str)


def main():
    print("=== CardScraper Daily Update ===")

    # Step 1: Load existing data
    events = load_json(EVENTS_FILE)
    wrestlers = load_json(WRESTLERS_FILE)
    skill_history = load_json(HISTORY_FILE)

    print(f"Loaded {len(events)} existing events")

    # Step 2: Determine date range for new events
    if events:
        last_date = max(e["Date"] for e in events)
        if isinstance(last_date, str):
            last_date = date.fromisoformat(last_date)
        start_date = last_date + timedelta(days=1)
    else:
        start_date = date(1984, 1, 1)

    end_date = date.today()

    if start_date > end_date:
        print("No new events to scrape. Data is up to date.")
        new_events_df = pd.DataFrame()
    else:
        print(f"Scraping events from {start_date} to {end_date}")

        # Fetch new events
        broadcast_df = fetch_cagematch_events_range(start_date, end_date, SHOWTYPE_BROADCAST, verbose=True)
        ppv_df = fetch_cagematch_events_range(start_date, end_date, SHOWTYPE_PPVS, verbose=True)

        new_events_df = pd.concat([broadcast_df, ppv_df], ignore_index=True)
        if not new_events_df.empty:
            new_events_df = new_events_df.drop_duplicates(subset=["event"]).sort_values("Date")

        print(f"Found {len(new_events_df)} new events")

    # Step 3: Process new events
    new_events_processed = []
    new_teams_data = []

    if not new_events_df.empty:
        for _, row in new_events_df.iterrows():
            event_id = str(row["event"])
            print(f"Processing event {event_id}: {row['Event Name']}")

            card_result = fetch_card_safe(event_id)

            if card_result["error"]:
                print(f"  Error: {card_result['error']}")
                continue

            card = card_result["result"]

            # Store event
            event_data = {
                "Date": str(row["Date"]),
                "Event Name": row["Event Name"],
                "event": event_id,
                "event_url": row.get("event_url", ""),
                "year": int(row.get("year", row["Date"].year if hasattr(row["Date"], "year") else 0))
            }
            new_events_processed.append(event_data)

            # Store wrestlers
            if not card["wrestlers"].empty:
                for _, w in card["wrestlers"].iterrows():
                    wid = str(w["wrestler_ids"])
                    if wid and wid not in wrestlers:
                        wrestlers[wid] = {
                            "wrestler_id": wid,
                            "wrestler_name": w["wrestler_names"],
                            "wrestler_link": w.get("wrestler_links", "")
                        }

            # Store teams data for model updates
            if not card["teams_summary"].empty:
                for _, team in card["teams_summary"].iterrows():
                    if team.get("team_outcome") and team.get("wrestlers_in_team"):
                        new_teams_data.append({
                            "event": event_id,
                            "date": str(row["Date"]),
                            "match_index": team["match_index"],
                            "team_number": team["team_number"],
                            "wrestlers": team["wrestlers_in_team"],
                            "outcome": team["team_outcome"],
                            "taped_date": str(team["taped_date"]) if team.get("taped_date") else None
                        })

    # Step 4: Update events list
    events.extend(new_events_processed)
    existing_event_ids = {e["event"] for e in events}
    events = [e for e in events if e["event"] in existing_event_ids]  # Deduplicate by event ID

    # Deduplicate properly
    seen = set()
    unique_events = []
    for e in events:
        if e["event"] not in seen:
            seen.add(e["event"])
            unique_events.append(e)
    events = sorted(unique_events, key=lambda x: x["Date"])

    save_json(events, EVENTS_FILE)
    print(f"Saved {len(events)} total events to {EVENTS_FILE}")

    # Step 5: Save wrestlers
    save_json(wrestlers, WRESTLERS_FILE)
    print(f"Saved {len(wrestlers)} wrestlers to {WRESTLERS_FILE}")

    # Step 6: Update TrueSkill ratings
    print("Updating TrueSkill ratings...")
    ratings = PlayerRatings(str(RATINGS_FILE))

    # Group matches by event and match_index
    matches = {}
    for team in new_teams_data:
        key = (team["event"], team["match_index"])
        if key not in matches:
            matches[key] = {"winners": [], "losers": [], "date": team["date"]}

        wrestler_ids = [str(w) for w in team["wrestlers"]]
        if team["outcome"] == "winner":
            matches[key]["winners"].extend(wrestler_ids)
        elif team["outcome"] == "loser":
            matches[key]["losers"].extend(wrestler_ids)

    # Process matches
    for (event_id, match_idx), match in matches.items():
        if match["winners"] and match["losers"]:
            ratings.update_match(match["winners"], match["losers"])

    ratings.save()
    print(f"Saved ratings for {len(ratings.players)} wrestlers to {RATINGS_FILE}")

    # Step 7: Update skill history
    today_str = str(date.today())
    today_snapshot = {
        "date": today_str,
        "ratings": {
            pid: {"mu": r.mu, "sigma": r.sigma}
            for pid, r in ratings.players.items()
        }
    }
    skill_history.append(today_snapshot)
    save_json(skill_history, HISTORY_FILE)
    print(f"Saved skill history snapshot to {HISTORY_FILE}")

    # Step 8: Generate summary stats
    rankings = ratings.get_rankings()
    summary_df = pd.DataFrame(rankings)
    if not summary_df.empty:
        # Add wrestler names
        summary_df["wrestler_name"] = summary_df["wrestler_id"].map(
            lambda x: wrestlers.get(str(x), {}).get("wrestler_name", "Unknown")
        )
        summary_df = summary_df[["wrestler_id", "wrestler_name", "mu", "sigma", "conservative_rating"]]
        summary_df.to_csv("wrestler_summary_stats.csv", index=False)
        print("Saved wrestler_summary_stats.csv")

    print("=== Daily update completed ===")


if __name__ == "__main__":
    main()
