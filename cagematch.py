"""
Cagematch.net scraper for WWE event and match data.
"""

import re
import time
from datetime import datetime, date
from typing import Optional
import requests
from bs4 import BeautifulSoup
import pandas as pd


SCRAPE_DELAY = 0.03  # seconds between requests
BASE_URL = "https://www.cagematch.net"

# Show type constants
SHOWTYPE_BROADCAST = "TV-Show%7CPay+Per+View%7CPremium+Live+Event%7COnline+Stream"
SHOWTYPE_PPVS = "Pay+Per+View%7CPremium+Live+Event"


def fetch_page(url: str, delay: float = SCRAPE_DELAY) -> Optional[BeautifulSoup]:
    """Fetch a page and return parsed HTML."""
    time.sleep(delay)
    try:
        response = requests.get(url, timeout=30)
        response.raise_for_status()
        return BeautifulSoup(response.text, 'html.parser')
    except Exception as e:
        print(f"Error fetching {url}: {e}")
        return None


def fetch_cagematch_page(year: int, showtype: str, page: int = 1, verbose: bool = False) -> pd.DataFrame:
    """Fetch a single page of events for a given year and show type."""
    offset = (page - 1) * 100
    url = (
        f"{BASE_URL}/?id=8&nr=1&page=4&name=&vDay=&vMonth=&vYear={year}"
        f"&showtype={showtype}&location=&arena=&region="
        f"&minRating=&maxRating=&minVotes=&maxVotes=&s={offset}"
    )

    if verbose:
        print(f"Fetching: {url}")

    doc = fetch_page(url)
    if doc is None:
        return pd.DataFrame()

    rows = doc.select("tr[class*='TRow']")
    if not rows:
        return pd.DataFrame()

    results = []
    for row in rows:
        cols = row.select("td")
        if len(cols) < 5:
            continue

        col_texts = [col.get_text(strip=True) for col in cols]
        event_date_str = col_texts[1] if len(col_texts) > 1 else ""
        event_name = col_texts[3] if len(col_texts) > 3 else ""

        # Parse date (DD.MM.YYYY format)
        try:
            event_date = datetime.strptime(event_date_str, "%d.%m.%Y").date()
        except ValueError:
            continue

        # Get event URL and extract event ID
        link = cols[3].select_one("a") if len(cols) > 3 else None
        event_url = link.get("href", "") if link else ""
        event_id_match = re.search(r"nr=(\d+)", event_url)
        event_id = event_id_match.group(1) if event_id_match else ""

        results.append({
            "Date": event_date,
            "Event Name": event_name,
            "event": event_id,
            "event_url": event_url
        })

    return pd.DataFrame(results)


def fetch_cagematch_events(year: int, showtype: str, max_pages: int = 20, verbose: bool = False) -> pd.DataFrame:
    """Fetch all events for a given year and show type."""
    all_results = []

    for page in range(1, max_pages + 1):
        df_page = fetch_cagematch_page(year, showtype, page, verbose)
        if df_page.empty:
            break
        all_results.append(df_page)

    if not all_results:
        return pd.DataFrame()
    return pd.concat(all_results, ignore_index=True)


def fetch_cagematch_events_range(
    start_date: date,
    end_date: date,
    showtype: str,
    max_pages: int = 20,
    verbose: bool = False
) -> pd.DataFrame:
    """Fetch events within a specific date range."""
    all_events = []

    for year in range(start_date.year, end_date.year + 1):
        events_year = fetch_cagematch_events(year, showtype, max_pages, verbose)
        if not events_year.empty:
            # Filter to date range
            mask = (events_year["Date"] >= start_date) & (events_year["Date"] <= end_date)
            filtered = events_year[mask].copy()
            filtered["year"] = year
            all_events.append(filtered)

    if not all_events:
        return pd.DataFrame()
    return pd.concat(all_events, ignore_index=True)


def fetch_card(event_id: str, scrape_delay: float = SCRAPE_DELAY) -> dict:
    """Fetch detailed card data for a given event."""

    # Part 1: Fetch match results
    url1 = f"{BASE_URL}/?id=1&nr={event_id}"
    print(f"Reading results from: {url1}")
    doc1 = fetch_page(url1, scrape_delay)

    if doc1 is None:
        return {"df": pd.DataFrame(), "wrestlers": pd.DataFrame(), "teams_summary": pd.DataFrame()}

    match_nodes = doc1.select(".Match")
    all_wrestlers = []
    results_list = []
    taped_dates = []

    for match_node in match_nodes:
        results_node = match_node.select_one(".MatchResults")
        if results_node is None:
            continue

        raw_text = results_node.get_text(separator=" ", strip=True)

        # Check for taped date
        taped_date = None
        note_node = match_node.select_one(".MatchNotes")
        if note_node:
            note_text = note_node.get_text()
            taped_match = re.search(r"taped on (\d{2}\.\d{2}\.\d{4})", note_text, re.IGNORECASE)
            if taped_match:
                try:
                    taped_date = datetime.strptime(taped_match.group(1), "%d.%m.%Y").date()
                except ValueError:
                    pass
        taped_dates.append(taped_date)

        # Extract wrestler info
        links = results_node.select("a")
        match_wrestlers = []
        for link in links:
            href = link.get("href", "")
            name = link.get_text(strip=True)
            id_match = re.search(r"&nr=(\d+)", href)
            wrestler_id = id_match.group(1) if id_match else ""
            match_wrestlers.append({
                "wrestler_names": name,
                "wrestler_ids": wrestler_id,
                "wrestler_links": href
            })
            all_wrestlers.append({
                "wrestler_names": name,
                "wrestler_ids": wrestler_id,
                "wrestler_links": href
            })

        # Determine match outcome
        defeat_occurred = "defeats" in raw_text.lower()

        match_results = {}
        if defeat_occurred:
            defeat_pos = raw_text.lower().find("defeats")
            for w in match_wrestlers:
                name_pos = raw_text.find(w["wrestler_names"])
                if name_pos >= 0:
                    result = 1 if name_pos < defeat_pos else -1
                else:
                    result = 0
                match_results[w["wrestler_ids"]] = result
        else:
            for w in match_wrestlers:
                match_results[w["wrestler_ids"]] = 0

        match_results["taped_date"] = taped_date
        results_list.append(match_results)

    # Part 2: Fetch match card for team composition
    url2 = f"{BASE_URL}/?id=1&nr={event_id}&page=2"
    print(f"Reading card from: {url2}")
    doc2 = fetch_page(url2, scrape_delay)

    teams_data = []
    if doc2:
        card_nodes = doc2.select(".Match")

        for match_idx, card_node in enumerate(card_nodes):
            card_text = card_node.get_text(separator=" ", strip=True)
            # Remove match number prefix
            card_text = re.sub(r"^.*?match\s*", "", card_text, flags=re.IGNORECASE)

            links = card_node.select("a")
            wrestlers_in_match = []
            for link in links:
                href = link.get("href", "")
                name = link.get_text(strip=True)
                id_match = re.search(r"&nr=(\d+)", href)
                if id_match:
                    wrestlers_in_match.append({
                        "name": name,
                        "id": id_match.group(1)
                    })

            # Split by "vs." to get teams
            teams = re.split(r"vs\.", card_text, flags=re.IGNORECASE)

            for team_idx, team_text in enumerate(teams):
                team_text = team_text.strip()
                team_wrestler_ids = []
                for w in wrestlers_in_match:
                    if w["name"] in team_text:
                        team_wrestler_ids.append(w["id"])

                if team_wrestler_ids:
                    # Get outcome for this team from results
                    team_outcome = None
                    if match_idx < len(results_list):
                        for wid in team_wrestler_ids:
                            result = results_list[match_idx].get(wid)
                            if result == 1:
                                team_outcome = "winner"
                                break
                            elif result == -1:
                                team_outcome = "loser"
                                break
                            elif result == 0:
                                team_outcome = "tie"

                    taped = results_list[match_idx].get("taped_date") if match_idx < len(results_list) else None

                    teams_data.append({
                        "match_index": match_idx + 1,
                        "team_number": team_idx + 1,
                        "raw_team_text": team_text,
                        "wrestlers_in_team": team_wrestler_ids,
                        "team_outcome": team_outcome,
                        "taped_date": taped
                    })

    return {
        "df": pd.DataFrame(results_list) if results_list else pd.DataFrame(),
        "wrestlers": pd.DataFrame(all_wrestlers).drop_duplicates() if all_wrestlers else pd.DataFrame(),
        "teams_summary": pd.DataFrame(teams_data) if teams_data else pd.DataFrame()
    }


def fetch_card_safe(event_id: str, scrape_delay: float = SCRAPE_DELAY) -> dict:
    """Safe version of fetch_card that catches exceptions."""
    try:
        result = fetch_card(event_id, scrape_delay)
        return {"result": result, "error": None}
    except Exception as e:
        print(f"Error fetching card for event {event_id}: {e}")
        return {"result": None, "error": str(e)}
