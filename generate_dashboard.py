#!/usr/bin/env python3
"""
Generate a static HTML dashboard for CardScraper wrestling analytics.
"""

import json
from datetime import date, datetime
from pathlib import Path
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots


DATA_DIR = Path(".")
OUTPUT_FILE = DATA_DIR / "index.html"


def load_json(path: Path):
    """Load JSON file."""
    if path.exists():
        with open(path, "r") as f:
            return json.load(f)
    return None


def create_skill_history_chart(skill_history: list, wrestlers: dict) -> go.Figure:
    """Create interactive skill history chart."""
    if not skill_history or len(skill_history) < 2:
        return go.Figure().add_annotation(text="Not enough history data", showarrow=False)

    # Build time series data
    data = []
    for snapshot in skill_history:
        snapshot_date = snapshot["date"]
        for wrestler_id, rating in snapshot["ratings"].items():
            wrestler_name = wrestlers.get(wrestler_id, {}).get("wrestler_name", f"ID:{wrestler_id}")
            data.append({
                "date": snapshot_date,
                "wrestler_id": wrestler_id,
                "wrestler_name": wrestler_name,
                "mu": rating["mu"]
            })

    df = pd.DataFrame(data)
    if df.empty:
        return go.Figure().add_annotation(text="No data available", showarrow=False)

    # Filter to wrestlers with significant history
    wrestler_counts = df.groupby("wrestler_id").size()
    active_wrestlers = wrestler_counts[wrestler_counts >= 5].index.tolist()
    df = df[df["wrestler_id"].isin(active_wrestlers)]

    fig = px.line(
        df,
        x="date",
        y="mu",
        color="wrestler_name",
        title="Wrestler Skill Estimates Over Time",
        labels={"mu": "Estimated Skill (μ)", "date": "Date", "wrestler_name": "Wrestler"},
        hover_data=["wrestler_name"]
    )

    fig.update_traces(opacity=0.6)
    fig.update_layout(
        showlegend=False,
        hovermode="closest",
        xaxis_title="Date",
        yaxis_title="Bayesian Skill Estimate (μ)",
        height=600
    )
    fig.add_hline(y=25, line_dash="dash", line_color="gray", annotation_text="Average")

    return fig


def create_rankings_chart(summary_df: pd.DataFrame) -> go.Figure:
    """Create current rankings chart with confidence intervals."""
    if summary_df.empty:
        return go.Figure().add_annotation(text="No rankings data", showarrow=False)

    # Add tier categories
    df = summary_df.copy()
    df["tier"] = pd.cut(
        df["mu"],
        bins=[-float("inf"), 20, 25, 30, float("inf")],
        labels=["Jobbers", "Midcarders", "Upper Card", "Main Event"]
    )

    # Take top wrestlers per tier
    top_per_tier = df.groupby("tier").head(15)

    fig = go.Figure()

    colors = {"Main Event": "gold", "Upper Card": "silver", "Midcarders": "lightblue", "Jobbers": "lightgray"}

    for tier in ["Main Event", "Upper Card", "Midcarders", "Jobbers"]:
        tier_data = top_per_tier[top_per_tier["tier"] == tier].sort_values("mu", ascending=True)
        if tier_data.empty:
            continue

        # Error bars showing uncertainty (2 sigma)
        fig.add_trace(go.Scatter(
            x=tier_data["mu"],
            y=tier_data["wrestler_name"],
            mode="markers",
            name=tier,
            marker=dict(size=10, color=colors.get(tier, "gray")),
            error_x=dict(
                type="data",
                array=tier_data["sigma"] * 2,
                visible=True
            ),
            hovertemplate="<b>%{y}</b><br>Rating: %{x:.2f}<extra></extra>"
        ))

    fig.update_layout(
        title="Current Wrestler Rankings by Tier",
        xaxis_title="Skill Rating (μ) with 95% CI",
        yaxis_title="",
        height=800,
        showlegend=True,
        legend_title="Tier"
    )

    return fig


def create_stats_section(events: list, wrestlers: dict, summary_df: pd.DataFrame) -> str:
    """Create HTML for statistics section."""
    total_events = len(events) if events else 0
    total_wrestlers = len(wrestlers) if wrestlers else 0

    if events:
        dates = [e["Date"] for e in events]
        min_date = min(dates)
        max_date = max(dates)
    else:
        min_date = max_date = "N/A"

    top_10_html = ""
    if not summary_df.empty:
        top_10 = summary_df.head(10)
        top_10_html = "<table class='stats-table'><tr><th>Rank</th><th>Wrestler</th><th>Rating</th><th>Uncertainty</th></tr>"
        for i, (_, row) in enumerate(top_10.iterrows(), 1):
            top_10_html += f"<tr><td>{i}</td><td>{row['wrestler_name']}</td><td>{row['mu']:.2f}</td><td>±{row['sigma']:.2f}</td></tr>"
        top_10_html += "</table>"

    return f"""
    <div class="stats-grid">
        <div class="stat-card">
            <h3>Total Events</h3>
            <p class="stat-value">{total_events:,}</p>
        </div>
        <div class="stat-card">
            <h3>Total Wrestlers</h3>
            <p class="stat-value">{total_wrestlers:,}</p>
        </div>
        <div class="stat-card">
            <h3>Data Range</h3>
            <p class="stat-value">{min_date} to {max_date}</p>
        </div>
        <div class="stat-card">
            <h3>Last Updated</h3>
            <p class="stat-value">{date.today()}</p>
        </div>
    </div>
    <h2>Top 10 Wrestlers</h2>
    {top_10_html}
    """


def generate_html(
    skill_chart: go.Figure,
    rankings_chart: go.Figure,
    stats_html: str,
    summary_df: pd.DataFrame
) -> str:
    """Generate complete HTML dashboard."""

    skill_chart_html = skill_chart.to_html(full_html=False, include_plotlyjs=False)
    rankings_chart_html = rankings_chart.to_html(full_html=False, include_plotlyjs=False)

    # Create rankings table HTML
    rankings_table = ""
    if not summary_df.empty:
        rankings_table = summary_df.head(100).to_html(
            classes="data-table",
            index=False,
            float_format=lambda x: f"{x:.2f}"
        )

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>CardScraper - Wrestling Analytics</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
        * {{ box-sizing: border-box; }}
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
            margin: 0;
            padding: 20px;
            background: #f5f5f5;
            color: #333;
        }}
        .container {{ max-width: 1400px; margin: 0 auto; }}
        h1 {{ color: #1a1a2e; border-bottom: 3px solid #e94560; padding-bottom: 10px; }}
        h2 {{ color: #1a1a2e; margin-top: 40px; }}
        .nav {{
            display: flex;
            gap: 10px;
            margin-bottom: 20px;
            flex-wrap: wrap;
        }}
        .nav button {{
            padding: 10px 20px;
            border: none;
            background: #1a1a2e;
            color: white;
            cursor: pointer;
            border-radius: 5px;
            font-size: 14px;
        }}
        .nav button:hover {{ background: #e94560; }}
        .nav button.active {{ background: #e94560; }}
        .tab-content {{ display: none; background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }}
        .tab-content.active {{ display: block; }}
        .stats-grid {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }}
        .stat-card {{
            background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);
            color: white;
            padding: 20px;
            border-radius: 10px;
            text-align: center;
        }}
        .stat-card h3 {{ margin: 0 0 10px 0; font-size: 14px; opacity: 0.8; }}
        .stat-value {{ font-size: 24px; font-weight: bold; margin: 0; }}
        .stats-table, .data-table {{
            width: 100%;
            border-collapse: collapse;
            margin-top: 20px;
        }}
        .stats-table th, .stats-table td,
        .data-table th, .data-table td {{
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #ddd;
        }}
        .stats-table th, .data-table th {{
            background: #1a1a2e;
            color: white;
        }}
        .stats-table tr:hover, .data-table tr:hover {{ background: #f5f5f5; }}
        .about {{
            background: #fff;
            padding: 20px;
            border-radius: 10px;
            margin-top: 20px;
        }}
        .about h3 {{ color: #e94560; }}
        footer {{
            text-align: center;
            padding: 20px;
            color: #666;
            margin-top: 40px;
        }}
    </style>
</head>
<body>
    <div class="container">
        <h1>CardScraper Wrestling Analytics</h1>

        <div class="nav">
            <button class="active" onclick="showTab('history')">Skill History</button>
            <button onclick="showTab('rankings')">Current Rankings</button>
            <button onclick="showTab('stats')">Statistics</button>
            <button onclick="showTab('data')">Full Data</button>
            <button onclick="showTab('about')">About</button>
        </div>

        <div id="history" class="tab-content active">
            <h2>Wrestler Skill Evolution</h2>
            <p>Interactive chart showing how wrestler ratings have changed over time. Hover for details, zoom and pan to explore.</p>
            {skill_chart_html}
        </div>

        <div id="rankings" class="tab-content">
            <h2>Current Rankings</h2>
            <p>Current wrestler ratings with 95% confidence intervals, grouped by tier.</p>
            {rankings_chart_html}
        </div>

        <div id="stats" class="tab-content">
            <h2>Statistics</h2>
            {stats_html}
        </div>

        <div id="data" class="tab-content">
            <h2>Full Rankings Data</h2>
            <p>Top 100 wrestlers by current rating.</p>
            {rankings_table}
        </div>

        <div id="about" class="tab-content">
            <div class="about">
                <h3>About CardScraper</h3>
                <p>CardScraper analyzes WWE match data using <strong>TrueSkill</strong>, a Bayesian skill rating system developed by Microsoft Research.</p>

                <h3>How It Works</h3>
                <ul>
                    <li><strong>μ (mu)</strong>: The estimated skill level. Higher values indicate stronger booking/performance.</li>
                    <li><strong>σ (sigma)</strong>: Uncertainty in the estimate. Lower values mean more confident ratings.</li>
                    <li>New wrestlers start at μ=25, σ≈8.3</li>
                    <li>Ratings update after each match based on win/loss and opponent strength</li>
                </ul>

                <h3>Data Source</h3>
                <p>Match data is scraped from <a href="https://www.cagematch.net">Cagematch.net</a>. Updated daily via GitHub Actions.</p>

                <h3>Source Code</h3>
                <p><a href="https://github.com/iandoriath/cardscraper">View on GitHub</a></p>
            </div>
        </div>

        <footer>
            <p>Generated on {datetime.now().strftime("%Y-%m-%d %H:%M")} | Data from Cagematch.net</p>
        </footer>
    </div>

    <script>
        function showTab(tabId) {{
            document.querySelectorAll('.tab-content').forEach(t => t.classList.remove('active'));
            document.querySelectorAll('.nav button').forEach(b => b.classList.remove('active'));
            document.getElementById(tabId).classList.add('active');
            event.target.classList.add('active');

            // Trigger plotly resize for proper rendering
            window.dispatchEvent(new Event('resize'));
        }}
    </script>
</body>
</html>"""

    return html


def main():
    print("=== Generating Dashboard ===")

    # Load data
    events = load_json(DATA_DIR / "events.json") or []
    wrestlers = load_json(DATA_DIR / "wrestlers.json") or {}
    skill_history = load_json(DATA_DIR / "skill_history.json") or []

    # Load summary stats
    summary_path = DATA_DIR / "wrestler_summary_stats.csv"
    if summary_path.exists():
        summary_df = pd.read_csv(summary_path)
    else:
        summary_df = pd.DataFrame()

    print(f"Loaded: {len(events)} events, {len(wrestlers)} wrestlers, {len(skill_history)} history snapshots")

    # Generate charts
    skill_chart = create_skill_history_chart(skill_history, wrestlers)
    rankings_chart = create_rankings_chart(summary_df)
    stats_html = create_stats_section(events, wrestlers, summary_df)

    # Generate HTML
    html = generate_html(skill_chart, rankings_chart, stats_html, summary_df)

    # Save
    with open(OUTPUT_FILE, "w", encoding="utf-8") as f:
        f.write(html)

    print(f"Dashboard saved to {OUTPUT_FILE}")
    print("=== Dashboard generation complete ===")


if __name__ == "__main__":
    main()
