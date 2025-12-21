"""
TrueSkill rating model for wrestler skill estimation.
Uses the trueskill package for Bayesian skill estimation.
"""

import json
from pathlib import Path
from typing import Optional
import trueskill


# Configure TrueSkill environment
# mu=25, sigma=25/3 are the defaults
env = trueskill.TrueSkill(
    mu=25.0,
    sigma=25.0 / 3,
    beta=25.0 / 6,
    tau=25.0 / 300,
    draw_probability=0.0
)
env.make_as_global()


class PlayerRatings:
    """Manages TrueSkill ratings for all players."""

    def __init__(self, file_path: Optional[str] = None):
        self.players: dict[str, trueskill.Rating] = {}
        self.file_path = file_path
        if file_path and Path(file_path).exists():
            self.load(file_path)

    def get_or_create(self, player_id: str) -> trueskill.Rating:
        """Get a player's rating, creating a new one if they don't exist."""
        if player_id not in self.players:
            self.players[player_id] = trueskill.Rating()
        return self.players[player_id]

    def update_match(self, winner_ids: list[str], loser_ids: list[str]) -> None:
        """Update ratings based on a match result."""
        if not winner_ids or not loser_ids:
            return

        # Get current ratings
        winners = [self.get_or_create(pid) for pid in winner_ids]
        losers = [self.get_or_create(pid) for pid in loser_ids]

        # Update ratings (winners ranked 0, losers ranked 1)
        new_winners, new_losers = trueskill.rate([winners, losers], ranks=[0, 1])

        # Store updated ratings
        for pid, rating in zip(winner_ids, new_winners):
            self.players[pid] = rating
        for pid, rating in zip(loser_ids, new_losers):
            self.players[pid] = rating

    def update_draw(self, team1_ids: list[str], team2_ids: list[str]) -> None:
        """Update ratings based on a draw."""
        if not team1_ids or not team2_ids:
            return

        team1 = [self.get_or_create(pid) for pid in team1_ids]
        team2 = [self.get_or_create(pid) for pid in team2_ids]

        new_team1, new_team2 = trueskill.rate([team1, team2], ranks=[0, 0])

        for pid, rating in zip(team1_ids, new_team1):
            self.players[pid] = rating
        for pid, rating in zip(team2_ids, new_team2):
            self.players[pid] = rating

    def predict_match(self, player1_id: str, player2_id: str) -> dict:
        """Predict the outcome of a 1v1 match."""
        r1 = self.get_or_create(player1_id)
        r2 = self.get_or_create(player2_id)

        delta_mu = r1.mu - r2.mu
        sum_sigma = (r1.sigma ** 2 + r2.sigma ** 2 + 2 * (env.beta ** 2)) ** 0.5
        win_prob = trueskill.CUMULATIVE_DISTRIBUTION_FUNCTION(delta_mu / sum_sigma)

        return {
            "p1_win_prob": win_prob,
            "p2_win_prob": 1 - win_prob,
            "expected_score_diff": delta_mu
        }

    def get_rankings(self) -> list[dict]:
        """Get all players ranked by skill."""
        rankings = []
        for pid, rating in self.players.items():
            rankings.append({
                "wrestler_id": pid,
                "mu": rating.mu,
                "sigma": rating.sigma,
                "conservative_rating": rating.mu - 3 * rating.sigma
            })
        return sorted(rankings, key=lambda x: x["mu"], reverse=True)

    def save(self, file_path: Optional[str] = None) -> None:
        """Save player ratings to a JSON file."""
        path = file_path or self.file_path
        if not path:
            raise ValueError("No file path specified")

        data = {
            pid: {"mu": r.mu, "sigma": r.sigma}
            for pid, r in self.players.items()
        }
        with open(path, "w") as f:
            json.dump(data, f, indent=2)

    def load(self, file_path: Optional[str] = None) -> None:
        """Load player ratings from a JSON file."""
        path = file_path or self.file_path
        if not path:
            raise ValueError("No file path specified")

        with open(path, "r") as f:
            data = json.load(f)

        self.players = {
            pid: trueskill.Rating(mu=r["mu"], sigma=r["sigma"])
            for pid, r in data.items()
        }

    def to_dataframe(self) -> "pd.DataFrame":
        """Convert ratings to a pandas DataFrame."""
        import pandas as pd
        return pd.DataFrame(self.get_rankings())
