#!/usr/bin/env python3
"""Fetch current LoL champions and their regions, write data/nodes.json."""

import json
import csv
import os
import sys
import requests

DDRAGON_BASE = "https://ddragon.leagueoflegends.com"
UNIVERSE_URL = "https://universe-meeps.leagueoflegends.com/v1/en_us/champion-browse/index.json"

SLUG_TO_REGION = {
    "freljord": "Freljord",
    "noxus": "Noxus",
    "demacia": "Demacia",
    "ionia": "Ionia",
    "piltover": "Piltover",
    "zaun": "Zaun",
    "bilgewater": "Bilgewater",
    "shadow-isles": "Shadow Isles",
    "shurima": "Shurima",
    "targon": "Mt. Targon",
    "void": "Void",
    "bandle-city": "Bandle City",
    "ixtal": "Ixtal",
    "runeterra": "Independent",
}


def fetch_versions():
    r = requests.get(f"{DDRAGON_BASE}/api/versions.json", timeout=10)
    r.raise_for_status()
    return r.json()


def fetch_champions(version):
    url = f"{DDRAGON_BASE}/cdn/{version}/data/en_US/champion.json"
    r = requests.get(url, timeout=10)
    r.raise_for_status()
    return r.json()


def fetch_universe():
    r = requests.get(UNIVERSE_URL, timeout=10)
    r.raise_for_status()
    return r.json()


def load_legacy_regions(csv_path):
    """Returns dict: {display_name: region_string} from nodes.csv."""
    regions = {}
    with open(csv_path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            regions[row["id"].strip()] = row["Allegiance"].strip()
    return regions


def region_from_universe(display_name, ddragon_key, universe_data):
    """Look up region via Universe API data. Returns region string or 'Unknown'."""
    slug_candidate = ddragon_key.lower().replace("'", "").replace(".", "").replace(" ", "-")
    for champ in universe_data.get("champion-list", []):
        champ_slug = champ.get("slug", "")
        if champ_slug == slug_candidate or champ_slug == display_name.lower().replace(" ", "-"):
            faction_slug = champ.get("associated-faction-slug", "")
            return SLUG_TO_REGION.get(faction_slug, "Unknown")
    return "Unknown"


def build_nodes(nodes_csv_path="nodes.csv"):
    versions = fetch_versions()
    version = versions[0]
    print(f"Using DDragon version: {version}")

    champ_data = fetch_champions(version)
    universe_data = fetch_universe()
    legacy_regions = load_legacy_regions(nodes_csv_path)

    nodes = []
    for key, champ in champ_data["data"].items():
        display_name = champ["name"]
        if display_name in legacy_regions:
            region = legacy_regions[display_name]
        else:
            region = region_from_universe(display_name, key, universe_data)
            if region == "Unknown":
                print(f"  WARNING: No region found for {display_name}", file=sys.stderr)
        nodes.append({"id": display_name, "region": region})

    return nodes


def main():
    nodes = build_nodes()
    output_path = "data/nodes.json"
    os.makedirs("data", exist_ok=True)
    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(nodes, f, indent=2, ensure_ascii=False)
    print(f"Wrote {len(nodes)} champions to {output_path}")


if __name__ == "__main__":
    main()
