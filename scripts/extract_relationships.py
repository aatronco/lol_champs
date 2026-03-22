#!/usr/bin/env python3
"""Extract champion relationships from DDragon lore text using Claude API."""

import json
import csv
import os
import time
import sys
import requests
import anthropic

DDRAGON_BASE = "https://ddragon.leagueoflegends.com"
TOOL_NAME = "extract_relationships"

TOOL_SCHEMA = {
    "name": TOOL_NAME,
    "description": "Extract champion relationships from lore text.",
    "input_schema": {
        "type": "object",
        "properties": {
            "relationships": {
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "from":         {"type": "string"},
                        "to":           {"type": "string"},
                        "relationship": {"type": "string", "enum": ["Friendly", "Antagonistic", "Related", "Romantic", "Other"]},
                        "confidence":   {"type": "number", "minimum": 0, "maximum": 1},
                        "lore_excerpt": {"type": "string"},
                    },
                    "required": ["from", "to", "relationship", "confidence", "lore_excerpt"],
                },
            }
        },
        "required": ["relationships"],
    },
}


def fetch_lore(version, ddragon_key):
    url = f"{DDRAGON_BASE}/cdn/{version}/data/en_US/champion/{ddragon_key}.json"
    r = requests.get(url, timeout=10)
    r.raise_for_status()
    data = r.json()
    return data["data"][ddragon_key]["lore"]


def load_legacy_pairs(links_csv_path):
    """Returns set of (source_lower, target_lower) tuples, bidirectional."""
    pairs = set()
    with open(links_csv_path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            s = row["from"].strip().lower()
            t = row["to"].strip().lower()
            pairs.add((s, t))
            pairs.add((t, s))
    return pairs


def call_claude(client, champion_name, lore_text, max_retries=4):
    """Call Claude API, return list of relationship dicts. Retries on rate limit."""
    delays = [1, 2, 4, 8]
    for attempt in range(max_retries):
        try:
            response = client.messages.create(
                model="claude-3-5-sonnet-20241022",
                max_tokens=1024,
                system="You are a League of Legends lore expert. Extract champion relationships from lore text. Only include relationships explicitly mentioned in the text.",
                messages=[{
                    "role": "user",
                    "content": f"Champion: {champion_name}\nLore: {lore_text}\nExtract all relationships mentioned."
                }],
                tools=[TOOL_SCHEMA],
                tool_choice={"type": "tool", "name": TOOL_NAME},
            )
            for block in response.content:
                if block.type == "tool_use":
                    return block.input.get("relationships", [])
            return []
        except anthropic.RateLimitError:
            if attempt < max_retries - 1:
                time.sleep(delays[attempt])
            else:
                print(f"  Rate limit exceeded for {champion_name}, skipping.", file=sys.stderr)
                return []
        except Exception as e:
            print(f"  Error processing {champion_name}: {e}", file=sys.stderr)
            return []


def split_by_confidence(relationships, legacy_pairs, threshold=0.7):
    """Split into high/low confidence, excluding legacy duplicates. Returns (high, low)."""
    high, low = [], []
    for rel in relationships:
        s = rel["from"].lower()
        t = rel["to"].lower()
        if (s, t) in legacy_pairs or (t, s) in legacy_pairs:
            continue  # legacy wins
        if rel["confidence"] >= threshold:
            high.append(rel)
        else:
            low.append(rel)
    return high, low


def to_link(rel):
    """Convert extracted relationship dict to links.json format."""
    return {"source": rel["from"], "target": rel["to"], "type": rel["relationship"]}


def main():
    api_key = os.environ.get("ANTHROPIC_API_KEY")
    if not api_key:
        print("ERROR: Set ANTHROPIC_API_KEY environment variable.", file=sys.stderr)
        sys.exit(1)

    client = anthropic.Anthropic(api_key=api_key)

    r = requests.get(f"{DDRAGON_BASE}/api/versions.json", timeout=10)
    r.raise_for_status()
    version = r.json()[0]

    r = requests.get(f"{DDRAGON_BASE}/cdn/{version}/data/en_US/champion.json", timeout=10)
    r.raise_for_status()
    champ_data = r.json()

    legacy_pairs = load_legacy_pairs("links.csv")

    links_new = []
    needs_review = []

    champions = list(champ_data["data"].items())
    for i, (key, champ) in enumerate(champions):
        name = champ["name"]
        print(f"[{i+1}/{len(champions)}] Processing {name}...")
        try:
            lore = fetch_lore(version, key)
        except Exception as e:
            print(f"  Could not fetch lore: {e}", file=sys.stderr)
            continue

        rels = call_claude(client, name, lore)
        high, low = split_by_confidence(rels, legacy_pairs)
        links_new.extend(to_link(rel) for rel in high)
        needs_review.extend(low)

    os.makedirs("data", exist_ok=True)

    with open("data/links_new.json", "w", encoding="utf-8") as f:
        json.dump(links_new, f, indent=2, ensure_ascii=False)

    with open("data/needs_review.csv", "w", newline="", encoding="utf-8") as f:
        fields = ["from", "to", "relationship", "confidence", "lore_excerpt"]
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(needs_review)

    print(f"\nDone. {len(links_new)} high-confidence, {len(needs_review)} need review.")
    print("Next: review data/needs_review.csv, then run merge_reviewed.py")


if __name__ == "__main__":
    main()
