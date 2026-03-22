#!/usr/bin/env python3
"""Merge links.csv + data/links_new.json + data/needs_review.csv → data/links.json."""

import json
import csv
import os


def load_legacy_csv(path):
    """Load links.csv (columns: from, to, relationship) → list of {source, target, type}."""
    links = []
    with open(path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            links.append({
                "source": row["from"].strip(),
                "target": row["to"].strip(),
                "type": row["relationship"].strip(),
            })
    return links


def load_links_new(path):
    """Load data/links_new.json → list of {source, target, type}. Returns [] if file missing."""
    if not os.path.exists(path):
        return []
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def load_needs_review(path):
    """Load data/needs_review.csv (human-edited) → list of row dicts. Returns [] if missing."""
    if not os.path.exists(path):
        return []
    rows = []
    with open(path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            rows.append(row)
    return rows


def normalize_reviewed(row):
    """Convert a needs_review.csv row to {source, target, type} format."""
    return {
        "source": row["from"].strip(),
        "target": row["to"].strip(),
        "type": row["relationship"].strip(),
    }


def pair_key(source, target):
    """Canonical key for a relationship pair — order-independent, case-insensitive."""
    return tuple(sorted([source.lower(), target.lower()]))


def merge(legacy, links_new, reviewed):
    """Merge three relationship sources. Priority: legacy > links_new > reviewed.

    Args:
        legacy: list of {source, target, type} from links.csv
        links_new: list of {source, target, type} from links_new.json
        reviewed: list of row dicts from needs_review.csv (with from/to/relationship keys)

    Returns:
        list of {source, target, type} with no duplicates
    """
    seen = {}  # pair_key → link dict; first occurrence wins

    for link in legacy:
        k = pair_key(link["source"], link["target"])
        if k not in seen:
            seen[k] = link

    for link in links_new:
        k = pair_key(link["source"], link["target"])
        if k not in seen:
            seen[k] = link

    for row in reviewed:
        link = normalize_reviewed(row)
        k = pair_key(link["source"], link["target"])
        if k not in seen:
            seen[k] = link

    return list(seen.values())


def main():
    print("Loading sources...")
    legacy   = load_legacy_csv("links.csv")
    new      = load_links_new("data/links_new.json")
    reviewed = load_needs_review("data/needs_review.csv")

    print(f"  Legacy: {len(legacy)}, New: {len(new)}, Reviewed: {len(reviewed)}")

    result = merge(legacy, new, reviewed)

    os.makedirs("data", exist_ok=True)
    with open("data/links.json", "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2, ensure_ascii=False)

    print(f"Wrote {len(result)} relationships to data/links.json")


if __name__ == "__main__":
    main()
