import json
import csv
import pytest
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'scripts'))
import merge_reviewed as mr


def write_csv(path, rows):
    with open(path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["from", "to", "relationship", "confidence", "lore_excerpt"])
        writer.writeheader()
        writer.writerows(rows)


def test_merge_deduplicates_legacy_wins(tmp_path):
    legacy = [{"source": "Ashe", "target": "Braum", "type": "Friendly"}]
    links_new = [{"source": "Ashe", "target": "Braum", "type": "Antagonistic"}]  # conflict
    reviewed = []

    result = mr.merge(legacy, links_new, reviewed)
    ashe_braum = [r for r in result if {r["source"], r["target"]} == {"Ashe", "Braum"}]
    assert len(ashe_braum) == 1
    assert ashe_braum[0]["type"] == "Friendly"  # legacy wins


def test_merge_includes_new_relationships(tmp_path):
    legacy = [{"source": "Ashe", "target": "Braum", "type": "Friendly"}]
    links_new = [{"source": "Jinx", "target": "Vi", "type": "Antagonistic"}]
    reviewed = [{"from": "Darius", "to": "Garen", "relationship": "Antagonistic", "confidence": "0.5", "lore_excerpt": "x"}]

    result = mr.merge(legacy, links_new, reviewed)
    ids = [(r["source"], r["target"]) for r in result]
    assert ("Jinx", "Vi") in ids
    assert ("Darius", "Garen") in ids


def test_merge_case_insensitive_dedup():
    legacy = [{"source": "ashe", "target": "braum", "type": "Friendly"}]
    links_new = [{"source": "Ashe", "target": "Braum", "type": "Antagonistic"}]

    result = mr.merge(legacy, links_new, [])
    matches = [r for r in result if r["source"].lower() == "ashe"]
    assert len(matches) == 1
    assert matches[0]["type"] == "Friendly"


def test_links_new_wins_over_reviewed():
    legacy = []
    links_new = [{"source": "A", "target": "B", "type": "Friendly"}]
    reviewed = [{"from": "A", "to": "B", "relationship": "Antagonistic", "confidence": "0.6", "lore_excerpt": ""}]

    result = mr.merge(legacy, links_new, reviewed)
    matches = [r for r in result if {r["source"], r["target"]} == {"A", "B"}]
    assert len(matches) == 1
    assert matches[0]["type"] == "Friendly"  # links_new wins over reviewed
