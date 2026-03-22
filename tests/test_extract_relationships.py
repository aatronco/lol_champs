import json
import csv
import pytest
from unittest.mock import patch, MagicMock
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'scripts'))
import extract_relationships as er


MOCK_LORE = "Ashe and Braum are close allies. She opposes Lissandra's dark magic."

MOCK_CLAUDE_RESPONSE = {
    "relationships": [
        {"from": "Ashe", "to": "Braum", "relationship": "Friendly", "confidence": 0.95, "lore_excerpt": "close allies"},
        {"from": "Ashe", "to": "Lissandra", "relationship": "Antagonistic", "confidence": 0.85, "lore_excerpt": "opposes"},
        {"from": "Ashe", "to": "Unclear", "relationship": "Other", "confidence": 0.4, "lore_excerpt": "vague mention"},
    ]
}

LEGACY_LINKS = [
    {"source": "Ashe", "target": "Braum", "type": "Friendly"},  # duplicate
]


def test_split_by_confidence():
    rels = MOCK_CLAUDE_RESPONSE["relationships"]
    legacy_pairs = {("ashe", "braum"), ("braum", "ashe")}
    high, low = er.split_by_confidence(rels, legacy_pairs, threshold=0.7)
    # Ashe-Braum is in legacy → excluded from both
    assert len(high) == 1  # Ashe-Lissandra
    assert len(low) == 1   # Ashe-Unclear
    assert high[0]["to"] == "Lissandra"
    assert low[0]["to"] == "Unclear"


def test_load_legacy_pairs_from_csv(tmp_path):
    csv_path = tmp_path / "links.csv"
    with open(csv_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["from", "to", "relationship"])
        writer.writeheader()
        writer.writerow({"from": "Ashe", "to": "Braum", "relationship": "Friendly"})

    pairs = er.load_legacy_pairs(str(csv_path))
    assert ("ashe", "braum") in pairs
    assert ("braum", "ashe") in pairs  # bidirectional


def test_to_link_format():
    rel = {"from": "Ashe", "to": "Lissandra", "relationship": "Antagonistic", "confidence": 0.9, "lore_excerpt": "x"}
    link = er.to_link(rel)
    assert link == {"source": "Ashe", "target": "Lissandra", "type": "Antagonistic"}


def test_call_claude_returns_relationships():
    mock_client = MagicMock()
    mock_tool_result = MagicMock()
    mock_tool_result.content = [MagicMock(type="tool_use", input=MOCK_CLAUDE_RESPONSE)]
    mock_client.messages.create.return_value = mock_tool_result

    result = er.call_claude(mock_client, "Ashe", MOCK_LORE)
    assert len(result) == 3
    assert result[0]["from"] == "Ashe"
