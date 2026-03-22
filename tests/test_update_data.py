import json
import csv
import pytest
from unittest.mock import patch, MagicMock
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'scripts'))
import update_data


MOCK_VERSIONS = ["14.6.1", "14.5.1"]

MOCK_CHAMPIONS = {
    "data": {
        "Ashe": {"id": "Ashe", "name": "Ashe"},
        "DrMundo": {"id": "DrMundo", "name": "Dr. Mundo"},
        "Chogath": {"id": "Chogath", "name": "Cho'Gath"},
        "NewChamp": {"id": "NewChamp", "name": "New Champ"},
    }
}

MOCK_UNIVERSE = {
    "champion-list": [
        {"slug": "new-champ", "associated-faction-slug": "ionia"},
        {"slug": "ashe", "associated-faction-slug": "freljord"},
    ]
}

MOCK_NODES_CSV = [
    {"id": "Ashe", "Allegiance": "Freljord"},
    {"id": "Dr. Mundo", "Allegiance": "Zaun"},
    {"id": "Cho'Gath", "Allegiance": "Void"},
]


def test_slug_to_region_mapping():
    assert update_data.SLUG_TO_REGION["freljord"] == "Freljord"
    assert update_data.SLUG_TO_REGION["shadow-isles"] == "Shadow Isles"
    assert update_data.SLUG_TO_REGION.get("unknown-slug") is None


def test_region_from_csv_takes_priority(tmp_path):
    csv_path = tmp_path / "nodes.csv"
    with open(csv_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["id", "Allegiance"])
        writer.writeheader()
        writer.writerows(MOCK_NODES_CSV)

    legacy = update_data.load_legacy_regions(str(csv_path))
    assert legacy["Ashe"] == "Freljord"
    assert legacy["Dr. Mundo"] == "Zaun"
    assert legacy["Cho'Gath"] == "Void"


def test_new_champion_gets_region_from_universe():
    universe_data = MOCK_UNIVERSE
    region = update_data.region_from_universe("New Champ", "NewChamp", universe_data)
    assert region == "Ionia"


def test_unknown_champion_gets_unknown_region():
    region = update_data.region_from_universe("Ghost", "ghost", MOCK_UNIVERSE)
    assert region == "Unknown"


def test_build_nodes_output(tmp_path):
    csv_path = tmp_path / "nodes.csv"
    with open(csv_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["id", "Allegiance"])
        writer.writeheader()
        writer.writerows(MOCK_NODES_CSV)

    with patch("update_data.fetch_versions", return_value=MOCK_VERSIONS), \
         patch("update_data.fetch_champions", return_value=MOCK_CHAMPIONS), \
         patch("update_data.fetch_universe", return_value=MOCK_UNIVERSE):
        nodes = update_data.build_nodes(str(csv_path))

    ids = {n["id"] for n in nodes}
    assert "Ashe" in ids
    assert "Dr. Mundo" in ids
    assert "Cho'Gath" in ids
    assert "New Champ" in ids

    ashe = next(n for n in nodes if n["id"] == "Ashe")
    assert ashe["region"] == "Freljord"  # from CSV

    new_champ = next(n for n in nodes if n["id"] == "New Champ")
    assert new_champ["region"] == "Ionia"  # from Universe API
