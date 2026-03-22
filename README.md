# lol_champs
Interactive League of Legends champion relationship explorer — GitHub Pages web app built with D3.js.

## Live site
https://aatronco.github.io/lol_champs/

## What it shows
- Force-directed graph of champion lore relationships (Friendly, Antagonistic, Romantic, Related)
- Node color = official region (Freljord, Noxus, Ionia, etc.)
- Node border color = algorithmically detected community (Louvain)
- Click communities in the sidebar to highlight them

## Updating data
```bash
# Update champion list (run when new champions are released)
python scripts/update_data.py

# Extract new relationships from lore text via Claude API
python scripts/extract_relationships.py
# → review data/needs_review.csv (delete rows to reject, fix relationship type if needed)

# Merge everything into data/links.json
python scripts/merge_reviewed.py

# Commit and push
git add data/nodes.json data/links.json
git commit -m "data: update champion relationships"
git push
```

## Legacy R analysis
The original R scripts are in `lol_champs.R` and `Depedencies.R`. The data files `nodes.csv` and `links.csv` are the original hand-curated dataset.

## Requirements (for data update scripts)
```bash
pip install requests anthropic
```
Set `ANTHROPIC_API_KEY` environment variable before running `extract_relationships.py`.
