# 2026 county results, combined (unofficial until certification)

File: county_results_2026_combined.csv (3,235 counties, one row per FIPS).

- senate_r2p / governor_r2p / house_r2p: Republican share of Republican plus Democratic votes, 0-100.
- *_source: "official" = state election office feed (pushbuild snapshot, 6 Nov 2026), see official_state_feeds_2026/README.md; "aggregator" = civicAPI copy in Electoral-Path-2026 (9 Nov 2026); blank = no result.
- Official rows used for Senate and governor only; Alaska official rows excluded (districts). House results come from the aggregator only.
- Aggregator rows are not yet checked against state websites, so the aggregator-sourced values still need that check before the analysis is final.
