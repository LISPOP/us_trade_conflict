# 2026 county results, combined (unofficial until certification)

File: `county_results_2026_combined.csv`, one row per county, 3,121 counties.
Built by `R_Scripts/3b_build_county_results_2026.R`; do not edit by hand.

For each race (senate, governor, house):

- `*_r2p`: Republican share of Republican plus Democratic votes, 0-100.
- `*_rmargin`: Republican minus Democratic share of all votes, in points.
- `*_source`: `official` (state election office feed, 6 Nov 2026),
  `aggregator` (civicAPI copy, 9 Nov 2026), `aggregator (official differs)`,
  or blank when there is no usable result.

Also `rmargin24`, `rmargin20` (presidential margins) and `votes24`.

## Cleaning rules

1. Alaska and DC are dropped: their rows are state house districts and wards,
   not counties.
2. A race is blank in a county when either major party has under 5% of the
   vote (uncontested or same-party contest).
3. A race is blank for a whole state when Republicans and Democrats together
   have under 90% of the state vote, because an independent or third candidate
   was a main contender: Senate in ID, MT, NE, SD; governor in ID, RI, TN, WY;
   House in WY.
4. The official figure is used only when it is within 1 point of the
   aggregator. Where they differ the aggregator is used and flagged. Known
   causes: the official file lists four North Carolina counties (Macon,
   Madison, Martin, McDowell) under each other's names, and some counties
   were still counting on 6 Nov (for example Madison County, Iowa).

## Known limits

- Official figures are used for Senate or governor in 15 states only (just one
  or two counties each in Kansas, Michigan and New Jersey). House
  figures come from the aggregator alone.
- Aggregator figures outside those 15 states are unchecked against state sites.
- County House totals add up every district that crosses the county, so a
  county partly in an uncontested or same-party district can look lopsided.
- California was still counting on 9 Nov (2026 votes are about 60% of 2024),
  so its figures are incomplete.
- Connecticut rows are the eight old counties; they have no 2024 baseline.
