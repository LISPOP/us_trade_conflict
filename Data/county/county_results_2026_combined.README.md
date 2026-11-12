# 2026 county results, combined (unofficial until certification)

File: `county_results_2026_combined.csv`, 3,121 rows: 3,104 counties plus 17
Connecticut rows (see Known limits).
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
5. The House figure is blank in a county when the aggregator flags it as
   partial (part of the county is in a district one party did not contest),
   or when its House votes are under 80% of the votes in its Senate or
   governor race. This removes 21 counties, for example Sauk County,
   Wisconsin and Childress County, Texas.

## Known limits

- Official figures are used for Senate or governor in 15 states only (just one
  or two counties each in Kansas, Michigan and New Jersey). House
  figures come from the aggregator alone.
- Aggregator figures outside those 15 states are unchecked against state sites.
- County House totals add up every district that crosses the county. Rule 5
  removes the counties known to be distorted by an uncontested district, but
  it relies on the aggregator's flag and may miss some.
- California was still counting on 9 Nov (2026 votes are about 60% of 2024),
  so its figures are incomplete.
- Connecticut has 17 rows: the eight old counties, which have 2026 results
  but no 2024 baseline, and nine planning regions, which have the 2024
  baseline but no 2026 results. Neither set can be used for a 2024-2026 change.
