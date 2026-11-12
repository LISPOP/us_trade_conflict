# 2026 county results from official state feeds

Source: per-state CSVs in https://github.com/pushbuild/us-2026-election-results (data/*.csv), each read from a state election office feed. Snapshot 6 Nov 2026 (see source_fetched_utc). Unofficial until certification.

Columns: state, county, race (senate/governor), contest (name as published), r_votes, d_votes, r_share_2p (Republican share of R+D votes, 0-100), source_fetched_utc.

Party mapping: any party starting with R = Republican; starting with D (including DFL) = Democratic. Other parties are excluded from the share.

Coverage: states with county rows in the official feed only. Partial or check before use:
- Alaska Senate: rows are election districts, not counties (81 of 121 reported); top-4 primary, so R/D shares are not a clean two-party split.
- Kansas, Michigan, New Jersey: 1-2 counties only.
- Colorado Senate: six counties differ from the aggregator by more than 1 point (e.g. Archuleta, Montrose); cause not checked.
- Rhode Island excluded: the feed reports 39 towns, not counties.

Not covered (no county rows in feed): AL, AZ, CA, CT, DE, IL, IN, LA, MA, MO, NE, NM, NV, NY, OK, PA Senate, SC, SD, TX Senate, UT, VT, WA, WI, WY, and most Senate races in states with a Senate contest but no county rows.

Comparison with the aggregator (Electoral-Path-2026, counties.json): Senate median gap 0.02 points; governor median gap 0.02 points. Larger gaps are mostly county-name mismatches, e.g. North Carolina Martin and Macon, and Iowa Madison.
