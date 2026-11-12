# County-level data sources: 2026 midterm vote and trade exposure

Compiled 2026-11-11; links were checked that day.

## Downloaded to `Data/county/` (2026-11-11)

| Folder | File | Notes |
|---|---|---|
| `results_2026_civicapi/` | `counties.json` (1.3 MB) | From `jcalepos146/Electoral-Path-2026`, commit `9a5d32a` of 9 Nov 2026. Unofficial counts; re-download after certification |
| `adhm_2020/` | `ADHM-PoliticalPolarization-FileArchive.zip` (3.6 MB) | Not extracted. Holds Stata data and do-files, including `dta/president_2000_2016.dta` and `dta/house_2002_2016.dta`, plus `Readme.pdf` |
| `bbc_2019/` | `Replication_BBC_15Oct19.zip` (9.1 MB) | Not extracted. Holds one Stata data file, `workfile_12Jun19.dta` (18 MB), and the do-files for each table |

Checks run 2026-11-11:

- `president_2000_2016.dta` is one row per county (3,107 rows, `cty_fips`),
  with Republican two-party shares for 1992-2016 (`shnr_pres*`), the China
  import exposure measure (`d_imp_usch_pd_0008`) and its instrument
  (`d_imp_otch_lag_pd_0008`).
- `workfile_12Jun19.dta` is one row per county (3,197 rows, `fips`, 1,459
  columns). It has separate 2018 retaliatory tariff shocks by partner,
  including **Canada** (`cty_can3_TS*`), China, Mexico and the EU.
- `counties.json` Senate figures were compared with official state feeds (the
  pushbuild CSVs of 6 Nov) for GA, NC, IA, OH and MN. County vote totals are
  identical in 484 of 530 counties and the Republican share is within 0.5
  points in 523. The larger gaps are a few counties still counting, and four
  North Carolina counties where the pushbuild file appears to have totals
  shifted between neighbouring county names.
- **`sen26_r`, `gov26_r` and `house26_r` are shares of all votes, not
  two-party shares.** Compute two-party as `r / (r + d)` to match the rest of
  the project.

## 1. The paper linking county trade exposure to Trump support

**Autor, Dorn, Hanson and Majlesi (2020), "Importing Political Polarization?
The Electoral Consequences of Rising Trade Exposure", AER 110(10): 3139-3183.**
Counties more exposed to Chinese import competition shifted toward the
Republican presidential candidate, 2000-2016.

- Companion note on Trump specifically: "A Note on the Effect of Rising Trade
  Exposure on the 2016 Presidential Election" (rev. March 2017). It regresses
  the 2000-2016 change in county Republican two-party share on import exposure.
- Paper: https://www.aeaweb.org/doi/10.1257/aer.20170011
- Replication package: https://www.openicpsr.org/openicpsr/project/119547
  (needs a free ICPSR login)
- Same archive, no login: https://www.ddorn.net/data.htm, item [P9]
  `ADHM-PoliticalPolarization-FileArchive.zip` (3.6 MB). The same page has
  county-to-commuting-zone and county-to-congressional-district crosswalks.
- Exposure there is defined on commuting zones and mapped to counties.

**Closest design to ours: Blanchard, Bown and Chor, "Did Trump's Trade War
Impact the 2018 Election?"** (NBER WP 26434; later published, 2024). County
exposure to *retaliatory* tariffs and the 2018 *midterm* House vote. Republican
House candidates lost support in counties more exposed to retaliation.

- Paper: https://www.nber.org/papers/w26434
- Replication files: https://data.nber.org/data-appendix/w26434/
  `Replication_BBC_15Oct19.zip` (8.7 MB, 2019 working-paper version)
- Method: tariff list (HS) -> industry (NAICS) -> county, weighted by County
  Business Patterns employment. County House vote from Dave Leip's Atlas.

**Second candidate for "the paper": Lake and Nie (2023), "The 2020 US
Presidential Election and Trump's Wars on Trade and Health Insurance",
European Journal of Political Economy.** County exposure to the 2018-19 trade
war and Trump's 2020 vote. Per its abstract, US tariff protection raised
Trump's support and foreign retaliation lowered it. The paper itself has not
been read.

Downloaded to `Data/county/lake_nie_2023/` on 2026-11-11: `estimationDataset.dta`
(3 MB) and `regressions_EJPE.do`. The package's other files (Covid mobility
data, shapefiles, figures) were skipped. Checks on the dataset:

- One row per county: 3,112 rows, no duplicate FIPS, 49 states plus DC
  (Alaska is absent). 236 columns, no variable labels.
- 2020 vote totals are 81.13 million Democratic and 74.03 million Republican,
  which equals the official national totals minus Alaska.
- County 2020 margins match an independent source (the 2020 margins in
  `counties.json`, from the tonmcg dataset) within 1 point in 99.9% of counties.
- Tariff exposure: `TS3_US` (US tariffs) and `TS3_R` (retaliation), with
  China-only versions `chn_TS3_US` and `chn_TS3_R`, and farm subsidies
  `agSubs_mfp_pw`. No missing values. `TS3_R` correlates 0.77-0.79 with the
  Blanchard-Bown-Chor retaliation shock, and `TS3_US` 0.41-0.78 with their US
  tariff shocks, so the two papers measure similar but not identical things.
- No Canada-specific shock here; only the Blanchard-Bown-Chor file has one.

- Paper: https://doi.org/10.1016/j.ejpoleco.2022.102338
- Free working-paper version (CESifo WP 8856): https://www.cesifo.org/DocDL/cesifo1_wp8856.pdf
- Replication package (Stata, CC BY 4.0): https://data.mendeley.com/datasets/33jbvtypvv/3

## 2. County-level trade exposure

No ready-made county measure of exposure to **Canada's** 2025 counter-tariffs
was found. Options, best first:

| Source | What it is | Link |
|---|---|---|
| Brookings (Maxim, Muro, Methkupally, Feb 2025) | County employment in industries hit by **China's** 2025 retaliation, xlsx (192 KB) | https://www.brookings.edu/articles/chinas-retaliatory-tariffs-will-hurt-trump-voting-counties-most/ |
| Brookings (Parilla, Bouchet, Oct 2018) | Jobs exposed to Chinese, EU and NAFTA retaliation, 2018 round, xlsx (212 KB) | https://www.brookings.edu/articles/which-us-communities-are-most-affected-by-chinese-eu-and-nafta-retaliatory-tariffs/ |
| Blanchard-Bown-Chor replication | County retaliatory and US tariff shocks, 2018 round | see above |
| Autor et al. archive | County/commuting-zone China import exposure, 1990s-2000s | see above |
| Washington Post map, 10 Mar 2025 | County jobs exposed to **Canadian and Chinese** counter-tariffs, built on Brookings data. Underlying file not located | https://www.washingtonpost.com/business/2025/03/10/canada-china-tariffs-american-jobs/ |
| Richmond Fed WP 26-08 and Economic Brief 25-12 | County effective **US import** tariff rates for 2025 (protection side, not retaliation) | https://www.richmondfed.org/publications/research/working_papers/2026/wp_26-08 |

**Caveat (2026-11-11):** the working paper draft dates Canada's counter-tariffs
to 7 September 2026. The surtax orders listed in step 1 below are the 2025
ones; the September 2026 list has not been located yet and is probably the
one to use.

**Building our own Canada measure** (the Blanchard-Bown-Chor recipe):

1. Canada's surtax lists by tariff item: SOR/2025-66 (other goods), SOR/2025-95
   (steel and aluminum), plus the autos order, on https://laws-lois.justice.gc.ca.
   Note the 29 Aug 2025 amendment cut coverage from over 1,800 tariff items to
   313 as of 1 Sep 2025, so exposure differs before and after that date.
2. HS-to-NAICS concordance (Census, or Dorn's crosswalks).
3. County employment by NAICS: Census County Business Patterns or BLS QCEW.

The Census Bureau does not publish exports by county and destination; the
ITA metro export series has country detail only for the top 50 metros.

## 3. County-level 2026 midterm results

County results do exist, but all are unofficial until states certify (late
November into December). There is no single national race, so "the county
vote" means Senate, governor, or all House races combined.

| Source | Coverage | Link |
|---|---|---|
| civicAPI, as packaged in `jcalepos146/Electoral-Path-2026` | `data/counties.json`: 3,235 county rows with FIPS. 2026 two-party shares for House (3,132 counties), Senate (2,465), governor (2,173), plus 2024 and 2020 presidential margins. Counts as of 9 Nov. Gaps: Alaska, Connecticut, DC | https://github.com/jcalepos146/Electoral-Path-2026 |
| civicAPI directly | Free, no key, attribution required. Does not state its own sources | https://civicapi.org |
| `pushbuild/us-2026-election-results` | One CSV per state straight from state election offices, CC0, as of 6 Nov. 41 states listed; no county rows for AL, CA, DE, IL, MA, NE, NM, NV, NY, OK, SD, VT, WY and only a few counties for KS, MI, NJ, PA, WI. No FIPS column | https://github.com/pushbuild/us-2026-election-results |
| MIT Election Data and Science Lab | Nothing for 2026 yet. Official precinct returns usually arrive months later (its 2024 repo was still being updated in May 2026) | https://github.com/MEDSL |
| Dave Leip's Atlas | Paid; what Blanchard-Bown-Chor used. No 2026 file listed yet | https://uselectionatlas.org |
| `tonmcg/US_County_Level_Election_Results_08-24` | 2008-2024 presidential by county, for the baseline | https://github.com/tonmcg/US_County_Level_Election_Results_08-24 |

Caveats for the House measure: counties split across districts are summed, and
the Electoral-Path file drops counties in districts one party did not contest.
