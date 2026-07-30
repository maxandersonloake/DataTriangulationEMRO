# Pakistan IDSR Dashboard — WHO EMRO

## 1. Where these files go

Posit Connect Cloud deploys straight from a GitHub/GitLab repo and expects
`app.R` at the path you point it to. The cleanest setup is to put `app.R`
**at the root of your existing `DataTriangulationEMRO` RProject**, alongside
your existing `Data/` folder:

```
DataTriangulationEMRO/
├── DataTriangulationEMRO.Rproj
├── app.R                      <- from this delivery
├── www/
│   ├── who_brand.css          <- from this delivery
│   └── logo.png               <- YOU add the official WHO logo here
└── Data/
    ├── PAK_IDSR_Data.csv
    ├── PAK_IDSR_Compliance.csv
    └── gadm/                  <- auto-created cache, don't need to add
```

**Note:** this is a change from your original app, where `app.R` lived in
`Dashboard/who_dashboard/` but read data from a top-level `Data/` folder
(those two locations didn't actually line up). Posit Connect Cloud needs
one self-contained folder, so I've moved everything to the project root.
If you'd rather keep a `Dashboard/who_dashboard/` subfolder, that's fine —
just make sure `Data/` sits at the same level as `app.R`, or update
`DATA_PATH`/`COMPLIANCE_PATH` at the top of `app.R` accordingly.

You still need to add the **official WHO logo PNG** at `www/logo.png` —
per WHO brand guidance this isn't something I can fabricate or bundle.

## 2. R packages required

```r
install.packages(c(
  "shiny", "dplyr", "tidyr", "readr", "ggplot2", "plotly", "DT",
  "scales", "base64enc", "leaflet", "sf", "geodata"
))
```

`sf` and `leaflet` need system geospatial libraries (GDAL/GEOS/PROJ).
Posit Connect Cloud's standard R build image includes these, but if you
test locally on Windows, the easiest route is installing from CRAN
binaries (`install.packages("sf")` pulls prebuilt binaries on Windows —
no separate GDAL install needed).

`geodata` is optional: it's used only to download Pakistan's admin-1
boundaries (province/territory outlines) for the map, the first time the
app runs, and caches them in `Data/gadm/`. If it's not installed, or if
the app can't reach the internet at runtime, the map tab shows a friendly
"map unavailable" message instead of crashing — everything else in the
dashboard keeps working.

Before deploying, run `renv::init()` then `renv::snapshot()` in the
project so Posit Connect Cloud can reproduce your package versions (see
their docs: https://docs.posit.co/connect-cloud/how-to/r/renv.html).

## 3. Deploying to Posit Connect Cloud

1. Push this project (including `app.R`, `www/`, and `Data/`) to a GitHub
   repository.
2. In Posit Connect Cloud, choose **Publish > Shiny app**, connect your
   GitHub account, and select the repo/branch.
3. Point it at `app.R` in the repo root.
4. Connect Cloud will read your `renv.lock` to install the right package
   versions automatically.

## 4. Design decisions / assumptions made while building this

- **Map boundaries**: rather than bundling a boundary file (which I
  couldn't verify against your exact region naming without your input),
  the app downloads official GADM Pakistan admin-1 boundaries at first
  run and caches them locally. Region names are matched to your
  abbreviations (KP, Sindh, Balochistan, GB, ICT, AJK). **Punjab** isn't
  present in your current data, so it will show as grey ("no data") on
  the map — this is expected given the CSVs you shared only include six
  regions plus a national total.
- **Projected total cases** = reported cases ÷ (compliance % / 100),
  computed per region/week from `PAK_IDSR_Compliance.csv`. Where
  compliance is 0% or missing, the projection is left blank (undefined)
  rather than showing `Inf`.
- **Map metric**: colours the % change vs the trailing 3-week average
  using whichever case-type (reported/projected) is toggled on the Data
  visualisation tab, for the most recent available week per region.
- **Year colours**: current year is always solid WHO Navy; earlier years
  cycle through a set of distinct WHO secondary colours (blue, orange,
  green, magenta, yellow, purple) rather than shades of one colour, so
  years are easier to tell apart at a glance.
- **Weekly table colour scale**: diverging red (increase) / green
  (decrease), with breakpoints at ±10%, ±30%, ±60%, and 100%, so only
  genuinely large swings reach the most saturated colours.
- **Quick view** on the summary table flags any disease at the *currently
  selected location* whose most recent week is ≥30% above its trailing
  3-week average. If you'd prefer it to scan **all** locations at once
  (not just the one selected in the dropdown) rather than one at a time,
  let me know and I can change it to a cross-location scan.

## 5. Open questions for you

- Do you want the quick-view / 30% threshold to scan only the
  currently-selected location (current behaviour), or all
  districts+diseases at once regardless of the dropdown?
- Should the map's % change metric always use **reported** cases
  regardless of the toggle, or follow the toggle as it does now?
- Any preference on which years should be pre-selected by default in the
  line chart (currently: all years selected)?
