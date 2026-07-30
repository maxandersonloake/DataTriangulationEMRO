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
    └── pakistan_admin1.geojson  <- from this delivery (map boundaries)
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
  "scales", "base64enc", "sf", "ggrepel"
))
```

`sf` needs system geospatial libraries (GDAL/GEOS/PROJ). Posit Connect
Cloud's standard R build image includes these, but if you test locally on
Windows, the easiest route is installing from CRAN binaries
(`install.packages("sf")` pulls prebuilt binaries on Windows — no
separate GDAL install needed).

**The map no longer uses leaflet.** After two rounds of it failing in
your environment (first an internet-dependent download, then a blank
render even with the bundled file), I've replaced it with a plain static
`ggplot2` + `sf` plot (`geom_sf`) — the same tooling already used
everywhere else in this app, rendered through the ordinary Shiny
`renderPlot`/`plotOutput` pair rather than a JS widget. It reads
`Data/pakistan_admin1.geojson` directly, colours each region by % change,
labels every region with its name and % change, and outlines whichever
region matches the Location dropdown in navy. There's no click-to-filter
any more (selection flows one way, from the dropdown to the map) — you
mentioned that trade-off was fine, and it removes an entire class of
failure mode. If the boundary file is ever missing/unreadable, the panel
shows a text message instead of crashing.

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

- **Data visualisation layout, polished**: Location/Disease now sit side
  by side in a centred shaded box above the chart and map. Both the chart
  and the map are wrapped in a matching white "card" panel
  (`.viz-panel`/`.viz-panel-controls` in the CSS) with a fixed-height
  controls footer, so the two panels line up at both the top and the
  bottom even though they contain different controls.
- **Legend labels**: plotly auto-generates combined legend entries like
  `(2026,Reported)` when both colour and linetype vary; these are now
  post-processed to read `2026, Reported`.
- **Compliance on hover**: now shown for both the Reported and Projected
  lines (previously only Reported).
- **Map title/caption**: renamed to "Percent change to rolling 3-week
  average, by region"; the "outlined in navy" caption underneath the map
  has been removed per your request (it's still true — the selected
  region is still outlined — just no longer captioned).

- **Map**: static `ggplot2`/`sf` render — no leaflet, no tiles, no click
  handling. Selection flows one-way from the Location dropdown to the map
  (thin navy outline on the matching region). Labels use `ggrepel` so
  they automatically space themselves apart and add a small leader line
  when a region is too small/crowded to hold its label directly (this is
  what fixed the AJK/KP/ICT overlap).
- **Data visualisation layout**: Location/Disease/Case-type controls now
  sit in a full-width row at the top; the chart and map sit side by side
  below; "Years to show" and the hover hint sit under the chart so the
  chart+controls column roughly matches the map's height.
- **Header**: logo enlarged (now 78px tall) and moved to the right;
  subtitle replaced with "Alpha Version". If you're still seeing a stale
  version after redeploying, it's almost certainly a browser/CDN cache
  issue — I bumped the CSS cache-busting query string again
  (`who_brand.css?v=4`) and the header styling is also applied as inline
  styles that don't depend on the external CSS file loading at all, so a
  hard refresh (Ctrl/Cmd+Shift+R) should resolve it either way.
- **Page alignment**: the Home and References tabs were using
  `margin: auto` to center a max-width block, which produced a large gap
  on wide screens. Switched to left-aligned padding (28px), matching the
  header and footer.
- **Alerts tab**: back to plain text lines, grouped by disease, e.g.
  "Pertussis: National (300% increase), ICT (42% increase)" — only
  locations that individually cross the 30% threshold are listed (not
  every location for that disease), ordered National-first then
  districts alphabetically, diseases sorted by their most severe location
  first. A note above the list states the % change is versus each
  location's own trailing 3-week average. Has its own reported/projected
  toggle.
- **Weekly table colour scale**: unchanged from the last round — ordinary
  week-to-week noise within ±30% stays plain white, colour only appears
  beyond that, reaching full saturation only beyond ±100%.
- **Projected total cases** = reported cases ÷ (compliance % / 100),
  computed per region/week from `PAK_IDSR_Compliance.csv`. Where
  compliance is 0% or missing, the projection is left blank (undefined)
  rather than showing `Inf`.
- **Year colours**: current year is always solid WHO Navy; earlier years
  cycle through a set of distinct WHO secondary colours (blue, orange,
  green, magenta, yellow, purple) rather than shades of one colour.

## 5. Open questions for you

- Should the map's % change metric always use **reported** cases
  regardless of the toggle on the Data visualisation tab, or follow the
  toggle as it does now?
- The boundary file's district→province groupings (e.g. which district
  falls in KP vs Balochistan) came from my own knowledge of Pakistan's
  administrative geography, not an official source — if you spot a
  misplaced district on the map, let me know and I'll correct the
  mapping and re-dissolve the shapes.
