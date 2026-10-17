# DataTriangulationEMRO

Automated collection and presentation of public health data to support the World Health Organization (WHO) Health Emergency Department (WHE) Regional Office for the Eastern Mediterranean Region (EMRO).

**Disclaimer:** This project is an independent volunteer contribution created as part of the UN volunteers program, and the author is not employed by the World Health Organization (WHO). This repository is provided for research and software development purposes only and should not be interpreted as an official WHO product or endorsement.

**Contact:** Max Anderson Loake, mandersonloake@gmail.com

---

## Overview

The repository contains an R Shiny dashboard for monitoring weekly Integrated Disease Surveillance and Response (IDSR) case reporting from **Pakistan** and **Somalia**, together with the scripts that collect and prepare the data behind it. The dashboard is an internal analytical tool: its alerts and projections are preliminary and do not replace Public Health Intelligence (PHI) processes, expert epidemiological review or verification by national authorities.

Switch between countries with the drop-down under the title banner. Each country has the same six tabs:

| Tab | What it shows |
|-----|---------------|
| **Home** | What the dashboard is for, the main data limitations, and how projected cases, non-reported provinces and alerts are calculated. |
| **Alerts** | Every disease and location scanned for unusually large increases in the chosen week, grouped by disease. |
| **Data visualisation** | Weekly case trend for a disease and location, an interactive map of the deviation from expected baseline (SD) by region, and a stacked chart of each province's (Pakistan) or state's (Somalia) contribution to total cases. |
| **Weekly summary table** | The week-by-week breakdown by location, shaded by SD. |
| **District-level data** | District epi curve, district map and table, for provinces/states that report at district level. |
| **References** | Data sources, boundary sources and methodology links. Pakistan's also has a tab for downloading the underlying CSVs. |

### Data

* **Pakistan** – weekly bulletins published by the National Institute of Health (NIH) at <https://www.nih.org.pk/phb/weekly-bulletin>. Province-level and district-level case counts and reporting compliance are extracted from the bulletin PDFs.
* **Somalia** – a weekly case-count workbook that is not publicly redistributed. The dashboard reads an encrypted copy of the processed data, and the Somalia page sits behind a password.

Case counts are mostly suspected rather than laboratory-confirmed cases, and are best treated as an early-warning signal.

### Methods in brief

* **Alerts and SD shading** use a modified CDC EARS-style CUSUM/C2 aberration detection method. For each disease and location, the 9 weeks before the week being assessed are used as a baseline, the 2 most recent of those are dropped as a guard band, and the mean and standard deviation of the remaining 7 weeks set the alert thresholds (mean + 2, 3 and 4 SD). At least 10 calendar weeks of history are needed, at least 3 baseline weeks must have data, and a minimum SD of 1 is applied to flat baselines. See the Home and References tabs for the full description and papers.
* **Projected total cases** = reported cases ÷ (compliance % ÷ 100). Left blank where compliance is 0% or missing. Somalia currently has no compliance data, so it reports case counts only.
* **Non-reported provinces/regions.** A region that did not report in a week is treated as missing, not zero. When looking at national totals, a non-reporting province can either contribute only in the weeks it reported (default) or be removed from every week for a like-for-like comparison.
* **Somalia regions.** The maps use the 18 official regions. The workbook reports at district level, so each region's weekly cases are the sum of its districts. A few workbook regions that have no boundary of their own are merged into their parent region (Marodi Jeh and Sahil into Woqooyi Galbeed, Ayn into Togdheer, Karkaar and Ras-Asayr into Bari, South Mudug into Mudug), and the dashboard lists whichever of these apply under the map. The trend plot can be filtered by state, then region, or by region across all states.
* **Boundaries.** The Data visualisation maps use EMRO boundaries, with the EMRO disputed areas drawn as dashed lines (also on Pakistan's district map) and the WHO map disclaimer shown under every map. The district-level maps use the UN OCHA Common Operational Datasets (COD) for [Pakistan](https://data.humdata.org/dataset/cod-ab-pak) and [Somalia](https://data.humdata.org/dataset/cod-ab-som).

---

## Repository contents

```
DataTriangulationEMRO/
├── app.R                              Shiny dashboard (UI and server)
├── www/
│   ├── who_brand.css                  Dashboard styling
│   └── logo.png                       WHO EMRO logo
├── encryption_utils.R                 Encrypt/decrypt helpers, and the Somalia page-password helpers
├── scripts/                           Data and deployment scripts (run from the project root)
│   ├── pakistan_extract_bulletins.R   Downloads and parses the NIH bulletins (Pakistan)
│   ├── pakistan_run_pipeline.R        Driver for the Pakistan pipeline; writes Data/run_log.txt
│   ├── somalia_process_workbook.R     Processes the Somalia workbook into an encrypted bundle
│   ├── emro_prepare_boundaries.R      Builds the EMRO boundary layers from the raw shapefiles
│   └── update_renv_manifest.R         Records package versions (renv.lock) and refreshes manifest.json
├── .github/workflows/update_data.yml  Daily GitHub Action that refreshes the Pakistan data
├── DESCRIPTION                        Package list used by the GitHub Action (not an installable package)
├── renv.lock, renv/, .Rprofile        Package versions (renv)
├── manifest.json                      Posit Connect Cloud deployment manifest
├── Somalia_Raw/                       Raw Somalia workbook (git-ignored, never committed)
├── Reports/                           Name-matching diagnostics written by the Somalia pipeline
└── Data/
    ├── PAK_IDSR_Data.csv                          Pakistan case counts, province level
    ├── PAK_IDSR_Compliance.csv                    Pakistan reporting compliance, province level
    ├── PAK_IDSR_Data_District.csv                 Pakistan case counts, district level
    ├── PAK_IDSR_Compliance_District.csv           Pakistan reporting compliance, district level
    ├── failed_reports.txt, run_log.txt            Pakistan pipeline logs
    ├── SOM_IDSR_Data.enc                          Somalia data (encrypted)
    ├── pakistan_admin1.geojson                    Dissolved province shapes
    ├── emro_disputed_areas.geojson                EMRO disputed areas (dashed lines)
    ├── emro_boundaries_src/                       Raw EMRO shapefiles (input to 2_2)
    ├── pak_admin_boundaries/
    │   ├── pak_admin1.geojson                     Province outlines (OCHA COD)
    │   ├── pak_admin1_emro.geojson                EMRO provinces (output of 2_2)
    │   ├── pak_admin2.geojson                     District boundaries (OCHA COD)
    │   ├── pak_admin3.geojson                     Tehsil boundaries (OCHA COD)
    │   └── pak_admin2_jaffarabad_split.geojson    Districts with Jaffarabad split in two
    └── som_admin_boundaries/
        ├── som_admin1.geojson                     Regions (OCHA COD)
        ├── som_admin1_emro.geojson                EMRO regions (output of 2_2)
        └── som_admin2.geojson                     Districts (OCHA COD)
```

`pak_admin2.geojson` and `pak_admin3.geojson` are not read by the app; they are the source of `pak_admin2_jaffarabad_split.geojson`. In that file the single Jaffarabad district polygon is split into "Usta Muhammad" (the Gandakha and Usta Mohammad tehsils) and "Jaffarabad" (the remaining Jhat Pat tehsil), so the district data reported separately for each matches its own polygon.

---

## Running the dashboard locally

1. Clone the repository and open `DataTriangulationEMRO.Rproj` in RStudio.
2. Restore the package versions with `renv::restore()`.
3. Run the app:

```r
shiny::runApp()
```

The Somalia page needs the `SOMALIA_DATA_KEY` environment variable (see below). Without it the Somalia page shows a "Somalia data not available" message instead of its content, and Pakistan is unaffected.

---

## Updating the dashboard

### Pakistan data (automatic)

`.github/workflows/update_data.yml` runs every day at 05:00 UTC. It checks the NIH bulletin page, extracts any new or changed weeks with `scripts/pakistan_run_pipeline.R` (which sources `scripts/pakistan_extract_bulletins.R`), and commits the updated CSVs in `Data/` back to the repository. A summary of the run (new weeks, fuzzy name matches, unmatched districts, parsing warnings) is written to `Data/run_log.txt` and emailed when there is something worth reading; a separate email is sent if the run fails outright.

The workflow can also be started by hand from the Actions tab, with an option to skip the district-level tables. To do the same locally:

```bash
Rscript scripts/pakistan_run_pipeline.R                          # province and district level
INCLUDE_DISTRICT=false Rscript scripts/pakistan_run_pipeline.R   # province level only
```

The email settings are stored as repository secrets (`MAIL_SERVER`, `MAIL_PORT`, `MAIL_USERNAME`, `MAIL_PASSWORD`, `MAIL_TO`). New R packages needed by the extraction script must also be added to `Imports:` in `DESCRIPTION`.

### Somalia data (manual)

The Somalia workbook is not a public download, so this step is run by hand whenever there is an updated workbook.

1. Save the workbook as `Somalia_Raw/IDSR_data.xlsx`. This folder is git-ignored, so the raw data is never committed.
2. Set `SOMALIA_DATA_KEY` in a `.Renviron` file (the project's own `.Renviron` is git-ignored) to the passphrase that is also set as a secret environment variable on Posit Connect Cloud. The two must match exactly.
3. Run `Rscript scripts/somalia_process_workbook.R`. It tidies and fuzzy-matches the State, Region and District names, rebuilds the case series, and writes the encrypted `Data/SOM_IDSR_Data.enc`. Any names it could not match are listed in `Reports/`.
4. Commit and push `Data/SOM_IDSR_Data.enc` only.

`scripts/somalia_process_workbook.R` also needs `readxl` and `stringdist`, which are not dependencies of the app itself.

### Somalia page password

The Somalia page is locked behind a password. Its content is only built and sent to the browser once the password is entered, for that browser session only, and five wrong attempts lock the session until the page is reloaded. Because the repository is public, the password itself is not stored: `app.R` holds only a random salt and a slow hash (`SOMALIA_PAGE_PW_SALT`, `SOMALIA_PAGE_PW_HASH`). To change the password:

```r
source("encryption_utils.R")
make_page_password_hash("new password")
```

then paste the two lines it prints over those constants near the top of `app.R`.

### Boundaries

The EMRO admin-1 layers and the disputed-area lines are built from the raw shapefiles in `Data/emro_boundaries_src/` by `scripts/emro_prepare_boundaries.R`. Re-run it (`Rscript scripts/emro_prepare_boundaries.R` from the project root) if any of those shapefiles are replaced. The shapefiles are stored as `.shp` files only, without attribute tables, so the script names each shape by finding which shape in the older OCHA layers it overlaps most, assumes WGS84 coordinates, and asserts 7 Pakistan provinces and 18 Somalia regions. It also:

* merges FATA into Khyber Pakhtunkhwa;
* carries Azad Kashmir and Gilgit-Baltistan over from `pak_admin1.geojson`, because the EMRO file treats that area as disputed (it is covered by the disputed-areas layer instead), while the dashboard still reports data for both.

The district-level maps (OCHA COD) are unchanged by this and use `pak_admin2_jaffarabad_split.geojson` and `som_admin2.geojson`. District names in the data do not always match the boundary files exactly; `app.R` matches them through a normalisation function and a small alias table (`resolve_dist_key()` and `SOM_DISTRICT_NAME_ALIASES`), and lists any it cannot match under the district map.

### Logo, styling and text

The logo is `www/logo.png` and all styling is in `www/who_brand.css`. The `?v=` number on the stylesheet link in `app.R` should be increased after editing the CSS so browsers do not serve a cached copy. The text of the Home and References tabs is written directly in `app.R`.

---

## Deploying to Posit Connect Cloud

1. After adding or changing a package, or adding, renaming or deleting any deployed file, refresh the lock file and manifest:

```bash
Rscript scripts/update_renv_manifest.R
```

2. Push to GitHub. In Posit Connect Cloud choose **Publish > Shiny app**, select the repository and branch, and point it at `app.R` in the repository root. Connect Cloud installs packages from `renv.lock`.
3. Under the app's publish settings, add `SOMALIA_DATA_KEY` as a secret environment variable.

`app.R` must sit next to the `Data/` and `www/` folders, since all paths in it are relative to the project root.
