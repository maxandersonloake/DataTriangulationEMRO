# Pakistan IDSR Dashboard — setup

1. **Data**: place your CSV at `Data/PAK_IDSR_Data.csv` (same columns as your sample:
   `Disease, Province, Cases, Week, Year, Source link, Source title, date, datetime_loaded`).

2. **Logo**: add your WHO logo file as `www/logo_eng.png`, in the same `who_dashboard`
   folder as `app.R` (i.e. `Dashboard/who_dashboard/www/logo_eng.png` if `app.R` is at
   `Dashboard/who_dashboard/app.R`). Use the primary horizontal logo in WHO Blue or
   white (per WHO brand guidance, don't stretch/recolor it, and only use official
   artwork you're authorized to use — it isn't included here since the WHO logo may
   not be reproduced without WHO's permission).

   The app now reads this file directly off disk with `base64enc::dataURI()` and
   embeds it, rather than relying on Shiny's `www/` URL routing — this is more
   robust, but more importantly, **if the file can't be found it now prints a loud
   warning in the R console at startup** with the *exact absolute path* it checked
   and the current working directory, e.g.:
   ```
   WHO logo NOT FOUND. R is looking for it at:
     C:/Users/you/Documents/Dashboard/who_dashboard/www/logo_eng.png
   Check the file exists there with that exact name/case, and that
   app.R's working directory (getwd(): C:/Users/you/Documents/Dashboard/who_dashboard) is its own folder.
   ```
   The page itself also shows a red dashed placeholder box in that case, instead of
   silently showing nothing. If you still get "not found" after checking the path,
   the working directory line in that message is the thing to look at — Shiny apps
   run with their working directory set to the folder containing `app.R`, so if
   you're launching it a different way (e.g. `source()`-ing the file from the RStudio
   project root) the relative path can resolve from the wrong place.

3. **Packages**:
   ```r
   install.packages(c("shiny", "dplyr", "tidyr", "readr", "ggplot2",
                       "plotly", "DT", "scales", "base64enc"))
   ```

4. **Run**:
   ```r
   shiny::runApp("who_dashboard")
   ```

## What's in the dashboard

- **Disease trends tab**: pick a location (National or a district/province) and a
  disease. The current year plots as a solid WHO Navy Blue line; each earlier year
  is WHO Blue, fading progressively lighter the further back it is, with a legend
  to identify each. Hover over any point to see its year, week and case count
  (interactive, via `plotly`).
- **Weekly summary table tab**: every disease's cases for the most recent N weeks
  (default 8) of the latest year in your data, for the selected location. Each cell
  is shaded using WHO Emergency Red, from white (no increase / insufficient history)
  through to deep red, scaled by the percentage increase in that week's cases versus
  the average of the previous 3 weeks.

## Notes / things you may want to adjust

- "National" is computed by summing all provinces' cases for a given disease/week —
  if your source data already includes a "Pakistan" or "National" row for some
  diseases, you may end up double-counting; let me know if that's the case and I'll
  adjust the aggregation to prefer/skip existing national rows.
- The weekly table currently only shows the latest year in the data. If you want it
  to span a year boundary (e.g. show weeks 51–52 of last year alongside weeks 1–6 of
  this year), I can extend the ISO-week logic to handle that.
- Row/column sort order, additional filters (by source bulletin, date range), or a
  CSV/Excel export button on the table are easy additions if useful.
