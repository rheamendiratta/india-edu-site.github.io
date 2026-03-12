# India Education Dashboard
 
An interactive data visualization platform that brings together education indicators from the World Bank (WDI, HCI, SSGD), UDISE+, NAS, ASER, and OECD into a three-level view: World (India vs. other countries), India (states compared), and States (districts within a state).
 
**Live site:** [https://rheamendiratta.github.io/india-edu-site.github.io/world/](https://rheamendiratta.github.io/india-edu-site.github.io/world/)
 
**Shiny apps:**
- World > Map: [https://rheamendiratta.shinyapps.io/worldmap/](https://rheamendiratta.shinyapps.io/worldmap/)
- World > Students: [https://rheamendiratta.shinyapps.io/world_students/](https://rheamendiratta.shinyapps.io/world_students/)
 
---
 
## Design Decisions and Discussion
### 1. Introduction
 
India runs the largest school system in the world, with over 250 million children enrolled, and yet the country's education outcomes still fall short of both global averages and its own ambitions. The challenge is not really about access anymore. India has reached near-universal primary enrolment, which is a genuine achievement. But learning is a different story, where more than half of Indian 10-year-olds cannot read a simple text, and the World Bank's Human Capital Index gives India a score of 0.49, meaning a child born there today will reach only about half of their productive potential.
 
The data needed to understand this exists, but it is scattered across sources that were never designed to work together. The World Bank's WDI and HCI datasets handle global comparisons. UDISE+ covers school-level administrative data across India's 36 states and union territories. NAS tracks learning outcomes through periodic assessments. ASER checks whether children can actually read and do basic math at the household level. Each of these sources uses its own methods, its own definitions, and its own year formats, which makes it hard to see the full picture in one place.
 
That is the gap this dashboard tries to fill. Right now, the World Bank's tools show global data but nothing at the Indian state level. UDISE's own portal covers Indian administrative data but offers no international context. There is no single platform that lets someone move between these views. Additionally, UDISE, NAS, PISA, among other sources sometimes measure the same things but the reports are scattered and not easily comparable or presented differently. This dashboard brings them together so that users can see how India compares to the world, how Indian states compare to each other, and how districts within a state differ, all through interactive maps and charts that can be filtered by indicator, year, gender, and school level - all to understand how we are doing on all aspects of education, from budget to literacy outcomes.
 
### 2. Methods
 
**How the site is built:** The dashboard is a Jekyll static site hosted on GitHub Pages, with R Shiny apps embedded into each page through iframes and hosted on shinyapps.io. Jekyll takes care of page layout and navigation, and Shiny hosts all the interactive content: the maps, charts, toggles, and sub-tabs. The static page appears right away, even when the Shiny app is still loading. The site is organized around three geographic levels, each with topic tabs (such as Map, Students, Teachers, Schools & System, Finance & Expenditure, Human Capital). Every tab runs its own dedicated Shiny app rather than one large monolithic app, which keeps each one small and quick to load.
 
**Data:** The world-level data comes from the World Bank's data360 API. I pulled indicators from five different World Bank datasets (WDI, HCI, SSGD, HCP, and EDSTATS), along with 7 OECD indicators and 8 PISA raw data files. The starting list had 262 rows, but I found 11 cases where the exact same indicator appeared in more than one dataset, so I removed the duplicates and ended up with 251 unique indicators (there is more processing to be done here). UDISE data comes from Excel files downloaded from udiseplus.gov.in. All cleaned data is saved as RDS files inside each Shiny app's folder, because RDS loads in under a second. Map outlines use Natural Earth 110m country shapes, simplified to 5% detail with rmapshaper.
 
**World Map:** The World > Map app is a choropleth where users pick an indicator from a dropdown and the map colors each country by its value. I built this with Leaflet rather than Plotly, because an early Plotly version re-sent the entire GeoJSON on every interaction, causing ShinyApp timeouts. Leaflet's leafletProxy() pattern loads shapes once and only updates fill colors. The color ramp runs from warm sand (#f2dcc5) through rose (#b8818a) to deep indigo (#2e3250). Countries with no data are near-black (#1A1A1A) with a separate "No data" legend entry. A coverage bar chart below the map shows how many countries report data for each year.
 
**World Students:** The World > Students app has two sub-tabs with four charts: GER and NER (Enrolment & Access), and Repetition Rate and Persistence to Last Grade (Completion & Flow). Each chart defaults to a bar chart ranking countries, with India highlighted in dusty rose. All four include a UDISE overlay in the over-time view. India's World Bank line is solid and the UDISE line is dashed. I only included this overlay where both sources genuinely measure the same thing through different data pipelines. Gender colors use coral-rose (#d4787a) for female and blue (#4a80b4) for male, chosen for colorblind accessibility.
 
### 3. Results and Discussion
 
**Map:** India sits in the lower-middle range on the Human Capital Index, below China and far below East Asian leaders. But on primary net enrolment, India is above 90%. When you flip to learning poverty, over half of 10-year-olds cannot read. This contrast between high enrolment and low learning quality is the core story in Indian education. India spends about 3-4% of GDP on education, below the global average of roughly 4.5%.
 
**Students tab:** India's primary GER is above 100% (over-age enrolment), but tertiary GER is only about 30%. Female GER has caught up to or passed male GER at primary and secondary levels. The NER chart shows the pipeline where primary NER is high, secondary NER drops sharply. India's repetition rate is low (2-4%), likely reflecting automatic promotion policies under the Right to Education Act of 2008. Persistence to last grade sits around 85-90%.
 
**UDISE overlay:** On GER and NER, UDISE and World Bank figures for India can differ by 2-5 percentage points because they use different population denominators (Census vs. UN estimates). On repetition rate, the gap can be larger. These disagreements are a feature rather than a problem. Both numbers appear on the same chart, and users can see the gap and think about what it means. No other public tool shows this kind of side-by-side comparison for Indian education data.
 
**Truthful:** The coverage chart shows when data is missing. NA countries cannot be mistaken for low scores. UDISE stays separate from WB on global charts. Every indicator includes a plain-language description.
 
**Functional:** The three-level setup matches how people ask education questions. The toggles add complexity one step at a time. The year slider resets per indicator. There is scope to make this more accessible by reducing navigation complexity using AI.
 
**Beautiful:** I use a consistent color palette across Jekyll and Shiny, with colorblind-safe gender colors and the Inter typeface (inspired by a Friend's personal website) with warm off-white backgrounds.
 
**Insightful:** The UDISE overlay shows gaps that are not easy to see otherwise. The sidebar callout gives India's value and the coverage chart reveals uneven global data infrastructure, calling for more data reporting and coverage.
 
**Enlightening:** High enrolment and low learning are easy to compare through the maps, and will become more thorough as more indicators are populated on the app. The NER chart highlights the pipeline from primary to middle to secondary school. 
 
---

## Repo Structure
 
```
docs/                         Jekyll site (GitHub Pages)
shiny/world/map/              World > Map Shiny app
shiny/world/students/         World > Students Shiny app
data/clean/                   Cleaned data files (RDS)
data/clean/geo/               Simplified world polygons (RDS)
data_pipeline/                R scripts for fetching & cleaning
```
 
## How to Run Locally
 
### Software needed
 
- R (>= 4.2.0)
- RStudio (recommended)
- Jekyll (Ruby >= 2.7, bundler, jekyll)
 
### R packages
 
```r
install.packages(c(
  "shiny", "bslib", "leaflet", "plotly", "dplyr", "tidyr",
  "sf", "rmapshaper", "jsonlite", "httr2", "arrow", "fs",
  "rsconnect", "shinyjs", "stringr", "readxl"
))
```
 
### Steps
 
1. Clone the repo: `git clone https://github.com/rheamendiratta/india-edu-site.github.io`
2. Fetch World Bank data: `Rscript data_pipeline/fetch_wb.R`
3. Clean data: `Rscript data_pipeline/clean_map_data.R`
4. Copy cleaned RDS files into app folders (see shiny_handoff.docx for paths)
5. Run a Shiny app: open `shiny/world/map/` in RStudio and click Run App
6. Run the Jekyll site: `cd docs && bundle install && bundle exec jekyll serve`
 
### Data Sources
 
| Source | Access |
|--------|--------|
| World Bank WDI, HCI, SSGD | data360 API (data360api.worldbank.org) |
| OECD | data-explorer.oecd.org |
| UDISE+ | Excel downloads from udiseplus.gov.in |
| NAS 2021 | Scraped CSV from github.com/gsidhu/NAS-2021-data |
| GeoJSON | Natural Earth 110m via datasets/geo-countries on GitHub |
 
Raw WB data is fetched via API (not included in repo). UDISE Excel files must be downloaded manually from udiseplus.gov.in and placed in `data/raw/udise/`.
