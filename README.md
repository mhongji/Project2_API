# AIC Explorer

A Shiny app that queries the Art Institute of Chicago API ([https://api.artic.edu](https://api.artic.edu)) to explore, summarize, and visualize data on artworks, artists, and exhibitions.

---

## 📦 Required R Packages

The app depends on the following packages:

* **httr** — HTTP requests
* **jsonlite** — parse JSON responses
* **dplyr**, **tidyr** — data wrangling and tidying
* **ggplot2** — static plotting
* **plotly** — interactive plots
* **DT** — interactive data tables
* **janitor** — contingency tables & cleaning
* **shiny** — web app framework

---

## 🚀 Install All Packages

Run this in a clean R session to install dependencies:

```r
install.packages(c(
  "httr",
  "jsonlite",
  "dplyr",
  "tidyr",
  "ggplot2",
  "plotly",
  "DT",
  "janitor",
  "shiny"
))
```

---

## ▶️ Run the App

Once packages are installed, run:

```r
shiny::runGitHub("your-username/project-2-artic")
```

Replace `your-username` and `project-2-artic` with your GitHub handle and repository name.

---

## 📂 Repo Structure

```
project-2-artic/
├── R/                # Helper functions:
│   ├── api_helpers.R # – API query functions
│   └── summaries.R   # – Contingency tables, numeric summaries, plot helpers
├── www/              # Static assets (e.g. artic_logo.png)
├── app.R             # Shiny app code (ui + server)
├── README.md         # This file
└── .gitignore        # Files and folders to ignore in Git
```

---

## 📝 About

**Purpose:**

* Query the Art Institute of Chicago API for artworks, artists, and exhibitions.
* Return tidy data frames for download or further analysis.
* Build contingency tables and numerical summaries for categorical and quantitative variables.
* Create both standard (bar, box) and advanced (heatmap) plots.
* Offer a multi‑tab Shiny interface with dynamic UI elements.

Feel free to fork, adapt, and showcase this project on your personal website or GitHub pages!

