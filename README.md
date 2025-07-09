# AIC Explorer

AIC Explorer is a Shiny web application that lets users query the Art Institute of Chicago API for **artworks**, **artists**, and **exhibitions**, then explore the returned data through tables, numerical summaries, and interactive plots.

---

## 📦 Required R Packages

The app relies on the following packages:

* **shiny**       : for building the web app
* **DT**          : interactive tables
* **plotly**      : interactive plots
* **dplyr**       : data manipulation
* **tidyr**       : data tidying
* **ggplot2**     : plotting backend
* **rlang**       : programming with tidy evaluation
* **httr**        : HTTP requests to the API
* **jsonlite**    : parsing JSON responses
* **janitor**     : creating contingency tables

---

## 🛠️ Install Dependencies

Run this line in R to install all required packages at once:

```r
install.packages(c(
  "shiny",
  "DT",
  "plotly",
  "dplyr",
  "tidyr",
  "ggplot2",
  "rlang",
  "httr",
  "jsonlite",
  "janitor"
))
```

---

## ▶️ Launch the App

Once you have the packages installed, launch the app directly from GitHub:

```r
shiny::runGitHub(
  repo     = "Project2_API",
  username = "mhongji",
  ref      = "main"
)
```

This single command will download the repository and start the Shiny app in your R session.

---

### Repository Structure

```
Project2_API/
├── app.R               # Main Shiny application
├── R/
│   ├── api_helpers.R   # Functions to query the AIC API
│   └── summaries.R     # Functions to summarize and plot the data
├── www/
│   └── cover.jpg       # Cover image displayed in the About tab
└── README.md           # You are here!
```
