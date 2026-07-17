# DataTriangulationEMRO
Automated collection and presentation of public health data to support the World Health Organization (WHO) Health Emergency Department (WHE) Regional Office for the Eastern Mediterranean Region (EMRO). 

**Disclaimer:** This project is an independent volunteer contribution created as part of the UN volunteers progrram, and the author is not employed by the World Health Organization (WHO). This repository is provided for research and software development purposes only and should not be interpreted as an official WHO product or endorsement.

**Contact:** Max Anderson Loake, mandersonloake@gmail.com

## Description
This repository contains code for automating the collection, processing, and presentation of public health data from multiple sources. The aim is to streamline data aggregation and facilitate consistent reporting to support situational awareness and emergency response activities.

## Instructions for use

Install the required R packages before running the project. 

```r
source("install_packages.R")
```

Alternatively, install the required packages manually using `install.packages()`.

### Running the project

1. Clone this repository.
2. Open the project in RStudio (recommended) or your preferred R environment.
3. Install the required packages.
4. Run the main script:

```r
source("main.R")
```

This script executes the complete data extraction, processing, and reporting workflow.

### Repository Structure

| File/Folder | Description |
|-------------|-------------|
| `main.R` | Main script that orchestrates the complete workflow. |
| `R/` | Helper functions used throughout the project. |
| `data/` | Input data files and intermediate datasets (where applicable). |
| `output/` | Generated tables, figures, reports, and other outputs. |
| `config/` | Configuration files, parameters, and credentials (if used). |
| `README.md` | Project documentation and usage instructions. |
