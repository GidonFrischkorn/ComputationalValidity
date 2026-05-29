# ComputationalValidity

Companion R package for **Frischkorn, Rebmann, & Oberauer (submitted): "Validity is a Theoretical Problem — A Computational Psychometrics Perspective on How to Measure Cognition."**

## Overview

This package implements simulation-based validity analyses that demonstrate how generative cognitive models can assess the validity of behavioral indicators independently of cross-task correlations. The manuscript focuses on two models of conflict processing:

- **DMC** (Diffusion Model for Conflict) — temporal conflict between automatic and controlled evidence accumulation
- **SSP** (Shrinking Spotlight) — attentional narrowing from broad to focused processing

The simulations reveal four measurement phenomena: process impurity, selective validity, reliability–validity dissociations, and correlation transfer failure.

The repository also contains simulation code and results for the **M3** (Memory Measurement Model) applied to complex span tasks, which extends the framework to working memory measurement with categorical data. These M3 materials are not included in the current manuscript but are available for reuse.

## Repository Structure

```text
R/                 Package functions (simulation, analysis, plotting)
scripts/           Standalone scripts to run simulations and generate figures
reports/           Quarto manuscript (apaquarto format, renders to .docx and .pdf)
figures/           Generated manuscript figures
data/              Simulation datasets (not tracked in git — see Data Availability below)
output/            Raw simulation results (not tracked in git)
```

---

## Use Case A: Using the Functions in Your Own Work

If you only need the simulation and analysis functions from `R/`, install the package directly from GitHub:

```r
# install.packages("remotes")
remotes::install_github("gidonfrischkorn/ComputationalValidity")
```

The installed package exports all functions but does not include simulation scripts, data files, or figures.

---

## Use Case B: Reproducing the Manuscript Figures and Analyses

This is the primary use of the repository. Follow these steps exactly.

### Step 1: Clone the repository

```bash
git clone https://github.com/GidonFrischkorn/ComputationalValidity.git
```

### Step 2: Open the RProject

Open `ComputationalValidity.Rproj` in RStudio. **This step is required.** The `here` package anchors all file paths to the `.Rproj` file location. Running scripts without opening the project will cause file I/O to fail silently (wrong working directory).

### Step 3: Verify your setup

```r
source("scripts/check_setup.R")
```

This checks that `here()` is anchored correctly, required packages are installed, and reports which data files are present. Resolve any `[FAIL]` items before proceeding.

### Step 4: Download the simulation data

```r
source("scripts/download_data.R")
```

Downloads the processed `.rda` datasets (~318 MB) from [OSF (osf.io/6qnrv)](https://osf.io/6qnrv/) into `data/`, and the aggregated simulation outputs into `output/` if available.

> **Note on M3 data:** The 6 M3 dataset files (`m3_*.rda`) are not on OSF because the M3 analyses are not part of the current manuscript. `check_setup.R` will flag these as missing but clearly distinguishes them from the manuscript-critical files.

### Step 5: Generate manuscript figures

Each script automatically loads the package from source (no separate install needed):

```r
source("scripts/Generate_DMC_Plots.R")
source("scripts/Generate_SSP_Plots.R")
source("scripts/Generate_Model_Comparison.R")
source("scripts/Generate_ModelValidation_Plots.R")
source("scripts/Generate_OverestimationPlots.R")
```

Figures are saved to `figures/manuscript/`.

### Step 6: Render the manuscript (optional)

```r
quarto::quarto_render("reports/ValidityComputationalModelling.qmd")
```

Or from the terminal: `quarto render reports/ValidityComputationalModelling.qmd`

---

## Use Case C: Re-running the Full Simulations

The simulations are computationally expensive (hours per model). Only needed if you want to verify or modify the simulation design itself.

### Pipeline

```r
# 1. Run simulations — each writes results to output/
source("scripts/Simulation_DMC_Recovery.R")
source("scripts/Simulation_DMC_Correlation.R")
source("scripts/Simulation_SSP_Recovery.R")
source("scripts/Simulation_SSP_Correlation.R")
# M3 (not in manuscript):
# source("scripts/Simulation_M3_Recovery.R")
# source("scripts/Simulation_M3_Correlation.R")

# 2. Process results into package datasets — reads from output/, writes to data/
source("scripts/SaveData_DMC_Recovery.R")
source("scripts/SaveData_DMC_Correlation.R")
source("scripts/SaveData_SSP_Recovery.R")
source("scripts/SaveData_SSP_Correlation.R")

# 3. Continue with Use Case B Step 5
```

---

## Data Availability

Simulation datasets (~318 MB total) are hosted on [OSF (osf.io/6qnrv)](https://osf.io/6qnrv/) because the largest file exceeds GitHub's per-file size limit. The `data/` and `output/` directories are excluded from the git repository (see `.gitignore`). Run `scripts/download_data.R` to populate them.

## License

MIT
