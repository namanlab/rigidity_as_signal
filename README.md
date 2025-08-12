# Rigidity as Signal — Simulation Code and Visualizations

This repository contains the simulation code, visualizations, and supporting materials for the paper:

> **Rigidity as Signal: A Game-Theoretic Explanation for Persistent Vacancies in Urban Real Estate Markets**  
> *Author:* Naman Agrawal, Department of Economics, National University of Singapore  

## Overview

This project implements and visualizes a two-period signaling game model of urban real estate markets, where seller pricing decisions (hold at a high price or discount) act as signals of unobservable seller characteristics. The code reproduces simulation results, equilibrium illustrations, and comparative statics discussed in the paper.

## Repository Structure

```
├── imgs/ # Figures and diagrams generated from simulations
├── LICENSE # Licensing information for use and distribution
├── plots.R # Visualization scripts for simulation outputs
├── sim.R # Core simulation code for model dynamics
└── README.md # Project documentation (this file)
```

## Computational Environment

All simulations were conducted locally on:

- **Machine:** MacBook Pro (Model Number: MPHE3HN/A)  
- **OS:** macOS 14.5 (Sonoma)  
- **Processor:** Apple M2 Pro (10-core CPU: 6 performance + 4 efficiency cores)  
- **Memory:** 16 GB unified memory  
- **R Version:** 4.3.1 (2023-06-16)  

The Apple Silicon architecture was leveraged for optimized computation and parallel execution where possible.

## Dependencies

The following R packages were used for simulation, statistical computation, and visualization:

- **tidyverse** – Data manipulation, transformation, and general utilities  
- **patchwork** – Combining multiple plots into composite visualizations  
- **ggridges** – Ridge plots for distributional visualizations  
- **latex2exp** – Rendering LaTeX expressions in plot labels and annotations  
- **purrr** – Functional programming tools for simulation workflows  
- **mvtnorm** – Multivariate normal distribution functions for statistical modeling  

To install all required packages:

```r
install.packages(c("tidyverse", "patchwork", "ggridges", "latex2exp", "purrr", "mvtnorm"))
```



## Usage

1. Clone or download this repository.  
2. Open `sim.R` in R or RStudio to run the core simulation and generate model outputs.  
3. Use `plots.R` to produce publication-quality figures. Generated plots are saved to the imgs/ directory.

## Output

- **Equilibrium diagrams** illustrating pooling and separating regimes.  
- **Comparative statics plots** showing the effect of holding costs, optimism multipliers, and discount factors.  
- **Vacancy rate distributions** under different equilibrium configurations.  

All figures are stored in the `imgs/` directory in PNG format.

## License

See [LICENSE](LICENSE) for details on usage and distribution rights.

## Citation

If you use this code or figures in academic work, please cite:

Agrawal, N. (2025). *Vacancy as Signal: A Game-Theoretic Explanation for Persistent Vacancies in Urban Real Estate Markets*. National University of Singapore.


