# Facilitation and the evolution of trait-syndromes in the Mediterranean vegetation

This repository contains simulation code as well as analytical code for statistics and figures for the research paper 'Facilitation and the evolution of trait-syndromes in the Mediterranean vegetation' by Florian D. Schneider, Simon Benateau, Marina C. Rillo. Miguel Verdú & Sonia Kéfi, currently in preparation. 


## Project background

The code on evolution of trait syndromes in the Mediterranean climate has been developed in the Masters Thesis of Marina Rillo at ISEM, CNRS Montpellier, supervised by Sonia Kéfi and co-supervised by Florian D Schneider and has been revised by Simon Benateau in a subsequent project. 

The simulation study addresses the evolution and stable coexistence of two very different plant trait syndromes in the Mediterranean basin. 

## Files

The repository contains the following code files: 

### Simulation code

The code directory contains a raw simulation code file `code/full_climate_change_v4.R`, which can be modified to run simulations of any parameter specification. 

For running simulations, create a copy of that file in a subfolder of 'simresults', and edit the file accordingly. Pay particular attention to edit

- target directory for output
- simulation title
- target parameters 
- set climate to `constant` or `change` scenario

### Simulation Results

The folder `simresults` contains raw results from the simulations as presented in the paper. Two scenarios have been simulated: 

- `simresults\constant` contains results from a simulation under a constant climate scenario.  
- `simresults\change` contains results from a simulation under changing climate conditions 

### Analytical code & Figures

The essential functions for analysis or visualisation are placed in `functions.R`. 

Each figure is created by a standalone code file (in `code`-directory). The pdf output is directed to the `figures` directory. Figures 2 and 4 are plotted as pixel graphics (png) because of high data density. 

### Manuscript

A snapshot of the pre-submission manuscript will be included in the `manuscript` directory upon acceptance of the journal. 

## CITE AS

Schneider, F.D., Benateau, s., Rillo, M., Kéfi, S. (2022) Code repository for: Facilitation and the evolution of trait-syndromes in the Mediterranean vegetation


## LICENSE

This project is licensed under the terms of the [MIT license](LICENSE.md).

