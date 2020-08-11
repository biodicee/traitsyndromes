# cas04_traitsyndromes

This repository contains simulation code as well as analytical code for statistics and figures for the research paper 'Facilitation and the evolution of trait-syndromes in the Mediterranean vegetation' by Florian D. Schneider, Simon Benateau, Marina C. Rillo & Sonia Kéfi.


## Project background

The project on evolution of trait syndromes in the Mediterranean climate has been developed in the Masters Thesis of Marina Rillo at ISEM, Montpellier supervised by Sonia Kéfi and co-supervised by Florian D Schneider. 

The simulation study addresses the evolution and stable coexistence of two very different plant trait syndromes in the Mediterranean basin. 

## Files

The repository contains the following code files: 

### Simulation code

The code directory contains a raw simulation code file `code/full_climate_change_v4.R`, which can be modified to run simulations of any parameter specification. 

For running simulations, create a copy of that file in a subfolder of 'simresults', and edit the file accordingly. Pay particular attention to edit

- target folder
- simulation title
- target parameters
- check: constant or change scenario

### Simulation Results

The folder `simresults` contains raw results from the simulations as presented in the paper. Two scenarios have been simulated: 

- `simresults\constant` contains results from a simulation under a constant climate scenario.  
- `simresults\change` contains results from a simulation under changing climate conditions 

### Analytical code & Figures

The essential functions for analysing or visualising are placed in `functions.R`. 

Each figure is created by a standalone code file (in `code`-directory). The pdf output is directed to the `figures` directory. Figures 2 and 4 are plotted as pixel graphics (png) because of high data density. 

### Manuscript

The manuscript is currently being developed in Overleaf.com. 
A snapshot of the pre-submission manuscript will be placed in the `manuscript` directory. 

## CITE AS

Schneider, F.D., Benateau, s., Rillo, M., Kéfi, S. (2020) Code repository for: Facilitation and the evolution of trait-syndromes in the Mediterranean vegetation


## LICENSE

Copyright 2020 Schneider, F.D., Benateau, s., Rillo, M., & Kéfi, S.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


