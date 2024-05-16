[![DOI](https://zenodo.org/badge/663203965.svg)](https://zenodo.org/doi/10.5281/zenodo.11200196)

# Cost Benefits allocation on EC

## Introduction
Welcome to the Cost-Benefits allocation on energy communitires repository.
This piece of code has been created as support material for the paper:
Gonzalez-Asenjo, D., Izquierdo, L. R., & Sedano, J. (2023). [A simple and efficient method to allocate costs and benefits in energy communities](https://www.jiem.org/index.php/jiem/article/download/5514/1060). Journal of Industrial Engineering and Management, 16(2), 398–424. DOI: [10.3926/jiem.5514](https://doi.org/10.3926/jiem.5514)

The purpose of these scripts is to reproduce the graphical results presented in the paper, in particular:
The "Ejecutar1_Modelo 5_paper_contenidos.R" script generates the illustrations:
- Fig8: Energy consumption profiles
- Fig9: Savings % scenarios
- Fig10: Susplus scenarios
- plus two more graphs that help to understand the impact of the "Fraction of comsumption covergae with PV sysmtem"
  - A: Energy costs scenarios
  - B: Energy self-consumtion scenarios.

The "Ejecutar2_Modelo 5_paper_comparativa" script generates the illustration:
- Fig11: Allocation systems simulation


## How to proceed
Run the script depending on what figures do you want to generate, in both cases, the following steps will be automatically followed if you execute the whole script:

1. In the very beginning, the code will ask for the folder where this repositotory has been downloaded, you have to select it, tipycally it will be named "C:\... ...\GitHub\Cost-Benefits-allocation-on-EC".
2. After that, the code will run by itself, but some interacion will appear at the console, to be taken into account:
   - The code will check if a list of packages are installed, then installing required packages if misees something
   - In "Ejecutar1_Modelo 5_paper_contenidos.R", while simulations run, it will print the name of the simulation that is running at the moment so that we can check that the system has not frozen
   - Finally, it will print the name of the figure that is creating at the moment.
  
## Look for results
After running the piece of code, the results (One .jpg image per each one of the figures above mentioned) will be stored in a folder named "Graphs" in the same directory where the repository is. The code will check if this folder exists and create it if not present. 
Besides, a sample of how the graphs should look like is given in the folder "Graphs_sample". Here you will find the 6 graphs that are mentioned in this repository.

## Play with the code
In the case that you want to explore a little more about the results of this code, the Excel file called "Initial_conditions" is provided. THere, you can define certain initial conditions for the development of the calculations. Once defined, you could run again the code and see the differences between graphs.
These initial conditios are:
- Number of agents "N" that form the energy community, given 100 by default
- Number of simulations in the range of 0%-200% "Demand coverage ratio with PV" parameter, in the code this variable is named "Num_perfiles_gen", and represents the number of simulations that are going to be calculated and then represented in the X axis of the graphs, such as Fig9 or Fig10, among others, given 51 by default
- The selected simulation within the "Num_perfiles_gen" group to be simulated along the different cost allocation methods. In the code this variable is named "PERFILGEN" and is mainly used to generate Fig11 graph, given 10 by default

Please note that only integer numbers must be introduced in this Excel to avoid the code to crash
 
