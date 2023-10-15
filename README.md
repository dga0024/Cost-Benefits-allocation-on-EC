# Cost Benefits allocation on EC

## Introduction
Welcome to the Cost-Benefits allocation on energy communitires repository.
This piece of code has been created as support material for the paper titled "A simple and efficient method to allocate costs and benefits in energy communities"
https://doi.org/10.3926/jiem.5514(https://www.researchgate.net/publication/372482476_A_simple_and_efficient_method_to_allocate_costs_and_benefits_in_energy_communities)
Gonzalez-Asenjo, D., Izquierdo, L. R., & Sedano, J. (2023). A simple and efficient method to allocate costs and benefits in energy communities. Journal of Industrial Engineering and Management, 16(2), 398–424. 

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
Run the script depending on what figures do you want to generate, in both cases, the following steps will be automacitally followed if you execute the whole script:

1. In the very beginning, the code will ask for the folder where this repositotory has been downloaded, you have to select it, tipycally it will be named "C:\... ...\GitHub\Cost-Benefits-allocation-on-EC".
2. After that, the code will run by itself, but some interacion will appear at the console, to be taken into account:
   - The code will check if a list of packages are installed, then installing required packages if misees something
   - In "Ejecutar1_Modelo 5_paper_contenidos.R", while simulations run, it will print the name of the simulation that is running at the moment so that we can check that the system has not freezed
   - Finally, it will print the name of the figure that is creating at his moment.
  
## Look fot results
After running the piece of code, the results (One .jpg image per each one of the figures above mentioned) will be stored in a folder named "Graficos" in the same directory where the repository is. The code will check if this folfer exists and create it if not present.



 
