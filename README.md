######DESCRIPTION#####################################################################
# In addition to the material properties of Au and Ti, this repository consists of the code for describing the heat input from laser medium. The nature of heat flux is the main reason for the temperature fluctuation in the computational domain, thereby requiring the use of T-dependent material properties in the numerical model.

# The  heat input from laser to the surface 
1. The heat flux from flat top heat source is imported in the finite element model through the user defined function  (filename: LaserHeatSource.F90)

# temperature_dependent_material_properties
1. The viscosity (mu) data (Pa S) for Au and Ti are presented in the table of temperature and viscosity columns.
2. The specific enthalpy (J/kg)  data for Au and Ti are presented in the table of temperature and specific enthalpy (h) columns.
3. The thermal conductivity (W/(m K) ) of Au and Ti are defined as a function of temperature through the user defined functions.
4. The density (kg/m^3 ) of Au and Ti are defined as a function of temperature through the user defined functions.


# The .dat files can be directly called from the solver input file of Elmer.
# elmerf90 compiler must convert the .F90 file to .so file for enabling the calling of the user defined functions by the solver input file (sif) of ElmerSolver.


##########HOW TO CITE THIS WORK:############################################################################
