## Wind Turbine and Red Kite Simulation Overview

This project simulates the interaction between wind turbine placement and red 
kite population dynamics on a 100×100 grid over a series of timesteps (with each 
timestep representing one year). The model integrates environmental constraints 
with stochastic red kite behavioral and demographic processes to explore the 
ecological impacts of wind turbine development on red kite populations.
The goal is to explore the interaction between wind turbine development and the
habitat of red kites.


# Key features and assumptions

Turbine Placement:
    - Turbines are build in clusters (placing new turbines near existing ones)
    - Also, the turbine placement is modeled independently of the red kite population, 
      under the assumption that there would be no initial impact assessment or mitigation. 
    - A buffer zone is created around each turbine to simulate the area of potential impact.

Red Kite Dynamics:
    - The initial population of Red kite nests are based on an assumed density of 3 
      pairs per 100 km², and also a small fraction of lonely (adult) individuals.
    - Each nest and lone kite has an age that increases every timestep. If the age
      of a nest (or lone individual) exceeds the life expectancy (12 years) it dies.
    - Kites also die when they are located in cells with turbines or within turbine buffers, 
    either by movement or newly buold turbines.
    - Lonely adults (kites with age above reproduction threshold) move via a random 
      dispersal within the boundaries. When multiple kites converge on the same cell,
      the duplicates are checked and then only two individuals remains to form a nest. 
      Extra individuals are either reassigned new coordinates or, if reassignment fails after several attempts, marked as dead.
    - Nests are assumed to produce only one juvenile per timestep/year. Reproduction 
      is implemented using a Ricker-type formulation combined with Poisson. 
      If a nest reproduces, a juvenile is added (and the nest is then flagged with having a 
      juv). This makes sure, that each nest reproduces no more than once per year.

Visualization:
    - The simulation creates a series of plots, at each timestep showing the spatial 
      distribution of turbines, red kites (nests, lone individuals, and marked mortality).
      The separate scatterplot summarizes the dynamics over time, displaying the 
      number of turbines, total red kite abundance, and the number of red kites killed.


Project Structure:

Parameter Initialization (new_Time_Dim_Turbo_Region_para_setting.R):
    - Sets the grid dimensions, resolution, number of timesteps, initial turbine 
      placement, obstacles (e.g., building buffers), and dispersal parameters.

Red Kite Parameter Setup (new_Redkite_eco_para_setting.R):
    - Defines red kite ecological parameters including reproduction rates, survival, 
      life expectation, and also the initial population abundance.
      Initializes a 4D array for red kite state variables: 
      nest presence, juvenile flag, abundance, ages (nests and lone kites), and 
      mortality counts (for turbine construction and movement).

Simulation Model (model_version3.R):
    - Implements the main simulation loop:
        1. Turbine Construction: 
            New turbines are added based on existing turbine density, with random 
            placements supplementing clustered growth.
        2. Red Kite Dynamics:
        3. Mortality from turbine construction (cells with turbines/buffers) comes first.
        4. Aging of nests and lone kites is performed (kites/nests > life expectancy removed)
        5. Movement of lonely adults is simulated, including reassigning coordinates 
            when multiple kites meet at a cell.
            Duplicate handling ensures that only a pair survives at a cell 
            where more than two kites meet.
            Reproduction is simulated using a Ricker–Poisson approach, with each 
            nest allowed to produce at most one juvenile per timestep.
        6. Visualization: The model produces spatial plots at each timestep.

Visualization Scripts:
    - Additional scripts are provided to create maps (showing grid, turbine placements,
      and red kite distributions) and scatterplots that summarize population 
      dynamics over time.


Dependencies:
  The project requires R and the following libraries:
  - `ggplot`
  - `dplyr`
  - `tidyr`


How to Run:

Note that all files can be run seperately, to initialze the simulation step by step.
Alternatively only the `new_visualize_model.R` script needs to be run, as the pother
files are sourced into the file.

Parameter Setup:
    - Run `new_Time_Dim_Turbo_Region_para_setting.R` to initialize the simulation environment 
    - Run `new_Redkite_eco_para_setting.R to set red` kite ecological parameters and 
      initialize the red kite population

Simulation Execution:
    - Execute `model_version3.R` to run the main simulation loop.
    
Visualization:
    - RUn the `new_visualize_model.R` file to see the result for each timestep.
    - Use the provided script to generate maps of each timestep and scatterplots 
      summarizing turbine, red kite, and mortality dynamics.

Future Improvements:

- Introduce a helper layer for the aging and movement processes so that these updates 
  are not immediately written into the final state layer.
- Add a final consistency check after all movement and nest placements to ensure that 
  abundance values and state transitions are correct.
- For a different model design, store kites and turbines in a table (with fields 
  such as x, y, age, etc.) and always retrieve/update the state from that table
  rather than using multi-dimensional arrays.




**Authors: Neele H. and Lukas L.**

**Date: December 2024**

