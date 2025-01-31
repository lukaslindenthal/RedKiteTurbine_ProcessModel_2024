## Wind Turbine and Red Kite Simulation Overview

This project simulates the placement of wind turbines and the distribution of 
red kites within a 100x100 grid over a series of timesteps. The simulation 
accounts for:

- Wind Turbine Placement: Turbines are constructed in clusters for efficiency, 
with a few randomly placed turbines to simulate real-world scenarios.
- Red Kite Dynamics: The population and movement of red kites are modeled using
a stochastic approach (Poisson distribution) combined with the Ricker function 
for population growth.

The goal is to explore the interaction between wind turbine development and the
habitat of red kites.

**Authors: Neele H. and Lukas L.**

**Date: December 2024**

### Project Structure:

The project consists of two main scripts:

- Parameter Initialization (Turbo_region_parameter_setting.R):
        - Defines the grid size, timesteps, initial turbines, and obstacles.
        - Sets up the simulation environment.

- Simulation Model (Turbo_kite_model.R):
        - Implements the simulation logic for turbine placement and red kite 
          dynamics.
        - Includes visualization for each timestep.
     
        
### Requirements

This project requires R and the following libraries:

- `ggplot2`
- `dplyr`


### How to Run:

- Set Up Parameters:
    - Run the `Turbo_region_parameter_setting.R` script to initialize the grid,
      obstacles (placeholder for cities), turbines, and red kites.

- Run the Simulation:
    - Execute the `Turbo_kite_model.R` script to simulate turbine placement and 
      red kite dynamics over specified timesteps.

- Visualize the Results:
    - The simulation will display a series of plots showing the grid, turbine 
      placements (red), and red kites (green) at each timestep.
      

### Future Improvements

- Implement the Baden-Württemberg.shp file as geographical boundaries.
- Implement a buffer zone around wind turbines to better simulate red kite 
  avoidance behavior.
- Add additional ecological constraints for red kite population dynamics.
- Explore the impact of increased turbine density on red kite mortality rates.

### notes from review to work on 
von ELENA 
"Ich verstehe nicht ganz das Turbine Placement:
- Es gibt die for-Schleife von 1:n_new, in der die neuen Turbinen in der Nähe von bestehenden Turbinen platziert werden. Wenn keine gültigen Koordinaten gefunden werden, wird die Iteration abgebrochen und die Turbine wird gar nicht platziert.
- -Danach kommt das „Random turbine placement“ unter der Bedingung, dass n_new nicht 1 ist (und das ist ja sehr wahrscheinlich) und es noch freie Plätze gibt
-Dann wird eine Zufallszahl zwischen n_new und turb_neu_max generiert und diese Anzahl an Turbinen wird dann zufällig platziert – das heißt, es gibt dann am Ende meistens mehr neue Turbinen als die Anzahl n_new erstmal glauben lässt?"

von ANNE
„Fehler in selected_random_coords[i, 1] : Indizierung außerhalb der Grenzen (bei Veränderung der Timesteps)“


