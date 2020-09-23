# A tool for interactive visual analysis of 3D impact simulations
This tool was created by Elitza Vasileva during an internship at VRVis and as a project in Visual Computing for TU Wien. It is written in F# and JavaScript using the [Aardvark platform](https://github.com/aardvark-platform/aardvark.docs/wiki). 

The project's goal was to visualize and explore multivariate [data](https://www.vrvis.at/publications/PB-VRVis-2020-010) provided by Maindl et al. , who study the kinematic impact of a spacecraft as an opportunity to avert possibly dangerous asteroids that could collide with the Earth. The high-dimensional data simulation represents a section of an asteroid consisting of a large number of 3D points. Each point has its own attributes such as position, speed, mass, density, energy, pressure, alpha jutzi and other domain-specific internal simulation values. The main objective was to interactively explore the data and provide helpful visualizations to support the intuitive and understandable analysis of the data. As a result, two two-dimensional user interfaces were integrated with a 3D view and additional controls logically connected with the visualizations.

![](https://github.com/aardvark-community/impact-simulation/blob/master/src/ImpactVisualization/resources/impactvis.jpg)

# How to build
build.cmd/build.sh, then run the ImpactVisualization.exe in bin/Release/netcoreapp2.1/win-x64.

# Run from VisualStudio
src/Impact-Visualization.sln

# Controls
 Control   | Description
--- | ---
W,A,S,D	|  First person controls to move through the scene (forward, left, backward, right)
Left mouse button | Rotate the camera (around its own axis) and also used to select/change the UI controls
Right mouse button	|  Zoom in and out of the scene by moving the mouse up or down 
Wheel Up, Wheel Down | Zoom in and out of the scene
Wheel Button | Changes the position of the camera in 2D (no depth)

# Data location

Data type | Path
--- | ---
Hera data | [..\impact-simulation\data](https://github.com/aardvark-community/impact-simulation/tree/master/data)
JavaScript and .css files | [..\impact-simulation\src\ImpactVisualization\resources](https://github.com/aardvark-community/impact-simulation/tree/master/src/ImpactVisualization/resources)
.csv files | [..\impact-simulation\src\ImpactVisualization\resources\csv](https://github.com/aardvark-community/impact-simulation/tree/master/src/ImpactVisualization/resources/csv)
Transfer functions | [..\impact-simulation\src\ImpactVisualization\resources\transfer](https://github.com/aardvark-community/impact-simulation/tree/master/src/ImpactVisualization/resources/transfer)
