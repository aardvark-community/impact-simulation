# A tool for interactive visual analysis of 3D impact simulations
This tool was created by Elitza Vasileva during an internship at VRVis and as a project in Visual Computing for TU Wien. It is written in F# and JavaScript using the [Aardvark platform](https://github.com/aardvark-platform/aardvark.docs/wiki). 

The project's goal was to visualize and explore multivariate [data](https://www.vrvis.at/publications/PB-VRVis-2020-010) provided by Maindl et al. , who study the kinematic impact of a spacecraft as an opportunity to avert possibly dangerous asteroids that could collide with the Earth. The high-dimensional data simulation represents a section of an asteroid consisting of a large number of 3D points. Each point has its own attributes such as position, speed, mass, density, energy, pressure, alpha jutzi and other domain-specific internal simulation values. The main objective was to interactively explore the data and provide helpful visualizations to support the intuitive and understandable analysis of the data. As a result, two two-dimensional user interfaces were integrated with a 3D view and additional controls logically connected with the visualizations.

![](https://github.com/aardvark-community/impact-simulation/blob/master/src/ImpactVisualization/resources/impactvis.jpg)

# The simulation data

The simulation data, insight and great support was provided by:
 - Thomas Maindl, recently at [Astronomer at University of Vienna](https://homepage.univie.ac.at/thomas.maindl/) and now [CEO @ SDB Science-Driven Business Ltd.](https://sdb.ltd/)
 - Christoph Schäfer, [Computational Physicist at University of Tübingen](https://www.tat.physik.uni-tuebingen.de/~schaefer/)

# Authors

This work was carried out by Elitza Vasileva in the context of an internship @ [VRVis](https://www.vrvis.at/). The project will be continued towards a virtual reality application as part of Elitza's master thesis, supervised by [Harald Steinlechner](https://www.vrvis.at/ueber-uns/team/infos/steinlechner-harald).

# License

The source code is free while the data included is used in courtesy of Thomas Maindl and Christoph Schäfer.

# How to build
build.cmd/build.sh, then run the ImpactVisualization.exe in bin/Release/netcoreapp2.1/win-x64.

# Run from VisualStudio
src/Impact-Visualization.sln

# UI Controls and Tools
**1. Animation flow** - Play or pause the animation and activate All Frames animation for the histogram  
**2. Change point size** - Changes the sizes of the 3D points  
**3. Change color map** - Dropdown menu to change the current color map/transfer function  
**4. Change attribute** - Dropdown menu to change the current attribute  
**5. Set clipping planes and domain range** - Clipping planes to restrict the extent of the point cloud and three axes to control the domain range  
**6. Handle out of range data** - Either discard or color the points that are out of range  
**7. Change data range** - Sets the min and max values for the current attribute  
**8. Set a probe** - Choose a probe (each one is a cube placed at the center of the visualization and has a different size) to activate the histogram and parallel coordinates visualizations  
**9. Legend** - Show the current color map and the respective min and max values  
**10. Visualization techniques** - Histogram and Parallel Coordinates to explore data and apply brushing to filter out the data in the 3D view

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
