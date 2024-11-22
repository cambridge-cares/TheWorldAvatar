# Geospatial Analysis Utilities

This folder contains utilities for geospatial analysis, specifically aimed at identifying energy consumption hotspots across the UK. The analysis focuses on quantifying the spatial distribution of energy consumption attributes, which can be used in urban planning, energy policy, and resource allocation.

These utilities are used after successfully spinning up the UK Building Retrofit stack, utilizing the Ontop SPARQL endpoint to gather the required data.

## Prerequisites

- Access to the UK Building Retrofit stack and an active Ontop SPARQL endpoint.
- [Docker Desktop](https://docs.docker.com/get-docker/) or Docker Engine and Docker Compose.
- [Python 3.x](https://www.python.org/downloads/).
- A Python virtual environment (recommended).
- [Git](https://git-scm.com/downloads).

## How to Execute Calculation

1. **Ensure Stack is Running**: Make sure that the UK Building Retrofit stack is running, and the Ontop SPARQL endpoint is accessible to query the energy consumption data.
   
   Example configuration to check:
   ```json
   {
     "sparql_endpoint": "http://174.138.27.240:3838/ontop/sparql",
     "sparql_query_file": "ontop.sparql"
   }
   ```

2. **Select Spatial Weight Matrix**: Choose a spatial weight matrix for the analysis. By default, the k-nearest neighbors (k-NN) matrix is used for its computational efficiency.

   Example configuration:
   ```json
   {
     "spatial_matrix_type": "knn",
     "k": 8,
     "distance_threshold": 10000
   }
   ```

3. **Set Paths for Storage**: Define paths for storing the calculation results and font files needed for visualization.

   Example configuration:
   ```json
   {
     "output_file": "/your_local_path/output_results_with_hotspots.csv",
     "font_files_path": "/path/to/fonts"
   }
   ```

4. **Execution Steps**:
   - **Activate Virtual Environment**: Activate the Python virtual environment.
     ```bash
     source venv/bin/activate
     ```
   - **Install Required Packages**: Install all required dependencies.
     ```bash
     pip install -r requirements.txt
     ```
   - **Run Hotspot Identification**: Execute the hotspot identification script to calculate energy hotspots.
     ```bash
     python hotspots_identification.py
     ```

## Result Visualization (Hex Map)

Once the calculation is complete, visualize the results using the hexagonal binning approach to represent spatial clusters effectively:

- **Run Map Visualization**: Generate a visual representation of the identified hotspots using the provided script.
  ```bash
  python hotspots_map_vis.py
  ```

  The resulting hex map will illustrate the areas with concentrated high or low energy consumption values, allowing for easy identification of potential energy hotspots across the UK.

## Useful Links

- [Docker Documentation](https://docs.docker.com/)
- [Python Downloads](https://www.python.org/downloads/)
- [Git Installation](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
