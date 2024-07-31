# Assembly Model Rescaling and Alignment

## Overview

This project is designed to rescale and align molecular assembly models using predefined component building units (CBUs). It processes an input CSV file containing assembly models and corresponding CBUs, performs rescaling operations on the assembly models, extracts specific atom positions, and aligns the CBUs to these positions.

## Project Structure

- `main.py`: Entry point of the project.
- `initiation.py`: Reads the input CSV and initiates the rescaling workflow.
- `AM_rescaler.py`: Rescales the assembly model based on CBUs.
- `cbu_aligner.py`: Aligns CBUs to specific positions in the rescaled assembly model.
- `find_distance.py`: Calculates distances between dummy atoms in CBUs for rescaling purposes.
- `Data/Assembly_Models`: Directory containing assembly models.
- `Data/CBUs`: Directory containing CBUs.
- `Data/Assembly_Models/Output_Atoms`: Directory for storing extracted atom positions.
- `Data/Assembly_Models/Translated_CBUs`: Directory for storing translated and aligned CBUs.

## Input

### CSV File
The input CSV file should be formatted as follows:
Assembly Model,CBU 1,GBU 1,CBU 2,GBU 2
AM_M6L12,CBU1,4-planar,CBU2,2-bent

## Usage

1. **Running the Script**
   To run the main script, use the following command:
   ```bash
   python main.py
   ```

### Directory Setup
Ensure the directories Data/Assembly_Models and Data/CBUs contain the necessary JSON files for assembly models and CBUs, respectively.

### Input File Path Configuration
Configure the paths for assembly_models_dir, cbus_dir, and csv_file_path in main.py:

```python
assembly_models_dir = 'Data/Assembly_Models'
cbus_dir = 'Data/CBUs'
csv_file_path = 'input.csv'
```

### Workflow
#### Initiation
The Initiation class in initiation.py reads the input CSV file and parses the assembly model and CBU information.
It then calls the AM_rescaler.py to perform the rescaling operations on the assembly model.

#### Rescaling
AM_rescaler.py uses the MoleculeAnalyzer class from find_distance.py to calculate distances and rescale the assembly model.
It rescales the assembly model based on distances derived from the CBUs and saves the rescaled model.

#### Extracting Atom Positions
Extracts unique atom positions from the rescaled assembly model and saves them to Data/Assembly_Models/Output_Atoms.

#### Aligning CBUs
cbu_aligner.py aligns the CBUs to the extracted positions from the rescaled assembly model.
Translated and aligned CBUs are saved to Data/Assembly_Models/Translated_CBUs.

### Detailed Workflow Steps
#### Initiation
Reads the CSV file.
Extracts the assembly model and CBU details.
Initializes AM_rescaler.py for rescaling.

#### Rescaling
Loads the assembly model JSON file.
Calculates target distances using find_distance.py.
Rescales atom positions based on calculated distances.
Saves the rescaled assembly model.

#### Extracting Atom Positions
Loads the final rescaled assembly model.
Extracts and saves unique atom positions not labeled as "Dummy" or "Center".

#### Aligning CBUs
Aligns the CBUs to the extracted atom positions.
Saves translated and aligned CBUs.

## Dependencies
- Python 3.x
- NumPy
- SciPy

## How to Contribute
1. Fork the repository.
2. Create a new branch (git checkout -b feature-branch).
3. Commit your changes (git commit -m 'Add some feature').
4. Push to the branch (git push origin feature-branch).
5. Open a pull request.

## License
This project is licensed under the MIT License.
