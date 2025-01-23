# MOP Assembler: Assembly Model Rescaling, Alignment, and Workflow

## Overview:
  This project facilitates the rescaling, alignment, and assembly of molecular models using Chemical Building Units (CBUs) for constructing metal-organic polyhedra (MOPs). By leveraging predefined CBUs, the tool processes input data to rescale assembly models, aligns CBUs, and enables MOP construction through JSON-based workflows. The data is organized into JSON files, which describe the atomic and bonding structure of these CBUs.

## Project Structure:
 - main.py: Entry point of the project for initiating the rescaling, alignment, and MOP construction workflow.
 - mop_am_gbu_positions.py: Manages the positions of GBUs within the assembly models.
 - mop_am_rescaler.py: Rescales the assembly model based on CBUs.
 - mop_cbu_aligner.py: Aligns CBUs to specific positions in the rescaled assembly model.
 - mop_cbu_find_distance.py: Calculates distances between dummy atoms in CBUs for precise alignment.
 - mop_jsons2xyz.py: Converts JSON files into XYZ format for further processing.
 - mop_operations.py: Handles various operations related to the manipulation and adjustment of molecular models.

## Data Folder and Input Paths:
- **Assembly Models Directory**: `Data/Assembly_Models` – Directory containing assembly model JSON files.
- **CBUs Directory**: `Data/CBUs` – Directory containing CBU JSON files.
- **Workflow Data File**: `Data/input.csv` – CSV file containing data required for the workflow.

### Input CSV (Info describing the MOP Target):

The workflow requires an input CSV file named `input.csv`, located in the `Data` directory. The path to this file can be configured in the `main.py` script by updating the `workflow_data_file` variable.

  CSV File:
    The input CSV file should be formatted as follows:
    Assembly Model,CBU 1,GBU 1,CBU 2,GBU 2
    AM_M6L12,CBU1,4-planar,CBU2,2-bent

### Input CBU JSON (Info describing the CBU input):
    The CBUs folder contains JSON files with detailed information on each chemical building unit.

    Each JSON file in the CBUs folder describes individual atoms, their coordinates in 3D space, and the bonds between them. Atoms with bending or non-planar structures, or those with long substituent groups, include an additional "center" position, which is used in the alignment workflow by the MOP Assembler.
    
   {
      "99a00b53-c5a2-4fb0-9d4c-60f9eba4b284": {
         "atom": "Cr",
         "coordinate_x": 0.0,
         "coordinate_y": 0.0,
         "coordinate_z": 0.0,
         "bond": [
               {
                  "to_atom": "6572a6ad-021a-4b0c-971e-8f441d890a94",
                  "bond_order": 1.0
               },
               {
                  "to_atom": "1b7cc2e8-78c4-443b-bb0e-5fca2fff1f55",
                  "bond_order": 1.0
               },
               ...
         ],
         "mmtype": "Cr4+2",
         "qmmm": "MM"
      },
      "14db1344-f3a8-4646-8733-915000558618": {
         "atom": "Cr",
         "coordinate_x": 0.0,
         "coordinate_y": 0.0,
         "coordinate_z": -2.141683,
         "bond": [
               {
                  "to_atom": "6572a6ad-021a-4b0c-971e-8f441d890a94",
                  "bond_order": 1.0
               },
               {
                  "to_atom": "1b7cc2e8-78c4-443b-bb0e-5fca2fff1f55",
                  "bond_order": 1.0
               },
               ...
         ],
         "mmtype": "Cr4+2",
         "qmmm": "MM"
      },
      ...
      "c94c5281-faec-43e3-9898-7122555da0d6": {
         "atom": "X",
         "coordinate_x": -0.169096,
         "coordinate_y": 2.13638,
         "coordinate_z": 5.30183,
         "bond": [
               {
                  "to_atom": "eabc6e1e-83d0-46e7-b806-cd9f3b37155e",
                  "bond_order": 1.0
               },
               {
                  "to_atom": "0246dbbc-510f-472f-b4b4-e1443a538ad9",
                  "bond_order": 1.0
               }
         ],
         "mmtype": "O_HH",
         "qmmm": "MM"
      },
      "CENTER": {
         "atom": "CENTER",
         "coordinate_x": -0.16909599999999997,
         "coordinate_y": -1.22602075,
         "coordinate_z": 3.7247382499999997,
         "bond": [],
         "mmtype": "C_R",
         "qmmm": "MM"
      }
   }

### Input Assembly Models JSON:
    The file assembly_models.json contains descriptions of how various CBUs are assembled into larger structures. Each entry describes the positions of building units, their connectivity, and the positions of "dummy" atoms, which mark connectivity between generic building units. These dummy atoms assist in the alignment and scaling of complex geometries during the MOP Assembler workflow.

   {
      "(5-pyramidal)x12(2-linear)x30_Ih": [
         {
               "Key": "Position_1",
               "Label": "5-pyramidal",
               "X": 2.6368,
               "Y": 2.7551,
               "Z": 1.2068,
               "Neighbors": [
                  {
                     "Key": "Position_13",
                     "Label": "2-linear",
                     "Distance": 2.1029
                  },
                  {
                     "Key": "Position_15",
                     "Label": "2-linear",
                     "Distance": 2.1029
                  },
                  ...
               ],
               "ClosestDummies": ["Dummy_1", "Dummy_2", "Dummy_3", ...]
         },
         ...
      ],
      {
         "Key": "Dummy_1",
         "Label": "Dummy",
         "X": 2.8490,
         "Y": 1.7266,
         "Z": 1.2590,
         "Positions": ["Position_13", "Position_1"]
      },
      ...
      {
         "Key": "Center",
         "Label": "Center",
         "X": 0.0,
         "Y": 0.0,
         "Z": 0.0
      }
      ...
   }

## Center Positions:
    Some structures with non-planar units or complex geometries contain additional "center" atoms or positions, which help in the alignment process. These center positions are crucial for ensuring that the MOP Assembler tool correctly positions and scales the CBUs during model assembly.

## Usage:
  Running the Script:
    To run the main script, use the following command:
    python main.py

## Directory Setup:
    Ensure the directories Data/Assembly_Models and Data/CBUs contain the necessary JSON files for assembly models and CBUs, respectively.

## Input File Path Configuration:
    Configure the paths for assembly_models_dir, cbus_dir, and csv_file_path in main.py:
    assembly_models_dir: 'Data/Assembly_Models'
    cbus_dir: 'Data/CBUs'
    csv_file_path: 'input.csv'

## Dependencies:
  - Python 3.x
  - NumPy
  - SciPy