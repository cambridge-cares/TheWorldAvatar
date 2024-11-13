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

## Input:
  CSV File:
    The input CSV file should be formatted as follows:
    Assembly Model,CBU 1,GBU 1,CBU 2,GBU 2
    AM_M6L12,CBU1,4-planar,CBU2,2-bent

## Data/CBUs Folder:
    The CBUs folder contains JSON files with detailed information on each chemical building unit.

## CBU JSON Structure:
    Each JSON file in the CBUs folder describes individual atoms, their coordinates in 3D space, and the bonds between them. Atoms with bending or non-planar structures, or those with long substituent groups, include an additional "center" position, which is used in the alignment workflow by the MOP Assembler.
    
      atom_id:
        atom: "Element"
        coordinate_x: X.XX
        coordinate_y: Y.YY
        coordinate_z: Z.ZZ
        bond:
          - to_atom: "bonded_atom_id"
            bond_order: N.N
        mmtype: "Metal_Type"
        qmmm: "QM/MM label"

## Assembly Models:
    The file assembly_models.json contains descriptions of how various CBUs are assembled into larger structures. Each entry describes the positions of building units, their connectivity, and the positions of "dummy" atoms, which mark connectivity between generic building units. These dummy atoms assist in the alignment and scaling of complex geometries during the MOP Assembler workflow.

    Example model:
      "(3-pyramidal)x2(2-bent)x3_D3h":
        - Key: "Position_1"
          Label: "3-pyramidal"
          X: X.XX
          Y: Y.YY
          Z: Z.ZZ
          Neighbors:
            - Key: "Position_3"
              Label: "2-bent"
              Distance: D.DD
          ClosestDummies:
            - "Dummy_1"
            - "Dummy_2"

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

## License:
  This project is licensed under the MIT License.
