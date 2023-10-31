__author__ = "Aleksandar Kondinski"
__license__ = "MIT"  
__version__ = '0.1.0'  
__status__ = "development" 

from ase.io import read, write
from ase.io.dftb import write_dftb
import itertools


def cif_to_hd(cif_file, gen_file):
    # Read a structure from a CIF file
    atoms = read(cif_file)

    # Write the structure to a DFTB+ input file in GEN format
    with open(gen_file, 'w') as dftb_file:
        write_dftb(dftb_file, atoms)
    print(f"Converted {cif_file} to {gen_file}")

def extxyz_to_hsd(extxyz_file, gen_file):
    # Read a structure from an extended XYZ file
    atoms = read(extxyz_file)

    # Write the structure to a DFTB+ input file in GEN format
    with open(gen_file, 'w') as dftb_file:
        write_dftb(dftb_file, atoms)
    print(f"Converted {extxyz_file} to {gen_file}")

def gen_2_cif(gen_file, cif_file):
    # Read a structure from a .gen file
    atoms = read(gen_file)

    # Write the structure to a CIF file
    write(cif_file, atoms)
    print(f"Converted {gen_file} to {cif_file}")

def write_hsd_file(elements, skf_files, output_filename):

    max_angular_momentum = {
        "H": "s",
        "B": "p",
        "C": "p",
        "N": "p",
        "O": "p",
        "Al": "p",
        "Si": "p",
        "P": "p",
        "Cu": "d", 
        "Na": "s",
        "Ti": "d"
    }

    with open(output_filename, 'w') as f:
        # Write the header part of the HSD file
        f.write("Geometry = GenFormat {\n")
        f.write("    <<< \"geo_end.gen\"\n")
        f.write("}\n\n")
        
        # Write the Driver block
        f.write("Driver = ConjugateGradient {\n")
        f.write("   MaxSteps = 300\n")
        f.write("   MaxForceComponent = 1E-2\n")
        f.write("   MovedAtoms = 1:-1\n")
        f.write("   LatticeOpt = Yes\n")
        f.write("   Isotropic = No\n")
        f.write("   OutputPrefix = \"geom.out\"\n")
        f.write("   Convergence {GradAMax = 1E-2}\n")
        f.write("}\n\n")
        
        # Write the Hamiltonian block
        f.write("Hamiltonian = DFTB{\n")
        f.write("   Dispersion = LennardJones {\n")
        f.write("      Parameters = UFFParameters {}\n")
        f.write("   }\n")
        f.write("   MaxAngularMomentum = {\n")
        for element in elements:
            angular_momentum = max_angular_momentum.get(element, "s")
            f.write(f"      {element} = \"{angular_momentum}\"\n")
        f.write("   }\n")
        f.write("   SCC = Yes\n")
        f.write("   Filling = Fermi {\n")
        f.write("      Temperature [Kelvin] = 100\n")
        f.write("   }\n\n")
        f.write("   KPointsAndWeights = SupercellFolding {\n")
        f.write("       1 0 0\n")
        f.write("       0 1 0\n")
        f.write("       0 0 1\n")
        f.write("       0.5 0.5 0.5\n")
        f.write("   }\n\n")
        f.write("   SlaterKosterFiles {\n")
        for skf in skf_files:
            f.write(f"       {skf} = \"/matsci-0-3/{skf}.skf\"\n")
        f.write("   }\n")
        f.write("}\n\n")

        # Write the Options and ParserOptions blocks
        f.write("Options {\n")
        f.write("   WriteResultsTag = Yes\n")
        f.write("}\n\n")
        f.write("ParserOptions {\n")
        f.write("   IgnoreUnprocessedNodes = Yes\n")
        f.write("}\n\n")
        
        # Write the Analysis block
        f.write("Analysis {\n")
        f.write("   CalculateForces = Yes\n")
        f.write("   ProjectStates {\n")
        f.write("      Region {\n")
        f.write("         Atoms = C\n")
        f.write("         ShellResolved = Yes\n")
        f.write("         Label = \"pdos.C\"\n")
        f.write("      }\n")
        f.write("   }\n")
        f.write("}\n")


def initiate_hsd_file(gen_filename, skf_files, output_filename="dftb_in.hsd"):
    # Extract element names from the GEN file
    with open(gen_filename, 'r') as f:
        lines = f.readlines()
        if len(lines) < 2:
            return "The file doesn't have enough lines to extract element names."
        element_line = lines[1].strip()
        elements = element_line.split()
    
    # Generate all possible binary combinations of elements, including self-interactions
    binary_combinations = set(itertools.combinations_with_replacement(elements, 2))
    
    # Convert the tuples in binary_combinations to a format compatible with the SKF filenames
    formatted_combinations = set()
    for combo in binary_combinations:
        formatted_combinations.add(combo[0] + "-" + combo[1])
        if combo[0] != combo[1]:
            formatted_combinations.add(combo[1] + "-" + combo[0])
    
    # Check which SKF files exist
    existing_skf_files = set(skf_files).intersection(formatted_combinations)
    missing_skf_files = formatted_combinations - existing_skf_files

    # Write to the HSD file
    write_hsd_file(elements, existing_skf_files, output_filename)

    return {
        'existing_skf_files': existing_skf_files,
        'missing_skf_files': missing_skf_files
    }

def initiate_hsd_file1(gen_filename, skf_files,output_filename="dftb_in.hsd"):
    # Extract element names from the GEN file
    with open(gen_filename, 'r') as f:
        lines = f.readlines()
        if len(lines) < 2:
            return "The file doesn't have enough lines to extract element names."
        element_line = lines[1].strip()
        elements = element_line.split()
    
    # Generate all possible binary combinations of elements
    binary_combinations = set(itertools.combinations(elements, 2))
    
    # Convert the tuples in binary_combinations to a format compatible with the SKF filenames
    formatted_combinations = set()
    for combo in binary_combinations:
        formatted_combinations.add(combo[0] + '-' + combo[1])
        formatted_combinations.add(combo[1] + '-' + combo[0])
    
    # Check which SKF files exist
    existing_skf_files = set(skf_files).intersection(formatted_combinations)
    missing_skf_files = formatted_combinations - existing_skf_files

    # Write to the HSD file
    write_hsd_file(elements, existing_skf_files, output_filename)

    return {
        'existing_skf_files': existing_skf_files,
        'missing_skf_files': missing_skf_files
    }

# List of SKF files
skf_files = [
    "Al-Al", "Al-C", "Al-Cu", "Al-H", "Al-Na", "Al-O", "Al-P", "Al-Si",
    "B-B", "B-C", "B-H", "B-N", "B-O",
    "C-Al", "C-B", "C-C", "C-H", "C-N", "C-O", "C-P", "C-Si", "C-Ti",
    "Cu-Al", "Cu-Cu", "Cu-H", "Cu-Na", "Cu-O", "Cu-Si",
    "H-Al", "H-B", "H-C", "H-Cu", "H-H", "H-N", "H-Na", "H-O", "H-P", "H-Si", "H-Ti",
    "N-B", "N-C", "N-H", "N-N", "N-O", "N-P", "N-Si", "N-Ti",
    "Na-Al", "Na-Cu", "Na-H", "Na-Na", "Na-O", "Na-Si",
    "O-Al", "O-B", "O-C", "O-Cu", "O-H", "O-N", "O-Na", "O-O", "O-P", "O-Si", "O-Ti",
    "P-Al", "P-C", "P-H", "P-N", "P-O", "P-P", "P-Si", "P-Ti",
    "Si-Al", "Si-C", "Si-Cu", "Si-H", "Si-N", "Si-Na", "Si-O", "Si-P", "Si-Si",
    "Ti-C", "Ti-H", "Ti-N", "Ti-O", "Ti-P", "Ti-Ti"
]

#gen_filename = "geo_end.gen"
#result = initiate_hsd_file(gen_filename, skf_files)
#print("Existing SKF files:", result['existing_skf_files'])
#print("Missing SKF files:", result['missing_skf_files'])


#if __name__ == "__main__":
#    # Example of calling functions with specific file paths
#    cif_to_hd('path/to/input.cif', 'path/to/output.gen')
#    extxyz_to_hsd('path/to/input.extxyz', 'path/to/output.gen')
#    gen_2_cif('path/to/input.gen', 'path/to/output.cif')
