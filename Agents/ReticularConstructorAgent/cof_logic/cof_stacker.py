import os
import re
from ase.io import read, write
from cof_logic.cof_dftb import cif_to_hd
from cof_logic.cof_dftb import initiate_hsd_file
from cof_logic.cof_dftb import skf_files


class COFStacker:
    """ COFStacker is a Python class designed for the manipulation and management
    of atomic and lattice vector data extracted from a crystallographic
    file in the .extxyz format. It focuses on altering and stacking layer
    of Covalent-Organic Frameworks (COFs) using various stacking modes (AA and AB),
    then writing the modified atomic data back into diverse file formats.
    """

    def __init__(self, input_filename):
        if not input_filename.endswith('.extxyz'):
            input_filename += '.extxyz'
        self.input_filename = input_filename

    def extract_lattice_vectors(self, input_str):
        match = re.search(r'Lattice="([^"]+)"', input_str)
        if match is None:
            raise ValueError("No lattice vectors found in the input string")
        lattice_str = match.group(1)
        lattice_vals = list(map(float, lattice_str.split()))
        a = lattice_vals[0:3]
        b = lattice_vals[3:6]
        c = lattice_vals[6:9]
        return a, b, c

    def find_most_negative_z(self, atoms):
        return min([float(re.split(r'\s+', atom.strip())[3]) for atom in atoms])

    def find_most_positive_z(self, atoms):
        return max([float(re.split(r'\s+', atom.strip())[3]) for atom in atoms])

    def shift_atoms(self, atoms, shift_vector):
        shifted_atoms = []
        for atom in atoms:
            species, x, y, z, _ = re.split(r'\s+', atom.strip())
            x, y, z = [float(coord) for coord in (x, y, z)]
            x_new, y_new, z_new = x + shift_vector[0], y + shift_vector[1], z + shift_vector[2]
            shifted_atoms.append(f"{species} {x_new:.8f} {y_new:.8f} {z_new:.8f} 0")
        return shifted_atoms

    def shift_atoms_up(self, atoms, shift_value):
        shifted_atoms = []
        for atom in atoms:
            species, x, y, z, _ = re.split(r'\s+', atom.strip())
            x, y, z = [float(coord) for coord in (x, y, z)]
            z_new = z + shift_value
            shifted_atoms.append(f"{species} {x:.8f} {y:.8f} {z_new:.8f} 0")
        return shifted_atoms

    def create_AA_stacking(self, original_atoms, shift_value_aa):
        return original_atoms + self.shift_atoms_up(original_atoms, shift_value_aa)

    def create_AB_stacking(self, original_atoms, shift_value_ab, shift_vector):
        shifted_atoms_B = self.shift_atoms_up(original_atoms, shift_value_ab)
        shifted_atoms_B = self.shift_atoms(shifted_atoms_B, shift_vector)
        ab_stacked_atoms = original_atoms + shifted_atoms_B
        return ab_stacked_atoms

    def write_to_extxyz(self, filename, atoms, lattice_vectors):
        num_atoms = len(atoms)
        lattice_str = f'Lattice="{lattice_vectors[0][0]} {lattice_vectors[0][1]} {lattice_vectors[0][2]} ' \
                      f'{lattice_vectors[1][0]} {lattice_vectors[1][1]} {lattice_vectors[1][2]} ' \
                      f'{lattice_vectors[2][0]} {lattice_vectors[2][1]} {lattice_vectors[2][2]}" ' \
                      f'Properties=species:S:1:pos:R:3:tags:I:1 pbc="T T T"'
        with open(filename, 'w') as f:
            f.write(str(num_atoms) + "\n")
            f.write(lattice_str + "\n")
            f.write("\n".join(atoms) + "\n")

    def calculate_lattice_c(self, atoms):
        max_z = max(float(atom.split()[3]) for atom in atoms)
        return [0.0, 0.0, max_z + 1.7]
    
    def write_and_convert_to_cif(self, prefix, atoms, lattice_vectors):
        extxyz_path = os.path.join("Data", "Generated_Temp_COFextxyz", f"{prefix}.extxyz")
        cif_path = os.path.join("Data", "Generated_CIFs_Stacked", f"{prefix}.cif")

        self.write_to_extxyz(extxyz_path, atoms, lattice_vectors)

        atoms = read(extxyz_path)
        atoms.set_cell(lattice_vectors)  # Set the cell explicitly
        write(cif_path, atoms)

    def generate_dftb_files(self, cof_nr, stacking_type):
        if stacking_type not in ["AA", "AB"]:
            raise ValueError("Invalid stacking type. Must be 'AA' or 'AB'.")

        cif_file = os.path.join("Data", "Generated_CIFs_Stacked", f"COF_{cof_nr}_{stacking_type}.cif")
        dftb_cof_path = os.path.join("Data", "DFTB", f"COF_{cof_nr}_{stacking_type}")

        os.makedirs(dftb_cof_path, exist_ok=True)

        gen_file = os.path.join(dftb_cof_path, 'geo_end.gen')
        cif_to_hd(cif_file, gen_file)

        hsd_file_path = os.path.join(dftb_cof_path, 'dftb_in.hsd')
        initiate_hsd_file(gen_file, skf_files, output_filename=hsd_file_path)

    def stack_cof(self, cof_nr, input_filename=None):
        input_filename = input_filename or self.input_filename

        if not input_filename.endswith('.extxyz'):
            input_filename += '.extxyz'

        print("Input Filename:", input_filename)
        input_path = os.path.join("Data", "Generated_COFs", input_filename)

        if not os.path.exists(input_path):
            print(input_path)
            raise FileNotFoundError(f"No such file or directory: '{input_path}'")

        with open(input_path, "r") as file:
            input_str = file.read()

        a, b, c = self.extract_lattice_vectors(input_str)
        atoms = input_str.strip().split("\n")[2:]

        most_negative_z = self.find_most_negative_z(atoms)
        highest_z = self.find_most_positive_z(atoms)  # Find the highest z position

        shift_value = 0.1 - most_negative_z
        shifted_atoms = self.shift_atoms_up(atoms, shift_value)

        # Calculate shift value for stacking
        layer_height = highest_z - most_negative_z + 3

        # AA Stacking
        aa_stacked_atoms = self.create_AA_stacking(shifted_atoms, layer_height)
        c_aa = self.calculate_lattice_c(aa_stacked_atoms)
        self.write_and_convert_to_cif(f"COF_{cof_nr}_AA", aa_stacked_atoms, [a, b, c_aa])
        self.generate_dftb_files(cof_nr, "AA")

        # AB Stacking
        ab_shift_vector = [a[i] / 2 for i in range(3)]
        ab_stacked_atoms = self.create_AB_stacking(shifted_atoms, layer_height, ab_shift_vector)
        c_ab = self.calculate_lattice_c(ab_stacked_atoms)
        self.write_and_convert_to_cif(f"COF_{cof_nr}_AB", ab_stacked_atoms, [a, b, c_ab])
        self.generate_dftb_files(cof_nr, "AB")

