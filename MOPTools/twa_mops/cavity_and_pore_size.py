# NOTE script adapted by the original version provided by Dr. Aleksandar Kondinski (ak2332@cam.ac.uk)

from geo import *
import os
import csv
import math
from rdkit.Chem import GetPeriodicTable
from rdkit.Chem.rdmolfiles import MolFromXYZFile


# Define the directory paths and file names
xyz_folder = "./data/xyz_mops_new"
output_csv = "./data/xyz_mops_new/ogm_inner_diameter_new.csv"

PERIODIC_TABLE = GetPeriodicTable()

def outer_diameter(atoms: List[Point]):
    positions = np.array([atom.as_array for atom in atoms])
    distances = np.linalg.norm(positions, axis=1)
    return distances.max() * 2


def largest_inner_sphere_diameter(atoms: List[Point]):
    # find the centroid of the atoms
    centriod = Point.centroid(atoms)

    # find the atom that is closest to the centroid when subtracted covalent radius
    sorted_by_adjusted_distance = sorted(atoms, key = lambda x: centriod.get_distance_to(x) - PERIODIC_TABLE.GetRcovalent(x.label))

    # calculate the inner diameter as the adjusted distance, as well as the inner volume of the sphere
    inner_radius = sorted_by_adjusted_distance[0].get_distance_to(centriod) - PERIODIC_TABLE.GetRcovalent(sorted_by_adjusted_distance[0].label)
    inner_radius = max(0, inner_radius) # set it to 0 if it's negative (meaning the centroid is within distance of the atom's covalent radius)
    inner_diameter = inner_radius * 2
    inner_diameter_atom = sorted_by_adjusted_distance[0].label
    inner_volume = (4 / 3) * math.pi * inner_radius ** 3
    return inner_diameter_atom, inner_diameter, inner_volume


def pore_size_diameter(atoms: List[Point], probing_vector: Vector):
    # filter out the atoms that are in the positive direction of the probing vector
    postive_atoms = [a for a in atoms if a.as_array.dot(probing_vector.as_array) > 0]
    if not postive_atoms:
        return None
    # calculate the perpendicular distances of the atoms to the probing direction and sort them by the adjusted distance (subtracting covalent radius)
    sorted_by_adjusted_distance = sorted(postive_atoms, key = lambda x: x.get_distance_to_vector(probing_vector) - PERIODIC_TABLE.GetRcovalent(x.label))
    pore_size = sorted_by_adjusted_distance[0].get_distance_to_vector(probing_vector) - PERIODIC_TABLE.GetRcovalent(sorted_by_adjusted_distance[0].label)
    pore_size_diameter = max(0, pore_size) * 2 # set it to 0 if it's negative (meaning the vector is within distance of the atom's covalent radius)
    return pore_size_diameter


if __name__ == '__main__':
    with open(output_csv, mode='w', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(["File Name", "Atom", "Inner Diameter", "Volume"])

        failed = 0
        # Loop through each XYZ file in the folder
        for file_name in os.listdir(xyz_folder):
            if file_name.endswith(".xyz"):
                file_path = os.path.join(xyz_folder, file_name)

                # Read the XYZ file to find the closest atom
                mol = MolFromXYZFile(file_path)

                if mol is None:
                    failed += 1
                    csv_writer.writerow([file_name, 'failed', 'failed', 'failed'])
                else:
                    atoms = []
                    for a in mol.GetAtoms():
                        pos = mol.GetConformer().GetAtomPosition(a.GetIdx())
                        pt = Point(x=pos.x, y=pos.y, z=pos.z, label=a.GetSymbol())
                        atoms.append(pt)

                    inner_diameter_atom, inner_diameter, inner_volume = largest_inner_sphere_diameter(atoms)

                    csv_writer.writerow([file_name, inner_diameter_atom, inner_diameter, inner_volume])
                    print(f"Processed {file_name}: Closest atom = {inner_diameter_atom}, Inner diameter = {inner_diameter:.3f} Å, Volume = {inner_volume:.3f} Å³")
