# NOTE script adapted by the original version provided by Dr. Aleksandar Kondinski (ak2332@cam.ac.uk)

from geo import *
import os
import csv
import math
from rdkit.Chem import GetPeriodicTable
from rdkit.Chem.rdmolfiles import MolFromXYZFile


# Define the directory paths and file names
xyz_folder = "./data/xyz_mops_regenerate"
output_csv = "./data/xyz_mops_regenerate/ogm_inner_radius_new.csv"
covalent_radii_file = "./data/covalent_radii.csv"


def diameter_of_largest_inner_sphere(atoms: List[Point]):
    # find the closest atom to the centroid of the atoms
    centriod = Point.centroid(atoms)
    closest_atom = centriod.rank_distance_to_points(atoms)[0]

    # find those within threshold distance of (the distance of the closest atom to centroid + 0.5)
    threshold_distance = closest_atom.get_distance_to(centriod) + 0.5
    atoms_within_threshold = centriod.get_points_within_threshold_distance(atoms, threshold_distance)
    periodic_table = GetPeriodicTable()
    sorted_by_adjusted_distance = sorted(atoms_within_threshold, key=lambda x: centriod.get_distance_to(x) - periodic_table.GetRcovalent(x.label))

    # calculate the inner diameter as the adjusted distance, as well as the inner volume of the sphere
    inner_radius = sorted_by_adjusted_distance[0].get_distance_to(centriod) - periodic_table.GetRcovalent(sorted_by_adjusted_distance[0].label)
    inner_radius = max(0, inner_radius) # set it to 0 if it's negative
    inner_radius_atom = sorted_by_adjusted_distance[0].label
    inner_volume = (4 / 3) * math.pi * inner_radius ** 3
    return inner_radius_atom, inner_radius, inner_volume

if __name__ == '__main__':
    with open(output_csv, mode='w', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(["File Name", "Atom", "Inner Radius", "Volume"])

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

                    inner_radius_atom, inner_radius, inner_volume = diameter_of_largest_inner_sphere(atoms)

                    csv_writer.writerow([file_name, inner_radius_atom, inner_radius, inner_volume])
                    print(f"Processed {file_name}: Closest atom = {inner_radius_atom}, Inner radius = {inner_radius:.3f} Å, Volume = {inner_volume:.3f} Å³")
