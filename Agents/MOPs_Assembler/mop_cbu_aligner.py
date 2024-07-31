__author__ = "Aleksandar Kondinski"
__license__ = "MIT" 
__version__ = '0.1.0' 
__status__ = "development" 

import os
import json
import glob
import numpy as np
from scipy.spatial.transform import Rotation as R

position_files_pattern = 'Data/Assembly_Models/Output_Atoms/Position_*.json'
cbu2_file_path = 'Data/CBUs/CBU2.json'
output_file_template = 'Data/CBUs/CBU_{}.json'

def read_json_data(file_path):
    with open(file_path, 'r') as file:
        return json.load(file)

def save_json_data(data, file_path):
    with open(file_path, 'w') as file:
        json.dump(data, file, indent=4)

def get_vector(from_point, to_point):
    return np.array(to_point) - np.array(from_point)

def apply_translation(data, translation_vector):
    for atom in data.values():
        atom['coordinate_x'] += translation_vector[0]
        atom['coordinate_y'] += translation_vector[1]
        atom['coordinate_z'] += translation_vector[2]

def apply_rotation(data, rotation, fixed_point):
    for atom in data.values():
        pos = np.array([atom['coordinate_x'], atom['coordinate_y'], atom['coordinate_z']])
        new_pos = rotation.apply(pos - fixed_point) + fixed_point
        atom['coordinate_x'], atom['coordinate_y'], atom['coordinate_z'] = new_pos

def round_coordinates(data, decimals=5, epsilon=1e-10):
    for atom in data.values():
        atom['coordinate_x'] = round(atom['coordinate_x'], decimals) if abs(atom['coordinate_x']) > epsilon else 0.0
        atom['coordinate_y'] = round(atom['coordinate_y'], decimals) if abs(atom['coordinate_y']) > epsilon else 0.0
        atom['coordinate_z'] = round(atom['coordinate_z'], decimals) if abs(atom['coordinate_z']) > epsilon else 0.0

def calculate_center_of_mass(data):
    atom_coords = np.array([[atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z']] 
                            for atom_data in data.values()])
    center_of_mass = np.mean(atom_coords, axis=0)
    return center_of_mass

def find_optimal_rotation(X, Y):
    X_centered = X - np.mean(X, axis=0)
    Y_centered = Y - np.mean(Y, axis=0)
    H = np.dot(X_centered.T, Y_centered)
    U, S, Vt = np.linalg.svd(H)
    R = np.dot(Vt.T, U.T)
    if np.linalg.det(R) < 0:
        Vt[2, :] *= -1
        R = np.dot(Vt.T, U.T)
    return R

def calculate_dihedral(p0, p1, p2, p3):
    b0 = -1.0 * (p1 - p0)
    b1 = p2 - p1
    b2 = p3 - p2

    b1 /= np.linalg.norm(b1)

    v = b0 - np.dot(b0, b1) * b1
    w = b2 - np.dot(b2, b1) * b1

    x = np.dot(v, w)
    y = np.dot(np.cross(b1, v), w)

    return np.degrees(np.arctan2(y, x))

class CBU4PlanarProcessor:
    def __init__(self, cbu_file, position_files):
        self.cbu_data = read_json_data(cbu_file)
        self.position_files = position_files
        print(f"Loaded CBU data from {cbu_file}")

    def process(self):
        cbu_coords = np.array([[atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z']] 
                               for atom_data in self.cbu_data.values()])
        cbu_center = np.mean(cbu_coords, axis=0)
        print(f"Calculated CBU center coordinates: {cbu_center}")

        x_atoms = [atom_data for atom_data in self.cbu_data.values() if atom_data['atom'] == 'X']
        x_coords = np.array([[atom['coordinate_x'], atom['coordinate_y'], atom['coordinate_z']] for atom in x_atoms])

        for position_file in self.position_files:
            position_data = read_json_data(position_file)
            print(f"Loaded position data from {position_file}")

            planar_center = np.array([position_data['X'], position_data['Y'], position_data['Z']])
            print(f"4-planar center coordinates: {planar_center}")

            translation_vector = planar_center - cbu_center
            print(f"Calculated translation vector: {translation_vector}")

            translated_cbu_data = {}
            for atom_id, atom_data in self.cbu_data.items():
                original_coords = np.array([atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z']])
                translated_coords = original_coords + translation_vector
                translated_cbu_data[atom_id] = {
                    "atom": atom_data['atom'],
                    "coordinate_x": translated_coords[0],
                    "coordinate_y": translated_coords[1],
                    "coordinate_z": translated_coords[2],
                    "bond": atom_data['bond'],
                    "mmtype": atom_data['mmtype'],
                    "qmmm": atom_data['qmmm']
                }

            dummy_coords = np.array([position_data['ClosestDummies'][key] for key in position_data['ClosestDummies']])
            optimal_rotation_matrix = find_optimal_rotation(x_coords, dummy_coords)
            print(f"Calculated optimal rotation matrix:\n{optimal_rotation_matrix}")

            for atom_id, atom_data in translated_cbu_data.items():
                original_coords = np.array([atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z']])
                rotated_coords = np.dot(optimal_rotation_matrix, original_coords - planar_center) + planar_center
                translated_cbu_data[atom_id]['coordinate_x'] = rotated_coords[0]
                translated_cbu_data[atom_id]['coordinate_y'] = rotated_coords[1]
                translated_cbu_data[atom_id]['coordinate_z'] = rotated_coords[2]
                print(f"Rotated atom {atom_id} to {rotated_coords}")

            output_file_path = position_file.replace('GBU_Positions', 'Translated_CBUs').replace('Position_', 'Translated_CBU1_Position_')
            os.makedirs(os.path.dirname(output_file_path), exist_ok=True)
            with open(output_file_path, 'w') as f:
                json.dump(translated_cbu_data, f, indent=4)
                print(f"Translated and rotated CBU saved to {output_file_path}")

class CBU2BentProcessor:
    def __init__(self, cbu_file, position_files):
        self.cbu_file = cbu_file
        self.position_files = position_files

    def process(self):
        for position_file in self.position_files:
            position_data = read_json_data(position_file)
            cbu2_data = read_json_data(self.cbu_file)
            print(f"Processing {position_file}")
            self.process_position(position_data, cbu2_data, position_file)

    def process_position(self, position_data, cbu2_data, position_file_path):
        cbu2_data, keys = self.translate_and_align(position_data, cbu2_data)
        if cbu2_data is None:
            return
        
        cbu2_data = self.calculate_and_add_center_of_mass(cbu2_data)
        cbu2_data = self.adjust_rotation_to_minimize_distance(position_data, cbu2_data, keys)
        
        output_file_path = position_file_path.replace('GBU_Positions', 'Translated_CBUs').replace('Position_', 'Translated_CBU2_Position_')
        os.makedirs(os.path.dirname(output_file_path), exist_ok=True)
        round_coordinates(cbu2_data, decimals=5, epsilon=1e-10)
        with open(output_file_path, 'w') as f:
            json.dump(cbu2_data, f, indent=4)
            print(f"Final aligned geometry saved in '{output_file_path}'.")
            
    def translate_and_align(self, position_data, cbu2_data):
        dummy_positions = list(position_data["ClosestDummies"].values())
        if len(dummy_positions) < 2:
            print(f"Error: Not enough dummy positions found in {position_data['Key']}.")
            return None, None
        
        dummy_1_coords = np.array(dummy_positions[0])
        dummy_21_coords = np.array(dummy_positions[1])
        
        print(f"Processing {position_data['Key']}...")
        print(f"Dummy_1 coordinates: {dummy_1_coords}")
        print(f"Dummy_21 coordinates: {dummy_21_coords}")
        
        x_atoms = {key: val for key, val in cbu2_data.items() if val['atom'] == 'X'}
        if len(x_atoms) < 2:
            print(f"Error: Not enough 'X' atoms found in {position_data['Key']}.")
            return None, None
        
        first_x_key, second_x_key = list(x_atoms.keys())[:2]
        first_x_atom = x_atoms[first_x_key]
        second_x_atom = x_atoms[second_x_key]
        
        first_x_coords = np.array([first_x_atom['coordinate_x'], first_x_atom['coordinate_y'], first_x_atom['coordinate_z']])
        second_x_coords = np.array([second_x_atom['coordinate_x'], second_x_atom['coordinate_y'], second_x_atom['coordinate_z']])
        
        translation_vector = dummy_1_coords - first_x_coords
        
        print(f"First X atom original coordinates: {first_x_coords}")
        print(f"Second X atom original coordinates: {second_x_coords}")
        print(f"Translation vector: {translation_vector}")
        
        apply_translation(cbu2_data, translation_vector)
        
        first_x_coords_translated = first_x_coords + translation_vector
        second_x_coords_translated = second_x_coords + translation_vector
        
        print(f"First X atom translated coordinates: {first_x_coords_translated}")
        print(f"Second X atom translated coordinates: {second_x_coords_translated}")
        
        initial_vector = second_x_coords_translated - dummy_1_coords
        target_vector = dummy_21_coords - dummy_1_coords
        
        print(f"Initial vector (Dummy_1 to Second X): {initial_vector}")
        print(f"Target vector (Dummy_1 to Dummy_21): {target_vector}")
        
        initial_vector_normalized = initial_vector / np.linalg.norm(initial_vector)
        target_vector_normalized = target_vector / np.linalg.norm(target_vector)
        
        rotation_axis = np.cross(initial_vector_normalized, target_vector_normalized)
        rotation_angle = np.arccos(np.dot(initial_vector_normalized, target_vector_normalized))
        
        print(f"Rotation axis: {rotation_axis}")
        print(f"Rotation angle (radians): {rotation_angle}")
        
        if np.linalg.norm(rotation_axis) > 0:
            rotation = R.from_rotvec(rotation_angle * rotation_axis / np.linalg.norm(rotation_axis))
        else:
            rotation = R.from_euler('z', 0)
        
        apply_rotation(cbu2_data, rotation, dummy_1_coords)
        
        second_x_coords_rotated = rotation.apply(second_x_coords_translated - dummy_1_coords) + dummy_1_coords
        
        print(f"Second X atom new coordinates after rotation: {second_x_coords_rotated}")
        print(f"Distance from Second X atom to Dummy_21 after rotation: {np.linalg.norm(second_x_coords_rotated - dummy_21_coords)}")
        
        return cbu2_data, (first_x_key, second_x_key)

    def calculate_and_add_center_of_mass(self, cbu2_data):
        center_atom = next((data for key, data in cbu2_data.items() if data["atom"] == "CENTER"), None)
        if center_atom is None:
            center_of_mass = calculate_center_of_mass(cbu2_data)
            cbu2_data["CENTER"] = {
                "atom": "CENTER",
                "coordinate_x": center_of_mass[0],
                "coordinate_y": center_of_mass[1],
                "coordinate_z": center_of_mass[2],
                "bond": [],
                "mmtype": "C_R",
                "qmmm": "MM"
            }
            center_atom = cbu2_data["CENTER"]
            print(f"Calculated center of mass at: {center_of_mass}")
        else:
            center_of_mass = np.array([center_atom["coordinate_x"], center_atom["coordinate_y"], center_atom["coordinate_z"]])
            print(f"Existing CENTER atom coordinates: {center_of_mass}")
        
        return cbu2_data

    def adjust_rotation_to_minimize_distance(self, position_data, cbu2_data, keys):
        first_x_key, second_x_key = keys
        first_x_atom = cbu2_data[first_x_key]
        second_x_atom = cbu2_data[second_x_key]
        center_atom = cbu2_data["CENTER"]

        first_x_coords = np.array([first_x_atom['coordinate_x'], first_x_atom['coordinate_y'], first_x_atom['coordinate_z']])
        second_x_coords = np.array([second_x_atom['coordinate_x'], second_x_atom['coordinate_y'], second_x_atom['coordinate_z']])
        center_coords = np.array([center_atom['coordinate_x'], center_atom['coordinate_y'], center_atom['coordinate_z']])
        target_coords = np.array([position_data["X"], position_data["Y"], position_data["Z"]])

        midi_coords = (first_x_coords + second_x_coords) / 2.0

        rotation_axis = get_vector(first_x_coords, second_x_coords)
        rotation_axis /= np.linalg.norm(rotation_axis)

        min_distance = float('inf')
        optimal_rotation = None

        for angle in range(0, 360):
            rotation = R.from_rotvec(np.radians(angle) * rotation_axis)
            rotated_center = rotation.apply(center_coords - midi_coords) + midi_coords
            distance = np.linalg.norm(rotated_center - target_coords)

            if distance < min_distance:
                min_distance = distance
                optimal_rotation = rotation

        for key, atom in cbu2_data.items():
            if key not in [first_x_key, second_x_key, 'CENTER']:
                pos = np.array([atom['coordinate_x'], atom['coordinate_y'], atom['coordinate_z']])
                new_pos = optimal_rotation.apply(pos - midi_coords) + midi_coords
                atom['coordinate_x'], atom['coordinate_y'], atom['coordinate_z'] = new_pos

        rotated_center = optimal_rotation.apply(center_coords - midi_coords) + midi_coords
        center_atom['coordinate_x'], center_atom['coordinate_y'], center_atom['coordinate_z'] = rotated_center

        print(f"Optimal rotation angle: {angle} degrees")
        print(f"Distance from CENTER to 2-bent after rotation: {min_distance}")

        return cbu2_data

def main():
    cbu1_processor = CBU4PlanarProcessor('Data/CBUs/CBU1.json', [f'Data/Assembly_Models/Output_Atoms/Position_{i}.json' for i in range(1, 7)])
    cbu1_processor.process()

    cbu2_processor = CBU2BentProcessor('Data/CBUs/CBU2.json', [f'Data/Assembly_Models/Output_Atoms/Position_{i}.json' for i in range(7, 19)])
    cbu2_processor.process()

if __name__ == "__main__":
    main()
