__author__ = "Aleksandar Kondinski"
__license__ = "MIT" 
__version__ = '0.1.0' 
__status__ = "development" 

import json
import os
import math
from mop_cbu_find_distance import MoleculeAnalyzer

class AssemblyModelRescaler:

    """
    AssemblyModelRescaler tailors the size of the assembly model in a way that it fits the sizes of both chemical building unit fragments.
    """

    def __init__(self, assembly_model_path, assembly_model_key, cbu1_label, cbu2_label, cbu1_path, cbu2_path):
        self.assembly_model_path = assembly_model_path
        self.assembly_model_key = assembly_model_key
        self.cbu1_label = cbu1_label
        self.cbu2_label = cbu2_label
        self.cbu1_path = cbu1_path
        self.cbu2_path = cbu2_path

    @staticmethod
    def calculate_distance(atom1, atom2):
        dx = atom1['X'] - atom2['X']
        dy = atom1['Y'] - atom2['Y']
        dz = atom1['Z'] - atom2['Z']
        return math.sqrt(dx*dx + dy*dy + dz*dz)

    @staticmethod
    def detect_square(distances):
        unique_distances = set(distances)
        if len(unique_distances) == 2:
            side = min(unique_distances)
            diagonal = max(unique_distances)
            return side, diagonal
        return None, None

    def scale_positions(self, data, scale_factor, affected_positions):
        for position in data[self.assembly_model_key]:
            if position["Key"] in affected_positions:
                position["X"] *= scale_factor
                position["Y"] *= scale_factor
                position["Z"] *= scale_factor

    def get_target_distance(self, cbu_file_path):
        analyzer = MoleculeAnalyzer(cbu_file_path)
        avg_side_distance, avg_diagonal_distance = analyzer.process()
        if avg_side_distance is None:
            raise ValueError(f"Failed to determine target distance from {cbu_file_path}")
        return avg_side_distance, avg_diagonal_distance

    def find_affected_positions(self, data, label):
        affected_positions = []
        for position in data[self.assembly_model_key]:
            if position["Label"] == label or position["Label"] == "Dummy":
                affected_positions.append(position["Key"])
        return affected_positions

    @staticmethod
    def calculate_scale_factor(original_distance, target_distance):
        return target_distance / original_distance

    @staticmethod
    def move_group_as_plane(group, step_size, center_position, direction):
        for atom in group:
            vector = [atom['X'] - center_position['X'], atom['Y'] - center_position['Y'], atom['Z'] - center_position['Z']]
            norm = math.sqrt(sum([v**2 for v in vector]))
            unit_vector = [v / norm for v in vector]
            if direction == "away":
                atom['X'] += step_size * unit_vector[0]
                atom['Y'] += step_size * unit_vector[1]
                atom['Z'] += step_size * unit_vector[2]
            elif direction == "towards":
                atom['X'] -= step_size * unit_vector[0]
                atom['Y'] -= step_size * unit_vector[1]
                atom['Z'] -= step_size * unit_vector[2]

    @staticmethod
    def preserve_dummy_distances(group):
        center = group[0]
        dummies = group[1:]
        dummy_distances = []

        for dummy in dummies:
            distance_vector = [dummy['X'] - center['X'], dummy['Y'] - center['Y'], dummy['Z'] - center['Z']]
            dummy_distances.append(distance_vector)
        
        return dummy_distances

    @staticmethod
    def restore_dummy_distances(group, dummy_distances):
        center = group[0]
        dummies = group[1:]

        for i, dummy in enumerate(dummies):
            distance_vector = dummy_distances[i]
            dummy['X'] = center['X'] + distance_vector[0]
            dummy['Y'] = center['Y'] + distance_vector[1]
            dummy['Z'] = center['Z'] + distance_vector[2]

    @staticmethod
    def group_atoms_by_4_planar(data, assembly_model_key):
        groups = {}
        for position in data[assembly_model_key]:
            if position["Label"] == "4-planar":
                group = [position]
                for dummy_key in position["ClosestDummies"]:
                    dummy = next(pos for pos in data[assembly_model_key] if pos["Key"] == dummy_key)
                    group.append(dummy)
                groups[position["Key"]] = group
        return groups

    def shift_positions_towards_target(self, data, cbu2_label, target_cbu2_side_length, center_position):
        max_iterations = 1000
        tolerance = 1e-2
        step_size = 0.01

        groups = self.group_atoms_by_4_planar(data, self.assembly_model_key)
        dummy_distances = {key: self.preserve_dummy_distances(group) for key, group in groups.items()}

        for iteration in range(max_iterations):
            distances_met = True

            for position in data[self.assembly_model_key]:
                if position["Label"] == cbu2_label:
                    closest_dummies = position["ClosestDummies"]
                    if len(closest_dummies) == 2:
                        dummy1 = next(p for p in data[self.assembly_model_key] if p["Key"] == closest_dummies[0])
                        dummy2 = next(p for p in data[self.assembly_model_key] if p["Key"] == closest_dummies[1])
                        distance = self.calculate_distance(dummy1, dummy2)
                        
                        if distance < target_cbu2_side_length - tolerance:
                            distances_met = False
                            for key, group in groups.items():
                                self.move_group_as_plane(group, step_size, center_position, "away")
                                self.restore_dummy_distances(group, dummy_distances[key])
                        elif distance > target_cbu2_side_length + tolerance:
                            distances_met = False
                            for key, group in groups.items():
                                self.move_group_as_plane(group, step_size, center_position, "towards")
                                self.restore_dummy_distances(group, dummy_distances[key])

            if distances_met:
                print(f"Target distance {target_cbu2_side_length} met after {iteration + 1} iterations.")
                break
        else:
            print(f"Max iterations reached. Target distance {target_cbu2_side_length} not met.")

    @staticmethod
    def calculate_bond_angle(atom1, atom2, atom3):
        v1 = [atom1['X'] - atom2['X'], atom1['Y'] - atom2['Y'], atom1['Z'] - atom2['Z']]
        v2 = [atom3['X'] - atom2['X'], atom3['Y'] - atom2['Y'], atom3['Z'] - atom2['Z']]
        dot_product = sum(v1[i] * v2[i] for i in range(3))
        norm_v1 = math.sqrt(sum(v1[i]**2 for i in range(3)))
        norm_v2 = math.sqrt(sum(v2[i]**2 for i in range(3)))
        cosine_angle = dot_product / (norm_v1 * norm_v2)
        angle = math.acos(cosine_angle)
        return math.degrees(angle)

    def adjust_2_bent_positions(self, data, original_data, cbu2_positions, center_position):
        for position in data[self.assembly_model_key]:
            if position["Label"] == "2-bent":
                original_position = next(p for p in original_data[self.assembly_model_key] if p["Key"] == position["Key"])
                closest_dummies = position["ClosestDummies"]

                if len(closest_dummies) == 2:
                    dummy1 = next(p for p in data[self.assembly_model_key] if p["Key"] == closest_dummies[0])
                    dummy2 = next(p for p in data[self.assembly_model_key] if p["Key"] == closest_dummies[1])
                    original_dummy1 = next(p for p in original_data[self.assembly_model_key] if p["Key"] == closest_dummies[0])
                    original_dummy2 = next(p for p in original_data[self.assembly_model_key] if p["Key"] == closest_dummies[1])

                    original_distance = self.calculate_distance(original_dummy1, original_position)
                    target_distance = self.calculate_distance(dummy1, dummy2) / 2

                    scale_factor = target_distance / original_distance

                    position['X'] = center_position['X'] + scale_factor * (original_position['X'] - center_position['X'])
                    position['Y'] = center_position['Y'] + scale_factor * (original_position['Y'] - center_position['Y'])
                    position['Z'] = center_position['Z'] + scale_factor * (original_position['Z'] - center_position['Z'])

                    # Restore the original bond angle
                    bond_angle = self.calculate_bond_angle(original_dummy1, original_position, original_dummy2)
                    current_angle = self.calculate_bond_angle(dummy1, position, dummy2)
                    angle_difference = bond_angle - current_angle

                    # Rotate around the axis passing through the center and the 2-bent atom
                    rotation_axis = [position['X'] - center_position['X'], position['Y'] - center_position['Y'], position['Z'] - center_position['Z']]
                    norm_axis = math.sqrt(sum(v**2 for v in rotation_axis))
                    unit_axis = [v / norm_axis for v in rotation_axis]

                    theta = math.radians(angle_difference)

                    rotation_matrix = [
                        [math.cos(theta) + unit_axis[0]**2 * (1 - math.cos(theta)),
                         unit_axis[0] * unit_axis[1] * (1 - math.cos(theta)) - unit_axis[2] * math.sin(theta),
                         unit_axis[0] * unit_axis[2] * (1 - math.cos(theta)) + unit_axis[1] * math.sin(theta)],
                        [unit_axis[1] * unit_axis[0] * (1 - math.cos(theta)) + unit_axis[2] * math.sin(theta),
                         math.cos(theta) + unit_axis[1]**2 * (1 - math.cos(theta)),
                         unit_axis[1] * unit_axis[2] * (1 - math.cos(theta)) - unit_axis[0] * math.sin(theta)],
                        [unit_axis[2] * unit_axis[0] * (1 - math.cos(theta)) - unit_axis[1] * math.sin(theta),
                         unit_axis[2] * unit_axis[1] * (1 - math.cos(theta)) + unit_axis[0] * math.sin(theta),
                         math.cos(theta) + unit_axis[2]**2 * (1 - math.cos(theta))]
                    ]

                    relative_position = [position['X'] - center_position['X'], position['Y'] - center_position['Y'], position['Z'] - center_position['Z']]
                    rotated_position = [
                        sum(rotation_matrix[i][j] * relative_position[j] for j in range(3)) + center_position['X'] for i in range(3)
                    ]

                    position['X'], position['Y'], position['Z'] = rotated_position

    def workflow_operations(self, output_dir):
        print(f"Performing workflow operations on:")
        print(f"Assembly model: {self.assembly_model_path}")
        print(f"CBU 1 label: {self.cbu1_label}")
        print(f"CBU 2 label: {self.cbu2_label}")

        target_cbu1_side_length, _ = self.get_target_distance(self.cbu1_path)
        target_cbu2_side_length, _ = self.get_target_distance(self.cbu2_path)
        print(f"Target side length derived from CBU 1: {target_cbu1_side_length}")
        print(f"Target side length derived from CBU 2: {target_cbu2_side_length}")

        try:
            with open(self.assembly_model_path, 'r') as json_file:
                data = json.load(json_file)
                original_data = json.load(open(self.assembly_model_path, 'r'))
                positions = data[self.assembly_model_key]

                # Separate dummy atoms for calculation
                dummy_atoms = {pos["Key"]: pos for pos in positions if pos["Label"] == "Dummy"}
                
                # Find affected positions for each CBU
                cbu1_positions = self.find_affected_positions(data, self.cbu1_label)
                cbu2_positions = self.find_affected_positions(data, self.cbu2_label)

                current_cbu1_side_length = None

                # Process CBU1 positions to determine current side length
                for position in positions:
                    if position["Key"] in cbu1_positions and position["Label"] == self.cbu1_label:
                        closest_dummies = position["ClosestDummies"]
                        if len(closest_dummies) == 4:
                            distances = []
                            for i in range(len(closest_dummies)):
                                for j in range(i + 1, len(closest_dummies)):
                                    dummy1 = dummy_atoms[closest_dummies[i]]
                                    dummy2 = dummy_atoms[closest_dummies[j]]
                                    distance = self.calculate_distance(dummy1, dummy2)
                                    distances.append(distance)
                            current_cbu1_side_length, diagonal = self.detect_square(distances)
                            if current_cbu1_side_length and diagonal:
                                print(f"Current side length: {current_cbu1_side_length:.4f}")
                                print(f"Diagonal length: {diagonal:.4f}")
                            else:
                                print("Could not determine distinct side and diagonal lengths for a square configuration.")

                if current_cbu1_side_length:
                    scale_factor_cbu1 = self.calculate_scale_factor(current_cbu1_side_length, target_cbu1_side_length)
                    print(f"Calculated scale factor for CBU 1: {scale_factor_cbu1:.4f}")

                    # Scale the CBU1 positions and their associated dummies by the calculated scale factor
                    self.scale_positions(data, scale_factor_cbu1, cbu1_positions)

                    # Verify the distances after scaling
                    for position in positions:
                        if position["Key"] in cbu1_positions and position["Label"] == self.cbu1_label:
                            closest_dummies = position["ClosestDummies"]
                            distances = []
                            for i in range(len(closest_dummies)):
                                for j in range(i + 1, len(closest_dummies)):
                                    dummy1 = dummy_atoms[closest_dummies[i]]
                                    dummy2 = dummy_atoms[closest_dummies[j]]
                                    distance = self.calculate_distance(dummy1, dummy2)
                                    distances.append(distance)
                            current_cbu1_side_length, diagonal = self.detect_square(distances)
                            print(f"Post-scaling side length: {current_cbu1_side_length:.4f}")
                            print(f"Post-scaling diagonal length: {diagonal:.4f}")
                else:
                    print("Failed to determine the current side length, scaling for CBU 1 aborted.")

                # Check the distance between dummy atoms sharing a 2-bent center
                for position in positions:
                    if position["Label"] == self.cbu2_label:
                        closest_dummies = position["ClosestDummies"]
                        if len(closest_dummies) == 2:
                            dummy1 = dummy_atoms[closest_dummies[0]]
                            dummy2 = dummy_atoms[closest_dummies[1]]
                            distance = self.calculate_distance(dummy1, dummy2)
                            print(f"Distance between dummies sharing {position['Key']}: {distance:.4f}")
                            if distance < target_cbu2_side_length:
                                print(f"Distance is smaller than the target CBU2 side length: {target_cbu2_side_length:.4f}")
                            elif distance > target_cbu2_side_length:
                                print(f"Distance is larger than the target CBU2 side length: {target_cbu2_side_length:.4f}")
                            else:
                                print(f"Distance matches the target CBU2 side length: {target_cbu2_side_length:.4f}")

                # Save the scaled model to a new file after achieving target 1
                scaled_assembly_model_path = os.path.join(output_dir, f"{os.path.basename(self.assembly_model_path).replace('.json', '_scaled_target1.json')}")
                with open(scaled_assembly_model_path, 'w') as scaled_json_file:
                    json.dump(data, scaled_json_file, indent=4)
                print(f"Scaled assembly model saved to: {scaled_assembly_model_path}")

                # Load the scaled data to perform the second target adjustment
                with open(scaled_assembly_model_path, 'r') as scaled_json_file:
                    data = json.load(scaled_json_file)

                center_position = next(p for p in data[self.assembly_model_key] if p["Label"] == "Center")

                # Shift positions towards the target for CBU 2
                self.shift_positions_towards_target(data, self.cbu2_label, target_cbu2_side_length, center_position)

                # Save the final scaled model after achieving target 2
                final_scaled_assembly_model_path = os.path.join(output_dir, f"{os.path.basename(self.assembly_model_path).replace('.json', '_scaled_target2.json')}")
                with open(final_scaled_assembly_model_path, 'w') as final_scaled_json_file:
                    json.dump(data, final_scaled_json_file, indent=4)
                print(f"Final scaled assembly model saved to: {final_scaled_assembly_model_path}")

                # Load the final scaled data to adjust the 2-bent positions
                with open(final_scaled_assembly_model_path, 'r') as final_scaled_json_file:
                    final_data = json.load(final_scaled_json_file)

                # Adjust the 2-bent positions based on the new dummy positions
                self.adjust_2_bent_positions(final_data, original_data, cbu2_positions, center_position)

                # Save the final updated geometry as JSON
                final_updated_assembly_model_path = os.path.join(output_dir, f"{os.path.basename(self.assembly_model_path).replace('.json', '_final_scaled.json')}")
                with open(final_updated_assembly_model_path, 'w') as final_updated_json_file:
                    json.dump(final_data, final_updated_json_file, indent=4)
                print(f"Final updated assembly model saved to: {final_updated_assembly_model_path}")

        except Exception as e:
            print(f"An error occurred: {e}")
