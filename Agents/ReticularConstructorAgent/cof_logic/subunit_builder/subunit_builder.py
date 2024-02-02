__author__ = "Aleksandar Kondinski"
__license__ = "MIT" 
__version__ = '0.1.0' 
__status__ = "development" 

import json
import numpy as np
import os
import glob
import logging
import copy

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class SubunitBuilder:

    def update_atom_bonds(self, lfr_atoms, old_uuid, new_uuid):
        # Update bonds in all atoms referencing the old UUID
        for atom_data in lfr_atoms.values():
            if 'bond' in atom_data:
                for bond in atom_data['bond']:
                    if bond['to_atom'] == old_uuid:
                        bond['to_atom'] = new_uuid
                        
    def rotation_matrix_from_vectors(self, vec1, vec2):
        """Create a rotation matrix that aligns vec1 to vec2"""
        a, b = (vec1 / np.linalg.norm(vec1)).reshape(3), (vec2 / np.linalg.norm(vec2)).reshape(3)
        v = np.cross(a, b)
        c = np.dot(a, b)
        s = np.linalg.norm(v)
        kmat = np.array([[0, -v[2], v[1]], [v[2], 0, -v[0]], [-v[1], v[0], 0]])
        rotation_matrix = np.eye(3) + kmat + kmat.dot(kmat) * ((1 - c) / (s ** 2))
        return rotation_matrix

    def substitute_atom_properties(self, lfr_atoms, target_atom_uuid, reference_atom_data, x_outer_uuid):
        # 1. Shift all atoms to maintain relative positions
        old_coordinates = np.array([
            lfr_atoms[target_atom_uuid]['coordinate_x'],
            lfr_atoms[target_atom_uuid]['coordinate_y'],
            lfr_atoms[target_atom_uuid]['coordinate_z']
        ])
        new_coordinates = np.array([
            reference_atom_data['coordinate_x'],
            reference_atom_data['coordinate_y'],
            reference_atom_data['coordinate_z']
        ])
        shift = new_coordinates - old_coordinates

        for uuid, atom_data in lfr_atoms.items():
            atom_data['coordinate_x'] += shift[0]
            atom_data['coordinate_y'] += shift[1]
            atom_data['coordinate_z'] += shift[2]

        # 2. Compute the rotation to align x_outer_uuid with the center 000 and target_atom
        original_vector = np.array([
            lfr_atoms[x_outer_uuid]['coordinate_x'] - reference_atom_data['coordinate_x'],
            lfr_atoms[x_outer_uuid]['coordinate_y'] - reference_atom_data['coordinate_y'],
            lfr_atoms[x_outer_uuid]['coordinate_z'] - reference_atom_data['coordinate_z']
        ])
        target_vector = new_coordinates - np.array([0, 0, 0])
        rotation_matrix = self.rotation_matrix_from_vectors(original_vector, target_vector)

        # 3. Rotate all atoms except the target_atom
        for uuid, atom_data in lfr_atoms.items():
            if uuid != target_atom_uuid:  # Exclude only the target_atom_uuid
                atom_coords = np.array([atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z']])
                rotated_coords = rotation_matrix.dot(atom_coords - new_coordinates) + new_coordinates
                atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z'] = rotated_coords

        # 4. Substitute the properties of the target atom with those of the reference atom
        for key, value in reference_atom_data.items():
            if key not in ['atom', 'uuid', 'bond']:
                lfr_atoms[target_atom_uuid][key] = value

        # 5. Save/update the bonds
        if 'bond' in reference_atom_data:
            if 'bond' not in lfr_atoms[target_atom_uuid]:
                lfr_atoms[target_atom_uuid]['bond'] = []
            for bond in reference_atom_data['bond']:
                new_bond = bond.copy()
                new_bond['to_atom'] = target_atom_uuid
                lfr_atoms[target_atom_uuid]['bond'].append(new_bond)

        return lfr_atoms

    def substitute_atom_properties_2(self, lfr_json_file_path, x_atom_uuid_to_remove, reference_atom_data, x_outer_uuid, target_atom_uuid_1, target_atom_uuid_2, neighboor_atoms, core_json_path, ref_atom_uuid):
        
        lfr_atoms = self.read_json_file(lfr_json_file_path)
        
        # 1. Shift all atoms to maintain relative positions
        neighboor__atom_uuid_1 = neighboor_atoms[0]
        neighboor__atom_uuid_2 = neighboor_atoms[1]
        core_atoms = self.read_json_file(core_json_path)
        
        old_coordinates = np.array([
            lfr_atoms[x_atom_uuid_to_remove]['coordinate_x'],
            lfr_atoms[x_atom_uuid_to_remove]['coordinate_y'],
            lfr_atoms[x_atom_uuid_to_remove]['coordinate_z']
        ])
        new_coordinates = np.array([
            reference_atom_data['coordinate_x'],
            reference_atom_data['coordinate_y'],
            reference_atom_data['coordinate_z']
        ])
        shift = new_coordinates - old_coordinates
        
        for uuid, atom_data in lfr_atoms.items():
            atom_data['coordinate_x'] += shift[0]
            atom_data['coordinate_y'] += shift[1]
            atom_data['coordinate_z'] += shift[2]

        # 2. Compute the rotation to align x_outer_uuid with the center 000 and target_atom
        original_vector = np.array([
            lfr_atoms[x_outer_uuid]['coordinate_x'] - reference_atom_data['coordinate_x'],
            lfr_atoms[x_outer_uuid]['coordinate_y'] - reference_atom_data['coordinate_y'],
            lfr_atoms[x_outer_uuid]['coordinate_z'] - reference_atom_data['coordinate_z']
        ])
        
        target_vector = new_coordinates - np.array([0, 0, 0])
        rotation_matrix = self.rotation_matrix_from_vectors(original_vector, target_vector)

        # 3. Rotate all atoms except the target_atom
        for uuid, atom_data in lfr_atoms.items():
            if uuid != x_atom_uuid_to_remove:  # Exclude only the x_atom_uuid_to_remove
                atom_coords = np.array([atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z']])
                rotated_coords = rotation_matrix.dot(atom_coords - new_coordinates) + new_coordinates
                atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z'] = rotated_coords
        
        
        # 4. Substitute the properties of the target atom with those of the reference atom
        for key, value in reference_atom_data.items():
            if key not in ['atom', 'uuid', 'bond']:
                lfr_atoms[x_atom_uuid_to_remove][key] = value

        # 5. Save/update the bonds
        if 'bond' in reference_atom_data:
            if 'bond' not in lfr_atoms[x_atom_uuid_to_remove]:
                lfr_atoms[x_atom_uuid_to_remove]['bond'] = []
            for bond in reference_atom_data['bond']:
                new_bond = bond.copy()
                new_bond['to_atom'] = x_atom_uuid_to_remove
                lfr_atoms[x_atom_uuid_to_remove]['bond'].append(new_bond)

        # Remove ref_atom_uuid from core_atoms
        if ref_atom_uuid in core_atoms:
            del core_atoms[ref_atom_uuid]

        # Update bonding information in other atoms
        for atom_data in core_atoms.values():
            if 'bond' in atom_data:
                atom_data['bond'] = [bond for bond in atom_data['bond'] if bond['to_atom'] != ref_atom_uuid]

        def euclidean_distance(coord1, coord2):
            return np.sqrt(np.sum((np.array(coord1) - np.array(coord2))**2))

        # Calculate distances
        distance_1 = euclidean_distance(
            [lfr_atoms[target_atom_uuid_1]['coordinate_x'], lfr_atoms[target_atom_uuid_1]['coordinate_y'], lfr_atoms[target_atom_uuid_1]['coordinate_z']],
            [core_atoms[neighboor__atom_uuid_1]['coordinate_x'], core_atoms[neighboor__atom_uuid_1]['coordinate_y'], core_atoms[neighboor__atom_uuid_1]['coordinate_z']]
        )
        distance_2 = euclidean_distance(
            [lfr_atoms[target_atom_uuid_1]['coordinate_x'], lfr_atoms[target_atom_uuid_1]['coordinate_y'], lfr_atoms[target_atom_uuid_1]['coordinate_z']],
            [core_atoms[neighboor__atom_uuid_2]['coordinate_x'], core_atoms[neighboor__atom_uuid_2]['coordinate_y'], core_atoms[neighboor__atom_uuid_2]['coordinate_z']]
        )

        # Conditional update of bonding information
        if distance_1 > distance_2:
            # Update bonds for lfr_atoms[target_atom_uuid_1] and core_atoms[neighboor__atom_uuid_1]
            lfr_atoms[target_atom_uuid_1]['bond'] = [{'to_atom': neighboor__atom_uuid_1, 'bond_order': 1.5}]
            core_atoms[neighboor__atom_uuid_1]['bond'] = [{'to_atom': target_atom_uuid_1, 'bond_order': 1.5}]

            # Update bonds for lfr_atoms[target_atom_uuid_2] and core_atoms[neighboor__atom_uuid_2]
            lfr_atoms[target_atom_uuid_2]['bond'] = [{'to_atom': neighboor__atom_uuid_2, 'bond_order': 1.5}]
            core_atoms[neighboor__atom_uuid_2]['bond'] = [{'to_atom': target_atom_uuid_2, 'bond_order': 1.5}]
        else:
            # Update bonds for lfr_atoms[target_atom_uuid_1] and core_atoms[neighboor__atom_uuid_2]
            lfr_atoms[target_atom_uuid_1]['bond'] = [{'to_atom': neighboor__atom_uuid_2, 'bond_order': 1.5}]
            core_atoms[neighboor__atom_uuid_2]['bond'] = [{'to_atom': target_atom_uuid_1, 'bond_order': 1.5}]

            # Update bonds for lfr_atoms[target_atom_uuid_2] and core_atoms[neighboor__atom_uuid_1]
            lfr_atoms[target_atom_uuid_2]['bond'] = [{'to_atom': neighboor__atom_uuid_1, 'bond_order': 1.5}]
            core_atoms[neighboor__atom_uuid_1]['bond'] = [{'to_atom': target_atom_uuid_2, 'bond_order': 1.5}]

        if ref_atom_uuid in core_atoms:
            del core_atoms[ref_atom_uuid]

        # Update bonding information in other atoms
        for atom_data in core_atoms.values():
            if 'bond' in atom_data:
                atom_data['bond'] = [bond for bond in atom_data['bond'] if bond['to_atom'] != ref_atom_uuid]

        # Remove x_atom_uuid_to_remove from core_atoms
        if x_atom_uuid_to_remove in lfr_atoms:
            del lfr_atoms[x_atom_uuid_to_remove]

        # Update bonding information in other atoms
        for atom_data in lfr_atoms.values():
            if 'bond' in atom_data:
                atom_data['bond'] = [bond for bond in atom_data['bond'] if bond['to_atom'] != x_atom_uuid_to_remove]
        
        lfr_atoms_copy = copy.deepcopy(lfr_atoms)
        print('------------------BEFORE-ROTATION-----------------')
        print(lfr_atoms_copy)
        print('------------------BEFORE-ROTATION-----------------')
        target_atom_1 = lfr_atoms_copy[target_atom_uuid_1]
        target_atom_2 = lfr_atoms_copy[target_atom_uuid_2]
        neighboor_atom_1 = core_atoms[neighboor__atom_uuid_1]
        neighboor_atom_2 = core_atoms[neighboor__atom_uuid_2]
        x_outer_atom = lfr_atoms_copy[x_outer_uuid]

        midpoint = ((target_atom_1['coordinate_x'] + target_atom_2['coordinate_x']) / 2,
                    (target_atom_1['coordinate_y'] + target_atom_2['coordinate_y']) / 2,
                    (target_atom_1['coordinate_z'] + target_atom_2['coordinate_z']) / 2)
        points = []
        points = self.cone_function(target_atom_1, midpoint, x_outer_atom)
        
        angle_of_rotation = None
        closest_point, angle_of_rotation = self.find_closest_point_and_angle(points, neighboor_atom_1, target_atom_1, target_atom_2, neighboor_atom_2, midpoint)
        #print(closest_point)
        print(angle_of_rotation)
        rotated_atoms = self.rotate_atoms_2(lfr_atoms_copy, midpoint, x_outer_atom, angle_of_rotation)
        #print(rotated_atoms)
        
        for atom_uuid, updated_atom_info in rotated_atoms.items():
            # Check if this atom exists in the original dictionary
            if atom_uuid in lfr_atoms_copy:
                # Update only the coordinate values for this atom
                lfr_atoms_copy[atom_uuid]['coordinate_x'] = updated_atom_info.get('coordinate_x', lfr_atoms[atom_uuid]['coordinate_x'])
                lfr_atoms_copy[atom_uuid]['coordinate_y'] = updated_atom_info.get('coordinate_y', lfr_atoms[atom_uuid]['coordinate_y'])
                lfr_atoms_copy[atom_uuid]['coordinate_z'] = updated_atom_info.get('coordinate_z', lfr_atoms[atom_uuid]['coordinate_z'])
            else:
                # If the atom does not exist in the original dictionary, add it (optional, based on your needs)
                lfr_atoms_copy[atom_uuid] = updated_atom_info
        
        self.write_json_file(core_json_path, core_atoms)
        self.write_json_file(lfr_json_file_path, lfr_atoms_copy)
        print('------------------AFTER-ROTATION-----------------')
        print(lfr_atoms_copy)
        print('------------------AFTER-ROTATION-----------------')

    def find_bs_and_update(self, json_file_path, ref_atom_uuid, ref_atom_data, bs_type, core_json_path):
        target_atoms_scenario_1 = {
            "MDNH2": "N", "MDCO": "C", "NHOH": "N", "C3X3CH3": "C",
            "CCN": "C", "ter-C": "C", "CO2R": "C", "MDOH": "O",
            "MDCOCl": "C", "CNRC": "N", "BDBOH": "B", 
        }
        target_atoms_scenario_2 = {
            "BDNH2": [["N","N"]],"BDO": [["N","N"],["O","O"]],"BDOH": [["N","N"],["O","O"]] 
            , "BDOHNH2": [["O","N"]],"OCOCO": [["C","C"]]}

        try:
            if bs_type in target_atoms_scenario_1:
                target_atom = target_atoms_scenario_1[bs_type]
                self.process_scenario_1(json_file_path, ref_atom_uuid, ref_atom_data, target_atom)
            elif bs_type in target_atoms_scenario_2:
                target_atoms = target_atoms_scenario_2[bs_type]
                self.process_scenario_2(json_file_path, ref_atom_uuid, ref_atom_data, target_atoms, core_json_path)
            
            else:
                logging.warning(f"Unsupported bs_type: {bs_type}")
        except Exception as e:
            logging.error(f"An error occurred: {e}")

    def process_scenario_1(self, json_file_path, ref_atom_uuid, ref_atom_data, target_atom):
        lfr_atoms = self.read_json_file(json_file_path)
        x_atom_uuid_to_remove, target_atom_uuid, x_outer_uuid = self.find_target_atom(lfr_atoms, target_atom)

        if x_atom_uuid_to_remove and target_atom_uuid:
            self.process_atoms_1(lfr_atoms, target_atom_uuid, ref_atom_data, x_outer_uuid, ref_atom_uuid, x_atom_uuid_to_remove)
            self.write_json_file(json_file_path, lfr_atoms)
            logging.info(f"Updated {json_file_path} with reference atom {ref_atom_uuid}")
        else:
            logging.warning("Required atoms not found.")


    def process_atoms_1(self, lfr_atoms, target_atom_uuid, ref_atom_data, x_outer_uuid, ref_atom_uuid, x_atom_uuid_to_remove):

        # Substitute properties without changing UUIDs
        lfr_atoms = self.substitute_atom_properties(lfr_atoms, target_atom_uuid, ref_atom_data, x_outer_uuid)
        # Update all bonds referencing the target atom UUID
        self.update_atom_bonds(lfr_atoms, target_atom_uuid, ref_atom_uuid)
        
        # Assign the target atom's data to the reference UUID
        lfr_atoms[ref_atom_uuid] = lfr_atoms.pop(target_atom_uuid)
        
        # Remove 'X' atom
        del lfr_atoms[x_atom_uuid_to_remove]
        
        # Remove corresponding bond in target atom
        bonds = lfr_atoms[ref_atom_uuid].get('bond', [])
        lfr_atoms[ref_atom_uuid]['bond'] = [bond for bond in bonds if bond['to_atom'] != x_atom_uuid_to_remove]

        # Return the modified atoms data
        return lfr_atoms
  
    def process_scenario_2(self, lfr_json_file_path, ref_atom_uuid, ref_atom_data, target_atoms, core_json_path):
        for target_atom_pair in target_atoms:
            try:
                target_atom_1, target_atom_2 = target_atom_pair
                #print("Processing with target atoms:", target_atom_1, target_atom_2)

                neighboor_atoms = self.find_neigboors(core_json_path, ref_atom_uuid)
                lfr_atoms_from_file = self.read_json_file(lfr_json_file_path)
                
                # Find target atoms
                x_atom_uuid_to_remove, target_atom_uuid_1, target_atom_uuid_2, x_outer_uuid = self.find_target_atoms(lfr_atoms_from_file, target_atom_1, target_atom_2)

                # Process if all required atoms are found
                if x_atom_uuid_to_remove is not None and target_atom_uuid_1 is not None and target_atom_uuid_2 is not None:
                    self.substitute_atom_properties_2(lfr_json_file_path, x_atom_uuid_to_remove, ref_atom_data, x_outer_uuid, target_atom_uuid_1, target_atom_uuid_2, neighboor_atoms, core_json_path, ref_atom_uuid)
                    #self.process_atoms_2(lfr_json_file_path, target_atom_uuid_1, target_atom_uuid_2, ref_atom_data, x_outer_uuid, ref_atom_uuid, x_atom_uuid_to_remove, neighboor_atoms, core_json_path)
                    logging.info(f"Updated {lfr_json_file_path} with reference atom {ref_atom_uuid}")
                    break  # Exit loop if processing is successful
                else:
                    logging.warning("Required atoms not found for current pair, trying next pair if available.")

            except Exception as e:
                logging.error(f"An error occurred with pair {target_atom_1}, {target_atom_2}: {e}")
                # Continue to the next pair
          
 
    def rotate_atoms_2(self, lfr_atoms_new, midpoint, x_outer_atom, angle_of_rotation_degrees):
        if angle_of_rotation_degrees is None:
        # Handle the None case appropriately, e.g., return the atoms unmodified or raise a more specific error
            raise ValueError("angle_of_rotation_degrees cannot be None")
        
        midpoint_pos = np.array(midpoint)
        x_outer_pos = np.array([x_outer_atom['coordinate_x'], x_outer_atom['coordinate_y'], x_outer_atom['coordinate_z']])

        # Calculate the virtual axis (unit vector) for rotation
        rotation_axis = x_outer_pos - midpoint_pos
        rotation_axis /= np.linalg.norm(rotation_axis)

        # Convert angle from degrees to radians
        angle = np.deg2rad(angle_of_rotation_degrees)

        # Define the rotation matrix around the axis (Rodrigues' rotation formula)
        def rotation_matrix(axis, theta):
            axis = axis / np.sqrt(np.dot(axis, axis))
            a = np.cos(theta / 2.0)
            b, c, d = -axis * np.sin(theta / 2.0)
            aa, bb, cc, dd = a * a, b * b, c * c, d * d
            bc, ad, ac, ab, bd, cd = b * c, a * d, a * c, a * b, b * d, c * d
            return np.array([[aa + bb - cc - dd, 2 * (bc + ad), 2 * (bd - ac)],
                            [2 * (bc - ad), aa + cc - bb - dd, 2 * (cd + ab)],
                            [2 * (bd + ac), 2 * (cd - ab), aa + dd - bb - cc]])

        # Rotate all atoms in lfr_atoms_new
        rotated_atoms = {}
        for atom_uuid, atom_data in lfr_atoms_new.items():
            atom_pos = np.array([atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z']])
            # Shift atom position relative to midpoint for rotation
            atom_pos -= midpoint_pos
            # Apply rotation
            atom_pos_rotated = np.dot(rotation_matrix(rotation_axis, angle), atom_pos)
            # Shift back after rotation
            atom_pos_rotated += midpoint_pos
            # Update atom data with new coordinates
            rotated_atoms[atom_uuid] = {'coordinate_x': atom_pos_rotated[0], 'coordinate_y': atom_pos_rotated[1], 'coordinate_z': atom_pos_rotated[2]}

        return rotated_atoms



    def find_closest_point_and_angle(self, points, neighbor_atom_1, target_atom_1, target_atom_2, neighbor_atom_2, midpoint):

        # Convert atoms' coordinates to numpy arrays
        neighbor_atom_1_pos = np.array([neighbor_atom_1['coordinate_x'], neighbor_atom_1['coordinate_y'], neighbor_atom_1['coordinate_z']])
        target_atom_1_pos = np.array([target_atom_1['coordinate_x'], target_atom_1['coordinate_y'], target_atom_1['coordinate_z']])
        target_atom_2_pos = np.array([target_atom_2['coordinate_x'], target_atom_2['coordinate_y'], target_atom_2['coordinate_z']])
        neighbor_atom_2_pos = np.array([neighbor_atom_2['coordinate_x'], neighbor_atom_2['coordinate_y'], neighbor_atom_2['coordinate_z']])
        midpoint = np.array(midpoint)
        #midpoint_pos = np.array([midpoint['coordinate_x'], midpoint['coordinate_y'], midpoint['coordinate_z']])
        
        closest_point = None
        min_dihedral_diff = np.inf
        angle_of_rotation_degrees = None
        
        point_prime = None
        
        for point in points:
            point_prime = 2 * midpoint - point

            # Calculate vectors
            v1 = point - point_prime
            v2 = neighbor_atom_1_pos - point
            v3 = neighbor_atom_2_pos - neighbor_atom_1_pos
            
            # Calculate dihedral angle
            dihedral_angle = self.calculate_dihedral_angle(v1, v2, v3)
            dihedral_angle_long = self.calculate_dihedral_angle(v1, v2, v3)
            
           
            # Calculate the difference from zero (target dihedral angle)
            dihedral_diff = np.abs(np.degrees(dihedral_angle_long))

            if dihedral_diff < min_dihedral_diff:
                min_dihedral_diff = dihedral_diff
                closest_point = point
        
        print (min_dihedral_diff)    
        if closest_point is not None:
            
            vec_a = midpoint - target_atom_1_pos
            vec_b = closest_point - midpoint

            cos_theta = np.dot(vec_a, vec_b) / (np.linalg.norm(vec_a) * np.linalg.norm(vec_b))
            angle_of_rotation_radians = np.arccos(np.clip(cos_theta, -1.0, 1.0))  # Clipping for numerical stability
            angle_of_rotation_degrees = np.degrees(angle_of_rotation_radians)
        else:
            angle_of_rotation_degrees = 0  # or appropriate fallback
        
        print(angle_of_rotation_degrees)    
        return closest_point, angle_of_rotation_degrees


    def calculate_dihedral_angle(self, v1, v2, v3):
        n1 = np.cross(v1, v2)
        n2 = np.cross(v2, v3)
        n1n2_cross = np.cross(n1, n2)
        angle = np.arctan2(np.linalg.norm(v2) * np.dot(v2, n1n2_cross) / np.linalg.norm(v2), np.dot(n1, n2))
        return angle
    
    def calculate_dihedral_angle_long(self, v1, v2, v3):
        # Normal of plane formed by v1, v2
        n1 = np.cross(v1, v2)
        n1 /= np.linalg.norm(n1)
        
        # Normal of plane formed by v2, v3
        n2 = np.cross(v2, v3)
        n2 /= np.linalg.norm(n2)
        
        # Unit vector orthogonal to v2
        u = v2 / np.linalg.norm(v2)
        
        # Angle between n1 and n2
        x = np.dot(n1, n2)
        y = np.dot(np.cross(n1, n2), u)
        
        return np.arctan2(y, x)
    
    def cone_function(self, target_atom_1, midpoint, x_outer_atom):
        # Convert dictionary values to numpy arrays for easier calculations
        target_atom_1_pos = np.array([target_atom_1['coordinate_x'], target_atom_1['coordinate_y'], target_atom_1['coordinate_z']])
        midpoint_pos = np.array(midpoint)
        x_outer_pos = np.array([x_outer_atom['coordinate_x'], x_outer_atom['coordinate_y'], x_outer_atom['coordinate_z']])
        # Calculate the radius of the cone's base
        radius = np.linalg.norm(target_atom_1_pos - midpoint_pos)

        # Calculate the normal vector of the cone's base
        cone_axis = midpoint_pos - x_outer_pos
        cone_axis_normalized = cone_axis / np.linalg.norm(cone_axis)
        
        # Initialize the first point on the circle
        first_point = target_atom_1_pos - midpoint_pos
        first_point_normalized = first_point / np.linalg.norm(first_point)
        
        # Calculate points around the base of the cone
        degrees = 5
        angle_rad = np.deg2rad(degrees)
        points = []
        for i in range(0, 360, degrees):
            # Calculate rotation matrix around the cone axis
            cos_angle = np.cos(angle_rad * i)
            sin_angle = np.sin(angle_rad * i)
            rotation_matrix = np.array([
                [cos_angle + cone_axis_normalized[0]**2 * (1 - cos_angle), cone_axis_normalized[0] * cone_axis_normalized[1] * (1 - cos_angle) - cone_axis_normalized[2] * sin_angle, cone_axis_normalized[0] * cone_axis_normalized[2] * (1 - cos_angle) + cone_axis_normalized[1] * sin_angle],
                [cone_axis_normalized[1] * cone_axis_normalized[0] * (1 - cos_angle) + cone_axis_normalized[2] * sin_angle, cos_angle + cone_axis_normalized[1]**2 * (1 - cos_angle), cone_axis_normalized[1] * cone_axis_normalized[2] * (1 - cos_angle) - cone_axis_normalized[0] * sin_angle],
                [cone_axis_normalized[2] * cone_axis_normalized[0] * (1 - cos_angle) - cone_axis_normalized[1] * sin_angle, cone_axis_normalized[2] * cone_axis_normalized[1] * (1 - cos_angle) + cone_axis_normalized[0] * sin_angle, cos_angle + cone_axis_normalized[2]**2 * (1 - cos_angle)]
            ])
            
            # Rotate the point
            new_point = np.dot(rotation_matrix, first_point_normalized) * radius + midpoint_pos
            points.append(new_point)
        
        # Return the set of XYZ coordinates
        return points

    def rotate_point_around_point(self, point, center, angle, axis='z'):
        # Move point to origin
        p = np.array(point) - np.array(center)
        # Rotation matrix around the Z-axis
        if axis == 'z':
            R = np.array([[np.cos(angle), -np.sin(angle), 0],
                        [np.sin(angle),  np.cos(angle), 0],
                        [0,              0,             1]])
        elif axis == 'y':
            R = np.array([[np.cos(angle), 0, np.sin(angle)],
                        [0,             1, 0            ],
                        [-np.sin(angle),0, np.cos(angle)]])
        elif axis == 'x':
            R = np.array([[1, 0,              0             ],
                        [0, np.cos(angle), -np.sin(angle)],
                        [0, np.sin(angle),  np.cos(angle)]])
        else:
            raise ValueError("Invalid rotation axis")
        # Rotate and move back
        return np.dot(R, p) + np.array(center)

    def distance(self, point1, point2):
        
        return np.sqrt(np.sum((np.array(point1) - np.array(point2))**2))

    def minimize_distance_by_rotation(self, lfr_atoms, target_atom_1_uuid, neighboor_atom_1_uuid, midpoint, x_outer_uuid, core_atoms, neighboor_atom_uuid_1, neighboor_atom_uuid_2):
        
        target_atom_1 = lfr_atoms[target_atom_1_uuid]
        x_outer = lfr_atoms[x_outer_uuid]
        original_distance_to_midpoint = self.distance(target_atom_1['coordinate_x'], midpoint)
        original_distance_to_x_outer = self.distance(target_atom_1['coordinate_x'], x_outer['coordinate_x'])   
        neighboor_atom_1 = core_atoms[neighboor_atom_1_uuid]
        
        target_atom_1_pos = np.array([target_atom_1['coordinate_x'], target_atom_1['coordinate_y'], target_atom_1['coordinate_z']])
        neighboor_atom_1_pos = np.array([neighboor_atom_1['coordinate_x'], neighboor_atom_1['coordinate_y'], neighboor_atom_1['coordinate_z']])
        original_distance = np.linalg.norm(target_atom_1_pos - neighboor_atom_1_pos)
                
        best_distance = np.inf  # Initialize with infinity to ensure any first distance is smaller
        best_position = start_position  # Initialize with the start position
        best_angle = 0
        for angle in np.linspace(0, 2 * np.pi, 360, endpoint=False):  # Rotate in 1 degree increments
            rotated_position = self.rotate_point_around_point(start_position, midpoint, angle, axis='z')  # Ensure correct axis
            current_distance = np.linalg.norm(rotated_position - neighboor_atom_1_pos)
            if current_distance < best_distance:
                best_distance = current_distance
                best_position = rotated_position
                best_angle = angle
            
        # Update target_atom_1 position
        lfr_atoms[target_atom_1_uuid]['coordinate_x'], lfr_atoms[target_atom_1_uuid]['coordinate_y'], lfr_atoms[target_atom_1_uuid]['coordinate_z'] = best_position
        
        # Rotate other atoms except x_outer_uuid
        for uuid, atom in lfr_atoms.items():
            if uuid != target_atom_1_uuid and uuid != x_outer_uuid:
                atom_position = (atom['coordinate_x'], atom['coordinate_y'], atom['coordinate_z'])
                rotated_position = self.rotate_point_around_point(atom_position, midpoint, best_angle)
                lfr_atoms[uuid]['coordinate_x'], lfr_atoms[uuid]['coordinate_y'], lfr_atoms[uuid]['coordinate_z'] = rotated_position
        return lfr_atoms

    
    
    def find_neigboors(self, core_json_path, ref_atom_uuid):
        neigboor_atoms = []
        core_atoms = self.read_json_file(core_json_path)
        atom_data = core_atoms.get(ref_atom_uuid)

        if atom_data and "bond" in atom_data:
            for bond in atom_data["bond"]:
                neigboor_atoms.append(bond["to_atom"])

        return neigboor_atoms

    def find_target_atom(self, lfr_atoms, target_atom):
        x_atom_uuid_to_remove = None
        target_atom_uuid = None
        x_outer_uuid = None
        found_target_atom = False

        for uuid, atom_data in lfr_atoms.items():
            if atom_data['atom'] == 'X':
                for bond in atom_data.get('bond', []):
                    to_atom_uuid = bond.get('to_atom')
                    if to_atom_uuid in lfr_atoms and lfr_atoms[to_atom_uuid]['atom'] == target_atom:
                        if not found_target_atom:
                            logging.info(f"Found {target_atom} atom at position {to_atom_uuid}")
                            x_atom_uuid_to_remove = uuid
                            target_atom_uuid = to_atom_uuid
                            found_target_atom = True
                        elif not x_outer_uuid:
                            x_outer_uuid = uuid
                    elif not x_outer_uuid:
                        x_outer_uuid = uuid
                        
        return x_atom_uuid_to_remove, target_atom_uuid, x_outer_uuid

    def find_target_atoms(self, lfr_atoms, target_atom_1, target_atom_2):
        x_atom_uuid_to_remove = None
        target_atom_uuid_1 = None
        target_atom_uuid_2 = None
        x_outer_uuid = None

        for uuid, atom_data in lfr_atoms.items():
            if atom_data['atom'] == 'X':
                for bond in atom_data.get('bond', []):
                    to_atom_uuid = bond.get('to_atom')
                    if to_atom_uuid in lfr_atoms:
                        if lfr_atoms[to_atom_uuid]['atom'] == target_atom_1 and not target_atom_uuid_1:
                            logging.info(f"Found {target_atom_1} atom at position {to_atom_uuid}")
                            x_atom_uuid_to_remove = uuid
                            target_atom_uuid_1 = to_atom_uuid
                        elif lfr_atoms[to_atom_uuid]['atom'] == target_atom_2 and not target_atom_uuid_2:
                            logging.info(f"Found {target_atom_2} atom at position {to_atom_uuid}")
                            x_atom_uuid_to_remove = uuid
                            target_atom_uuid_2 = to_atom_uuid
                        elif not x_outer_uuid:
                            x_outer_uuid = uuid

        return x_atom_uuid_to_remove, target_atom_uuid_1, target_atom_uuid_2, x_outer_uuid

    def read_json_file(self, filepath):
        with open(filepath, 'r') as file:
            return json.load(file)

    def write_json_file(self, filepath, data):
        with open(filepath, 'w') as file:
            json.dump(data, file, indent=2)
                
    def calculate_midpoint(self, atoms):
        coordinates = np.array([(atom['coordinate_x'], atom['coordinate_y'], atom['coordinate_z']) for atom in atoms.values() if atom['atom'] == 'X'])
        if coordinates.size == 0:
            return None
        return np.mean(coordinates, axis=0)

    def shift_atoms(self, atoms, shift):
        reference_atoms = []
        for atom_id, atom in atoms.items():
            if atom['atom'] == 'X':
                reference_atoms.append(atom_id)
            atom['coordinate_x'] -= shift[0]
            atom['coordinate_y'] -= shift[1]
            atom['coordinate_z'] -= shift[2]
        return atoms, reference_atoms

    def shift_atoms_to_origin(self, json_file_path):
        if not os.path.exists(json_file_path):
            logging.error(f"File not found: {json_file_path}")
            return
        
        try:
            with open(json_file_path, 'r') as file:
                atoms = json.load(file)
        except json.JSONDecodeError:
            logging.error(f"Error decoding JSON from file: {json_file_path}")
            #print(f"Error decoding JSON from file: {json_file_path}")
            return

        x_atoms = {atom_id: atom for atom_id, atom in atoms.items() if atom['atom'] == 'X'}
        midpoint = self.calculate_midpoint(x_atoms)  # Use self here
        
        if midpoint is None:
            logging.error("No X atoms found in the JSON file.")
            return

        #print(f"Midpoint of X atoms: {midpoint}")
        logging.info(f"Midpoint of X atoms: {midpoint}")
        
        shifted_atoms, reference_atoms = self.shift_atoms(atoms, midpoint)  # Use self here
        shifted_atoms['Center'] = {
            "atom": "Center",
            "coordinate_x": 0.00,
            "coordinate_y": 0.00,
            "coordinate_z": 0.00
        }
        #print(reference_atoms)
        with open(json_file_path, 'w') as file:
            json.dump(shifted_atoms, file, indent=2)
        
        logging.info(f"Atoms and midpoint shifted and saved back to {json_file_path}")
        #print(f"Atoms and midpoint shifted and saved back to {json_file_path}")
        return shifted_atoms, reference_atoms


    def update_lfr_files(self, core_atoms, reference_atoms, lfr_folder_path, bs_type, core_json_path):
        for i, ref_atom_uuid in enumerate(reference_atoms):
            ref_atom_data = core_atoms[ref_atom_uuid]
            lfr_file_path = os.path.join(lfr_folder_path, f'lfr_copy_{i+1}.json')
            if not os.path.exists(lfr_file_path):
                logging.warning(f"File not found: {lfr_file_path}")
                #print(f"File not found: {lfr_file_path}")
                continue
            self.find_bs_and_update(lfr_file_path, ref_atom_uuid, ref_atom_data, bs_type, core_json_path)

    def create_and_update_subcomponent(self, core_json_path, subcomponent_json_path, lfr_folder_path):
        # Start with an empty dictionary
        subcomponent = {}

        # Update the dictionary with the contents of core.json
        with open(core_json_path, 'r') as core_file:
            core_data = json.load(core_file)
            if "Center" in core_data:
                del core_data["Center"]
            subcomponent.update(core_data)

        # Loop over the contents of lfr_copy_*.json and update subcomponent
        lfr_files_pattern = os.path.join(lfr_folder_path, 'lfr_copy_*.json')
        for lfr_file in glob.glob(lfr_files_pattern):
            with open(lfr_file, 'r') as file:
                lfr_data = json.load(file)
                if "Center" in lfr_data:
                    del lfr_data["Center"]
                subcomponent.update(lfr_data)

        # Save the subcomponent dictionary as a JSON file
        with open(subcomponent_json_path, 'w') as output_file:
            json.dump(subcomponent, output_file, indent=4)

    def generate_xyz_from_json(self, json_file_path, xyz_file_path):
        with open(json_file_path, 'r') as f:
            data = json.load(f)

        with open(xyz_file_path, 'w') as f:
            f.write(f"{len(data)}\n\n")
            for atom_data in data.values():
                f.write(f"{atom_data['atom']} {atom_data['coordinate_x']} {atom_data['coordinate_y']} {atom_data['coordinate_z']}\n")
        logging.info(f"XYZ file has been written to {xyz_file_path}")
        #print(f"XYZ file has been written to {xyz_file_path}")

# Existing __main__ block
if __name__ == "__main__":
    builder = SubunitBuilder()

    core_json_path = r'C:\TheWorldAvatar\Agents\ReticularConstructorAgent\cof_logic\subunit_builder\output\core.json'
    lfr_folder_path = r'C:\TheWorldAvatar\Agents\ReticularConstructorAgent\cof_logic\subunit_builder\output'
    bs_type = "MDNH2"

    # Find the midpoint and shift atoms to the origin in core.json
    core_atoms, reference_atoms = builder.shift_atoms_to_origin(core_json_path)

    # Update the lfr files with the atoms from core.json
    builder.update_lfr_files(core_atoms, reference_atoms, lfr_folder_path, bs_type, core_json_path)
    
    subcomponent_json_path = os.path.join(lfr_folder_path, 'subcomponent.json')
    subcomponent_xyz_path = os.path.join(lfr_folder_path, 'subcomponent.xyz')
    builder.create_and_update_subcomponent(core_json_path, subcomponent_json_path, lfr_folder_path)
    builder.generate_xyz_from_json(subcomponent_json_path, subcomponent_xyz_path)
