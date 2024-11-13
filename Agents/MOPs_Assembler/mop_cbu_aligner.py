__author__ = "Aleksandar Kondinski"
__license__ = "MIT"
__version__ = '1.0.0'
__status__ = "production"

import os
import json
import numpy as np

def read_json_data(file_path):
    with open(file_path, 'r') as file:
        return json.load(file)

def calculate_normal_vector(points):
    v1 = points[1] - points[0]
    v2 = points[2] - points[0]
    normal = np.cross(v1, v2)
    norm = np.linalg.norm(normal)
    if norm == 0:
        # Degenerate case where points are colinear
        return np.array([0, 0, 1])
    normal = normal / norm
    return normal

def rotation_matrix_around_axis(axis, theta):
    axis = axis / np.linalg.norm(axis)
    K = np.array([[0, -axis[2], axis[1]],
                  [axis[2], 0, -axis[0]],
                  [-axis[1], axis[0], 0]])
    rotation_matrix = np.eye(3) + np.sin(theta) * K + (1 - np.cos(theta)) * np.dot(K, K)
    return rotation_matrix

def find_optimal_rotation_weighted(X, Y, weights=None):
    if weights is None:
        weights = np.ones(X.shape[0])

    X_weighted_center = np.average(X, axis=0, weights=weights)
    Y_weighted_center = np.average(Y, axis=0, weights=weights)

    X_centered = X - X_weighted_center
    Y_centered = Y - Y_weighted_center

    X_weighted = X_centered * weights[:, np.newaxis]
    Y_weighted = Y_centered * weights[:, np.newaxis]

    H = np.dot(X_weighted.T, Y_weighted)

    U, S, Vt = np.linalg.svd(H)
    R = np.dot(Vt.T, U.T)

    if np.linalg.det(R) < 0:
        Vt[-1, :] *= -1
        R = np.dot(Vt.T, U.T)

    return R

def rotation_matrix_from_vectors(vec1, vec2):
    a, b = (vec1 / np.linalg.norm(vec1)), (vec2 / np.linalg.norm(vec2))
    v = np.cross(a, b)
    c = np.dot(a, b)
    s = np.linalg.norm(v)

    if s == 0:
        # Vectors are parallel
        return np.eye(3) if c > 0 else -np.eye(3)

    kmat = np.array([[0, -v[2], v[1]],
                     [v[2], 0, -v[0]],
                     [-v[1], v[0], 0]])

    rotation_matrix = np.eye(3) + kmat + kmat.dot(kmat) * ((1 - c) / (s ** 2))
    return rotation_matrix

def calculate_rotation_angle(vec1, vec2, axis):
    vec1_proj = vec1 - np.dot(vec1, axis) * axis
    vec2_proj = vec2 - np.dot(vec2, axis) * axis

    if np.linalg.norm(vec1_proj) == 0 or np.linalg.norm(vec2_proj) == 0:
        return 0  # No rotation needed

    vec1_proj /= np.linalg.norm(vec1_proj)
    vec2_proj /= np.linalg.norm(vec2_proj)

    cos_theta = np.clip(np.dot(vec1_proj, vec2_proj), -1.0, 1.0)
    theta = np.arccos(cos_theta)
    cross_prod = np.cross(vec1_proj, vec2_proj)
    if np.dot(cross_prod, axis) < 0:
        theta = -theta
    return theta

class GenericCBUProcessor:
    def __init__(self, cbu_file, position_files, cbu_label):
        self.cbu_data = read_json_data(cbu_file)
        self.position_files = position_files
        self.cbu_label = cbu_label
        #print(f"Loaded CBU data from {cbu_file}")

    def process(self):
        x_atoms = [(atom_id, atom_data) for atom_id, atom_data in self.cbu_data.items() if atom_data['atom'] == 'X']
        x_atom_ids = [atom_id for atom_id, _ in x_atoms]
        X_coords_x_atoms = np.array([[atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z']] for _, atom_data in x_atoms])

        center_atom = next((atom_data for atom_data in self.cbu_data.values() if atom_data['atom'] == 'CENTER'), None)
        if center_atom is None:
            cbu_center = np.mean(X_coords_x_atoms, axis=0)
            center_atom = {
                'atom': 'CENTER',
                'coordinate_x': cbu_center[0],
                'coordinate_y': cbu_center[1],
                'coordinate_z': cbu_center[2],
                'bond': [],
                'mmtype': 'C_R',
                'qmmm': 'MM'
            }
            self.cbu_data['CENTER'] = center_atom
        else:
            cbu_center = np.array([center_atom['coordinate_x'], center_atom['coordinate_y'], center_atom['coordinate_z']])

        alignment_atoms = x_atoms + [('CENTER', center_atom)]
        X_coords = np.array([[atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z']] for _, atom_data in alignment_atoms])

        for position_file in self.position_files:
            position_data = read_json_data(position_file)
            position_center = np.array([position_data['X'], position_data['Y'], position_data['Z']])
            translation_vector = position_center - cbu_center
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

            # Map 'X' atoms to 'ClosestDummies' and 'CENTER' to position center
            dummy_coords_list = list(position_data['ClosestDummies'].values())
            dummy_coords_list.append(position_center)
            Y_coords = np.array(dummy_coords_list)

            if len(X_coords) != len(Y_coords):
                #print(f"Mismatch in number of atoms for alignment. X: {len(X_coords)}, Y: {len(Y_coords)}")
                n = min(len(X_coords), len(Y_coords))
                X_coords = X_coords[:n]
                Y_coords = Y_coords[:n]
                weights = np.ones(n)
            else:
                weights = np.array([1.0]*len(x_atom_ids) + [0.5])

            # Compute optimal rotation
            optimal_rotation_matrix = find_optimal_rotation_weighted(X_coords, Y_coords, weights)

            for atom_id, atom_data in translated_cbu_data.items():
                original_coords = np.array([atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z']])
                rotated_coords = np.dot(optimal_rotation_matrix, original_coords - position_center) + position_center
                translated_cbu_data[atom_id]['coordinate_x'] = rotated_coords[0]
                translated_cbu_data[atom_id]['coordinate_y'] = rotated_coords[1]
                translated_cbu_data[atom_id]['coordinate_z'] = rotated_coords[2]

            if self.cbu_label == '2-linear':
                print("Performing additional alignment for 2-linear CBU.")
                self.additional_alignment_2_linear(translated_cbu_data, x_atoms)
            elif self.cbu_label.endswith('-pyramidal'):
                print(f"Performing additional alignment for {self.cbu_label} CBU.")
                self.additional_alignment_pyramidal(translated_cbu_data, x_atoms, position_data)
            elif self.cbu_label.endswith('-planar'):
                print(f"Performing additional alignment for {self.cbu_label} CBU.")
                self.additional_alignment_planar(translated_cbu_data, x_atoms, position_data)
            else:
                print(f"No additional alignment needed for CBU label: {self.cbu_label}")

            label = position_data.get("Label", "Unknown")
            output_file_path = position_file.replace('GBU_Positions', 'Translated_CBUs').replace('Position_', f'Translated_CBU_{label}_Position_')
            os.makedirs(os.path.dirname(output_file_path), exist_ok=True)
            with open(output_file_path, 'w') as f:
                json.dump(translated_cbu_data, f, indent=4)
                print(f"Translated and rotated CBU saved to {output_file_path}")

    def additional_alignment_2_linear(self, translated_cbu_data, x_atoms):
        x1_id, x1_data = x_atoms[0]
        x2_id, x2_data = x_atoms[1]

        x1_coords = np.array([translated_cbu_data[x1_id]['coordinate_x'],
                              translated_cbu_data[x1_id]['coordinate_y'],
                              translated_cbu_data[x1_id]['coordinate_z']])
        x2_coords = np.array([translated_cbu_data[x2_id]['coordinate_x'],
                              translated_cbu_data[x2_id]['coordinate_y'],
                              translated_cbu_data[x2_id]['coordinate_z']])
        xx_axis = x2_coords - x1_coords
        xx_axis = xx_axis / np.linalg.norm(xx_axis)

        non_dummy_atoms = [atom_data for atom_data in translated_cbu_data.values() if atom_data['atom'] != 'X']
        if len(non_dummy_atoms) >= 3:
            points = np.array([
                [non_dummy_atoms[0]['coordinate_x'], non_dummy_atoms[0]['coordinate_y'], non_dummy_atoms[0]['coordinate_z']],
                [non_dummy_atoms[1]['coordinate_x'], non_dummy_atoms[1]['coordinate_y'], non_dummy_atoms[1]['coordinate_z']],
                [non_dummy_atoms[2]['coordinate_x'], non_dummy_atoms[2]['coordinate_y'], non_dummy_atoms[2]['coordinate_z']],
            ])
            current_normal = calculate_normal_vector(points)
        else:
            current_normal = np.array([0, 0, 1])

        cbu_center_translated = np.mean([x1_coords, x2_coords], axis=0)
        desired_normal = -cbu_center_translated / np.linalg.norm(cbu_center_translated)

        desired_normal_proj = desired_normal - np.dot(desired_normal, xx_axis) * xx_axis
        desired_normal_proj /= np.linalg.norm(desired_normal_proj)

        current_normal_proj = current_normal - np.dot(current_normal, xx_axis) * xx_axis
        current_normal_proj /= np.linalg.norm(current_normal_proj)

        cos_theta = np.clip(np.dot(current_normal_proj, desired_normal_proj), -1.0, 1.0)
        theta = np.arccos(cos_theta)
        cross_prod = np.cross(current_normal_proj, desired_normal_proj)
        if np.dot(cross_prod, xx_axis) < 0:
            theta = -theta

        rotation_axis = xx_axis
        rotation_matrix = rotation_matrix_around_axis(rotation_axis, theta)

        for atom_id, atom_data in translated_cbu_data.items():
            coords = np.array([atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z']])
            coords_rel = coords - x1_coords
            rotated_coords_rel = np.dot(rotation_matrix, coords_rel)
            rotated_coords = rotated_coords_rel + x1_coords
            translated_cbu_data[atom_id]['coordinate_x'] = rotated_coords[0]
            translated_cbu_data[atom_id]['coordinate_y'] = rotated_coords[1]
            translated_cbu_data[atom_id]['coordinate_z'] = rotated_coords[2]

    def additional_alignment_pyramidal(self, translated_cbu_data, x_atoms, position_data):
        # Get base atom IDs and positions
        base_atom_ids = [atom_id for atom_id, _ in x_atoms]
        base_positions = np.array([
            [translated_cbu_data[atom_id]['coordinate_x'],
            translated_cbu_data[atom_id]['coordinate_y'],
            translated_cbu_data[atom_id]['coordinate_z']] for atom_id in base_atom_ids
        ])

        # Get the apex position in the CBU ('CENTER' atom)
        apex_atom_id = 'CENTER'
        apex_position = np.array([
            translated_cbu_data[apex_atom_id]['coordinate_x'],
            translated_cbu_data[apex_atom_id]['coordinate_y'],
            translated_cbu_data[apex_atom_id]['coordinate_z']
        ])

        # Calculate the centroid of the CBU (average of base positions and apex)
        cbu_centroid = np.mean(np.vstack((base_positions, apex_position)), axis=0)

        # Get the positions of the GBU dummy atoms
        dummy_coords_list = list(position_data['ClosestDummies'].values())
        gbu_dummy_positions = np.array(dummy_coords_list)

        # Get the GBU apex position (position center)
        gbu_apex_position = np.array([position_data['X'], position_data['Y'], position_data['Z']])

        # Calculate the centroid of the GBU (average of dummy positions and apex)
        gbu_centroid = np.mean(np.vstack((gbu_dummy_positions, gbu_apex_position)), axis=0)

        # Step 1: Translate the CBU so that its centroid matches the GBU centroid
        translation_vector = gbu_centroid - cbu_centroid
        for atom_data in translated_cbu_data.values():
            atom_data['coordinate_x'] += translation_vector[0]
            atom_data['coordinate_y'] += translation_vector[1]
            atom_data['coordinate_z'] += translation_vector[2]

        # Update positions after translation
        base_positions += translation_vector
        apex_position += translation_vector
        cbu_centroid += translation_vector

        # Step 2: Rotate the CBU to align the vector from centroid to apex with the GBU apex vector
        vector_cbu_to_apex = apex_position - cbu_centroid
        vector_gbu_to_apex = gbu_apex_position - gbu_centroid
        rotation_matrix = rotation_matrix_from_vectors(vector_cbu_to_apex, vector_gbu_to_apex)

        # Rotate all atoms around the centroid
        for atom_data in translated_cbu_data.values():
            atom_position = np.array([atom_data['coordinate_x'],
                                    atom_data['coordinate_y'],
                                    atom_data['coordinate_z']])
            shifted_position = atom_position - cbu_centroid
            rotated_position = np.dot(rotation_matrix, shifted_position) + cbu_centroid
            atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z'] = rotated_position

        # Update positions after rotation
        base_positions = np.dot(rotation_matrix, (base_positions - cbu_centroid).T).T + cbu_centroid
        apex_position = np.dot(rotation_matrix, apex_position - cbu_centroid) + cbu_centroid

        # Step 3: Optimize alignment using a single X atom
        axis_vector = apex_position - cbu_centroid
        axis_vector /= np.linalg.norm(axis_vector)

        # Select the first X atom for alignment
        x_atom_id = base_atom_ids[0]
        x_atom_data = translated_cbu_data[x_atom_id]
        x_atom_position = np.array([x_atom_data['coordinate_x'],
                                    x_atom_data['coordinate_y'],
                                    x_atom_data['coordinate_z']])
        x_atom_vector = x_atom_position - cbu_centroid

        # Find the closest dummy position
        distances = np.linalg.norm(gbu_dummy_positions - x_atom_position, axis=1)
        closest_dummy_index = np.argmin(distances)
        closest_dummy_position = gbu_dummy_positions[closest_dummy_index]
        dummy_vector = closest_dummy_position - cbu_centroid

        # Calculate the rotation angle to align x_atom_vector with dummy_vector around axis_vector
        rotation_angle = calculate_rotation_angle(x_atom_vector, dummy_vector, axis_vector)

        # Rotate all atoms around the centroid to apex axis by the calculated angle
        rotation_matrix_axis = rotation_matrix_around_axis(axis_vector, rotation_angle)

        for atom_data in translated_cbu_data.values():
            atom_position = np.array([atom_data['coordinate_x'],
                                    atom_data['coordinate_y'],
                                    atom_data['coordinate_z']])
            shifted_position = atom_position - cbu_centroid
            rotated_position = np.dot(rotation_matrix_axis, shifted_position) + cbu_centroid
            atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z'] = rotated_position

        print("Completed additional alignment for pyramidal CBU.")


    def additional_alignment_planar(self, translated_cbu_data, x_atoms, position_data):
        # New method for aligning planar CBUs

        # Get base atom IDs and positions
        base_atom_ids = [atom_id for atom_id, _ in x_atoms]
        base_positions = np.array([
            [translated_cbu_data[atom_id]['coordinate_x'],
             translated_cbu_data[atom_id]['coordinate_y'],
             translated_cbu_data[atom_id]['coordinate_z']] for atom_id in base_atom_ids
        ])

        # Calculate the centroid of the base atoms (CBU base center)
        cbu_base_centroid = np.mean(base_positions, axis=0)

        # Get the positions of the GBU dummy atoms
        dummy_coords_list = list(position_data['ClosestDummies'].values())
        gbu_dummy_positions = np.array(dummy_coords_list)

        # Calculate the centroid of the GBU dummy positions (GBU base centroid)
        gbu_base_centroid = np.mean(gbu_dummy_positions, axis=0)

        # Step 1: Translate the CBU so that its base centroid matches the GBU base centroid
        translation_vector = gbu_base_centroid - cbu_base_centroid
        for atom_data in translated_cbu_data.values():
            atom_data['coordinate_x'] += translation_vector[0]
            atom_data['coordinate_y'] += translation_vector[1]
            atom_data['coordinate_z'] += translation_vector[2]

        # Update positions after translation
        base_positions += translation_vector
        cbu_base_centroid += translation_vector

        # Step 2: Rotate the CBU base plane to align with the GBU base plane
        if len(base_positions) < 3 or len(gbu_dummy_positions) < 3:
            print("Not enough base atoms to define a plane. Skipping rotation.")
            return

        # Calculate normals of the CBU base and GBU base
        cbu_base_normal = calculate_normal_vector(base_positions[:3])
        gbu_base_normal = calculate_normal_vector(gbu_dummy_positions[:3])

        # Calculate rotation matrix to align the CBU base normal with the GBU base normal
        rotation_matrix_base = rotation_matrix_from_vectors(cbu_base_normal, gbu_base_normal)

        # Rotate all atoms around the base centroid
        for atom_data in translated_cbu_data.values():
            atom_position = np.array([atom_data['coordinate_x'],
                                      atom_data['coordinate_y'],
                                      atom_data['coordinate_z']])
            shifted_position = atom_position - cbu_base_centroid
            rotated_position = np.dot(rotation_matrix_base, shifted_position) + cbu_base_centroid
            atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z'] = rotated_position

        # Update positions after base alignment rotation
        base_positions = np.dot(rotation_matrix_base, (base_positions - cbu_base_centroid).T).T + cbu_base_centroid

        # Step 3: Rotate around the base normal to best fit the base atoms to the dummy atoms
        # Project base atom positions onto the plane perpendicular to the base normal
        base_positions_proj = base_positions - np.outer(np.dot(base_positions - cbu_base_centroid, cbu_base_normal), cbu_base_normal)
        gbu_dummy_positions_proj = gbu_dummy_positions - np.outer(np.dot(gbu_dummy_positions - cbu_base_centroid, cbu_base_normal), cbu_base_normal)

        # Find the optimal rotation around the base normal
        base_vectors = base_positions_proj - cbu_base_centroid
        gbu_vectors = gbu_dummy_positions_proj - cbu_base_centroid

        # Use Kabsch algorithm in 2D (on the plane perpendicular to base normal)
        # Define two orthogonal vectors in the plane
        u = np.cross(cbu_base_normal, [1, 0, 0])
        if np.linalg.norm(u) == 0:
            u = np.cross(cbu_base_normal, [0, 1, 0])
        u = u / np.linalg.norm(u)
        v = np.cross(cbu_base_normal, u)

        # Express vectors in the plane coordinate system
        base_coords_2d = np.array([[np.dot(vec, u), np.dot(vec, v)] for vec in base_vectors])
        gbu_coords_2d = np.array([[np.dot(vec, u), np.dot(vec, v)] for vec in gbu_vectors])

        # Compute 2D rotation angle
        H = np.dot(base_coords_2d.T, gbu_coords_2d)
        U, S, Vt = np.linalg.svd(H)
        R2D = np.dot(Vt.T, U.T)
        if np.linalg.det(R2D) < 0:
            Vt[-1, :] *= -1
            R2D = np.dot(Vt.T, U.T)

        # Construct 3D rotation matrix around cbu_base_normal
        cos_theta = R2D[0, 0]
        sin_theta = R2D[1, 0]
        theta = np.arctan2(sin_theta, cos_theta)
        rotation_matrix_apex = rotation_matrix_around_axis(cbu_base_normal, theta)

        # Rotate all atoms around the base centroid
        for atom_data in translated_cbu_data.values():
            atom_position = np.array([atom_data['coordinate_x'],
                                      atom_data['coordinate_y'],
                                      atom_data['coordinate_z']])
            shifted_position = atom_position - cbu_base_centroid
            rotated_position = np.dot(rotation_matrix_apex, shifted_position) + cbu_base_centroid
            atom_data['coordinate_x'], atom_data['coordinate_y'], atom_data['coordinate_z'] = rotated_position

        print("Completed additional alignment for planar CBU.")


def rotation_matrix_from_vectors(vec1, vec2):
    a, b = (vec1 / np.linalg.norm(vec1)), (vec2 / np.linalg.norm(vec2))
    v = np.cross(a, b)
    c = np.dot(a, b)
    s = np.linalg.norm(v)

    if s == 0:
        # Vectors are parallel
        return np.eye(3) if c > 0 else -np.eye(3)

    kmat = np.array([[0, -v[2], v[1]],
                     [v[2], 0, -v[0]],
                     [-v[1], v[0], 0]])

    rotation_matrix = np.eye(3) + kmat + kmat.dot(kmat) * ((1 - c) / (s ** 2))
    return rotation_matrix

def calculate_rotation_angle(vec1, vec2, axis):
    vec1_proj = vec1 - np.dot(vec1, axis) * axis
    vec2_proj = vec2 - np.dot(vec2, axis) * axis

    if np.linalg.norm(vec1_proj) == 0 or np.linalg.norm(vec2_proj) == 0:
        return 0  # No rotation needed

    vec1_proj /= np.linalg.norm(vec1_proj)
    vec2_proj /= np.linalg.norm(vec2_proj)

    cos_theta = np.clip(np.dot(vec1_proj, vec2_proj), -1.0, 1.0)
    theta = np.arccos(cos_theta)
    cross_prod = np.cross(vec1_proj, vec2_proj)
    if np.dot(cross_prod, axis) < 0:
        theta = -theta
    return theta

def rotation_matrix_around_axis(axis, theta):
    axis = axis / np.linalg.norm(axis)
    K = np.array([[0, -axis[2], axis[1]],
                  [axis[2], 0, -axis[0]],
                  [-axis[1], axis[0], 0]])
    rotation_matrix = np.eye(3) + np.sin(theta) * K + (1 - np.cos(theta)) * np.dot(K, K)
    return rotation_matrix
