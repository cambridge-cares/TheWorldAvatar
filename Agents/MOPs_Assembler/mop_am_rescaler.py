__author__ = "Aleksandar Kondinski"
__license__ = "MIT" 
__version__ = '0.1.0' 
__status__ = "development" 

import json
import numpy as np
import math
import os
import logging

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class GBUProcessor:
    # Class variables for Set 1 and Set 2 labels
    set1_labels = ["2-linear", "3-planar", "4-planar", "5-planar"]
    set2_labels = ["2-bent", "3-pyramidal", "4-pyramidal", "5-pyramidal"]

    def __init__(self, assembly_path):
        self.assembly_data = self.load_json(assembly_path)

    @staticmethod
    def load_json(file_path):
        """Load JSON data from a file."""
        try:
            with open(file_path, 'r') as file:
                data = json.load(file)
            logger.info(f"Loaded assembly data from {file_path}")
            return data
        except FileNotFoundError:
            logger.error(f"Assembly file not found: {file_path}")
            raise
        except json.JSONDecodeError:
            logger.error(f"Invalid JSON format in assembly file: {file_path}")
            raise

    @staticmethod
    def calculate_centroid(cbu_data):
        """Calculate the centroid of dummy atoms in the CBU data."""
        total_x, total_y, total_z = 0.0, 0.0, 0.0
        dummy_count = 0

        for atom_data in cbu_data.values():
            if atom_data.get("atom") == "X":  # Check if it's a dummy atom
                total_x += atom_data.get("coordinate_x", 0.0)
                total_y += atom_data.get("coordinate_y", 0.0)
                total_z += atom_data.get("coordinate_z", 0.0)
                dummy_count += 1

        if dummy_count > 0:
            centroid_x = total_x / dummy_count
            centroid_y = total_y / dummy_count
            centroid_z = total_z / dummy_count
            centroid = np.array([centroid_x, centroid_y, centroid_z])
            logger.info(f"Calculated centroid: {centroid}")
            return centroid
        else:
            logger.warning("No dummy atoms found.")
            return None  # No dummy atoms found

    @staticmethod
    def calculate_distances_from_centroid(cbu_data, centroid):
        """Calculate the Euclidean distances from the centroid to each dummy atom."""
        distances = []
        for atom_data in cbu_data.values():
            if atom_data.get("atom") == "X":
                dummy_position = np.array([
                    atom_data.get("coordinate_x", 0.0),
                    atom_data.get("coordinate_y", 0.0),
                    atom_data.get("coordinate_z", 0.0)
                ])
                distance = np.linalg.norm(dummy_position - centroid)
                distances.append(distance)
        average_distance = np.mean(distances) if distances else 0.0
        logger.info(f"Calculated average distance from centroid to dummy atoms: {average_distance}")
        return average_distance

    def calculate_proportion_based_distance(self, label, a_prime):
        """Calculate the proportion-based distance for Set 2 type CBUs."""
        positions_list = list(self.assembly_data.values())[0]

        # Find all positions with the specified label
        positions_with_label = [position for position in positions_list if position.get('Label') == label]

        if not positions_with_label:
            logger.warning(f"No positions found with the label '{label}'.")
            return None

        total_distance_a = 0
        total_distance_b = 0
        count = 0

        for position in positions_with_label:
            pos_xyz = np.array([position.get('X', 0.0), position.get('Y', 0.0), position.get('Z', 0.0)])
            dummies_positions = []
            for dummy_key in position.get('ClosestDummies', []):
                dummy = next((d for d in positions_list if d.get('Key') == dummy_key), None)
                if dummy:
                    dummies_positions.append(np.array([dummy.get('X', 0.0), dummy.get('Y', 0.0), dummy.get('Z', 0.0)]))

            if dummies_positions:
                centroid = np.mean(dummies_positions, axis=0)
                distance_a = np.mean([np.linalg.norm(centroid - dp) for dp in dummies_positions])
                total_distance_a += distance_a

                distance_b = np.mean([np.linalg.norm(pos_xyz - dp) for dp in dummies_positions])
                total_distance_b += distance_b
                count += 1

        if count == 0:
            logger.warning(f"No dummy positions found for the label '{label}'.")
            return None

        average_distance_a = total_distance_a / count
        average_distance_b = total_distance_b / count
        proportion = average_distance_a / average_distance_b if average_distance_b != 0 else 0
        b_prime = a_prime / proportion if proportion != 0 else 0

        logger.info(f"Calculated proportion-based distance (b'): {b_prime}")
        return b_prime

    def process_cbu(self, cbu_path, cbu_label):
        """Process a CBU based on its GBU label and calculate the final distance."""
        # Load the CBU data
        try:
            cbu_data = self.load_json(cbu_path)
        except Exception as e:
            logger.error(f"Failed to load CBU data from {cbu_path}: {e}")
            return None

        # Calculate centroid of dummy atoms
        centroid = self.calculate_centroid(cbu_data)
        if centroid is None:
            logger.warning(f"No dummy atoms found in {cbu_path}.")
            return None

        # Compute distances from centroid to each dummy atom
        average_centroid_to_dummy = self.calculate_distances_from_centroid(cbu_data, centroid)

        if cbu_label in self.set2_labels:
            # For Set 2 types, calculate the proportion-based distance
            b_prime = self.calculate_proportion_based_distance(cbu_label, average_centroid_to_dummy)
            if b_prime is not None:
                return {
                    "GBU Label": cbu_label,
                    "Calculated Absolute Distance (b')": b_prime
                }
        elif cbu_label in self.set1_labels:
            # For Set 1 types, return the average centroid-to-dummy distance
            return {
                "GBU Label": cbu_label,
                "Average Centroid-to-Dummy Distance": average_centroid_to_dummy
            }
        else:
            logger.warning(f"Unknown GBU label '{cbu_label}' for {cbu_path}.")
            return None

    def process_all_cbos(self, cbu_paths_and_labels):
        """Process a list of CBUs with their respective labels and return the results."""
        results = []
        for cbu_path, cbu_label in cbu_paths_and_labels:
            result = self.process_cbu(cbu_path, cbu_label)
            if result:
                results.append(result)
        logger.info(f"Processed all CBUs. Results: {results}")
        return results

class AssemblyModelRescaler:
    def __init__(self, temp_assembly_model_path, assembly_model_key, cbu1_label, cbu2_label, cbu1_path, cbu2_path):
        """
        Initialize the AssemblyModelRescaler with necessary parameters.
        
        Parameters:
            temp_assembly_model_path (str): Path to the temporary assembly model JSON file.
            assembly_model_key (str): Key for the assembly model within the JSON.
            cbu1_label (str): Label for CBU1.
            cbu2_label (str): Label for CBU2.
            cbu1_path (str): Path to CBU1 JSON file.
            cbu2_path (str): Path to CBU2 JSON file.
        """
        self.temp_assembly_model_path = temp_assembly_model_path
        self.assembly_model_key = assembly_model_key
        self.cbu1_label = cbu1_label
        self.cbu2_label = cbu2_label
        self.cbu1_path = cbu1_path
        self.cbu2_path = cbu2_path
        self.data = None
        self.structure_key = None
        self.positions = None
        self.center = None

    def workflow_operations(self, rescaled_dir):
        # Ensure the rescaled directory exists
        os.makedirs(rescaled_dir, exist_ok=True)
        logger.info(f"Rescaled directory set to: {rescaled_dir}")

        # Define paths for the output files using assembly_model_key
        assembly_output_temp = os.path.join(rescaled_dir, f'{self.assembly_model_key}_temp.json')
        assembly_output_final = os.path.join(rescaled_dir, f'{self.assembly_model_key}_temp_final_scaled.json')

        # Initialize GBUProcessor
        try:
            gbu_processor = GBUProcessor(self.temp_assembly_model_path)
        except Exception as e:
            logger.error(f"Failed to initialize GBUProcessor: {e}")
            raise

        # Process CBUs and calculate the required distances
        cbu_paths_and_labels = [
            (self.cbu1_path, self.cbu1_label),
            (self.cbu2_path, self.cbu2_label)
        ]

        try:
            cbu_results = gbu_processor.process_all_cbos(cbu_paths_and_labels)
        except Exception as e:
            logger.error(f"Failed to process CBUs: {e}")
            raise

        # Extract the distances from the CBU results
        distances = {result['GBU Label']: result for result in cbu_results}

        # Validate that both labels are processed
        if self.cbu1_label not in distances or self.cbu2_label not in distances:
            logger.error("CBU processing did not return results for both labels.")
            raise ValueError("CBU processing did not return results for both labels.")

        # Assign distances directly
        label1_result = distances[self.cbu1_label]
        label2_result = distances[self.cbu2_label]

        label1_distance = label1_result.get("Average Centroid-to-Dummy Distance") or label1_result.get("Calculated Absolute Distance (b')")
        label2_distance = label2_result.get("Average Centroid-to-Dummy Distance") or label2_result.get("Calculated Absolute Distance (b')")

        if label1_distance is None or label2_distance is None:
            logger.error("Could not determine distances for both labels.")
            raise ValueError("Could not determine distances for both labels.")

        # Instantiate the InternalAssemblyModelRescaler
        try:
            rescaler = InternalAssemblyModelRescaler(
                input_file=self.temp_assembly_model_path,
                output_temp_file=assembly_output_temp,
                output_final_file=assembly_output_final
            )
        except Exception as e:
            logger.error(f"Failed to instantiate InternalAssemblyModelRescaler: {e}")
            raise

        # Load the assembly model
        try:
            rescaler.load_json()
        except Exception as e:
            logger.error(f"Failed to load assembly model: {e}")
            raise

        # Rescale and adjust dummy positions
        try:
            rescaler.rescale_and_adjust_dummies(
                label1_distance, label2_distance,
                self.cbu1_label, self.cbu2_label
            )
        except Exception as e:
            logger.error(f"Failed during rescaling and adjustment: {e}")
            raise

        logger.info(f"Workflow completed successfully. Final rescaled file at: {assembly_output_final}")



    def workflow_operations_working(self, rescaled_dir):
        """
        Perform the workflow operations to rescale and adjust the assembly model.
        
        Parameters:
            rescaled_dir (str): Directory where the rescaled JSON files will be saved.
        """
        # Ensure the rescaled directory exists
        os.makedirs(rescaled_dir, exist_ok=True)
        logger.info(f"Rescaled directory set to: {rescaled_dir}")

        # Define paths for the output files using assembly_model_key
        assembly_output_temp = os.path.join(rescaled_dir, f'{self.assembly_model_key}_temp.json')
        assembly_output_final = os.path.join(rescaled_dir, f'{self.assembly_model_key}_temp_final_scaled.json')

        # Initialize GBUProcessor
        try:
            gbu_processor = GBUProcessor(self.temp_assembly_model_path)
        except Exception as e:
            logger.error(f"Failed to initialize GBUProcessor: {e}")
            raise

        # Process CBUs and calculate the required distances
        cbu_paths_and_labels = [
            (self.cbu1_path, self.cbu1_label),
            (self.cbu2_path, self.cbu2_label)
        ]

        try:
            cbu_results = gbu_processor.process_all_cbos(cbu_paths_and_labels)
        except Exception as e:
            logger.error(f"Failed to process CBUs: {e}")
            raise

        # Extract the distances from the CBU results
        distances = {result['GBU Label']: result for result in cbu_results}

        # Validate that both labels are processed
        if self.cbu1_label not in distances or self.cbu2_label not in distances:
            logger.error("CBU processing did not return results for both labels.")
            raise ValueError("CBU processing did not return results for both labels.")

        # Initialize variables
        label1_result = distances[self.cbu1_label]
        label2_result = distances[self.cbu2_label]

        label1_dummy = None
        label2_dummy = None

        # Determine which label is Set1 and which is Set2
        if "Average Centroid-to-Dummy Distance" in label1_result:
            # cbu1_label is Set1 label
            set1_label = self.cbu1_label
            label1_dummy = label1_result["Average Centroid-to-Dummy Distance"]
        elif "Calculated Absolute Distance (b')" in label1_result:
            # cbu1_label is Set2 label
            set2_label = self.cbu1_label
            label2_dummy = label1_result["Calculated Absolute Distance (b')"]
        else:
            logger.error(f"Unexpected data for label '{self.cbu1_label}'.")
            raise ValueError(f"Unexpected data for label '{self.cbu1_label}'.")

        if "Average Centroid-to-Dummy Distance" in label2_result:
            # cbu2_label is Set1 label
            set1_label = self.cbu2_label
            label1_dummy = label2_result["Average Centroid-to-Dummy Distance"]
        elif "Calculated Absolute Distance (b')" in label2_result:
            # cbu2_label is Set2 label
            set2_label = self.cbu2_label
            label2_dummy = label2_result["Calculated Absolute Distance (b')"]
        else:
            logger.error(f"Unexpected data for label '{self.cbu2_label}'.")
            raise ValueError(f"Unexpected data for label '{self.cbu2_label}'.")

        if label1_dummy is None or label2_dummy is None:
            logger.error("Could not determine distances for both labels.")
            raise ValueError("Could not determine distances for both labels.")

        # Instantiate the InternalAssemblyModelRescaler
        try:
            rescaler = InternalAssemblyModelRescaler(
                input_file=self.temp_assembly_model_path,
                output_temp_file=assembly_output_temp,
                output_final_file=assembly_output_final
            )
        except Exception as e:
            logger.error(f"Failed to instantiate InternalAssemblyModelRescaler: {e}")
            raise

        # Load the assembly model
        try:
            rescaler.load_json()
        except Exception as e:
            logger.error(f"Failed to load assembly model: {e}")
            raise

        # Rescale and adjust dummy positions
        try:
            rescaler.rescale_and_adjust_dummies(
                label1_dummy, label2_dummy,
                set1_label, set2_label
            )
        except Exception as e:
            logger.error(f"Failed during rescaling and adjustment: {e}")
            raise

        logger.info(f"Workflow completed successfully. Final rescaled file at: {assembly_output_final}")

class InternalAssemblyModelRescaler:
    """
    Internal class to handle rescaling and adjusting dummy positions.
    This class is used within the AssemblyModelRescaler.workflow_operations method.
    """

    def __init__(self, input_file, output_temp_file, output_final_file):
        self.input_file = input_file
        self.output_temp_file = output_temp_file
        self.output_final_file = output_final_file
        self.data = None
        self.structure_key = None
        self.positions = None
        self.center = None

    def load_json(self):
        """Load JSON data from the input file."""
        try:
            with open(self.input_file, 'r') as f:
                self.data = json.load(f)
            logger.info(f"Loaded assembly model from {self.input_file}")
        except FileNotFoundError:
            logger.error(f"Assembly model file not found: {self.input_file}")
            raise
        except json.JSONDecodeError:
            logger.error(f"Invalid JSON format in assembly model file: {self.input_file}")
            raise

        # Get the structure key and positions data
        try:
            self.structure_key = list(self.data.keys())[0]
            self.positions = self.data[self.structure_key]
            logger.info(f"Extracted structure key: {self.structure_key}")
        except IndexError:
            logger.error("Assembly model JSON is empty or malformed.")
            raise

        # Find the center point
        self.center = next((p for p in self.positions if p.get("Label") == "Center"), None)
        if self.center is None:
            logger.error("Center point not found in assembly model.")
            raise ValueError("Center point not found.")

    def save_json(self, filename):
        """Save JSON data to a file."""
        try:
            with open(filename, 'w') as f:
                json.dump(self.data, f, indent=4)
            logger.info(f"Saved JSON data to {filename}")
        except Exception as e:
            logger.error(f"Failed to save JSON to {filename}: {e}")
            raise

    @staticmethod
    def calculate_distance(point1, point2):
        """Calculate the Euclidean distance between two points in 3D."""
        return math.sqrt(
            (point2["X"] - point1["X"]) ** 2 +
            (point2["Y"] - point1["Y"]) ** 2 +
            (point2["Z"] - point1["Z"]) ** 2
        )

    def rescale_from_center(self, point, scaling_factor):
        """Uniformly rescale a point relative to the center."""
        point["X"] = self.center["X"] + (point["X"] - self.center["X"]) * scaling_factor
        point["Y"] = self.center["Y"] + (point["Y"] - self.center["Y"]) * scaling_factor
        point["Z"] = self.center["Z"] + (point["Z"] - self.center["Z"]) * scaling_factor
        logger.debug(f"Rescaled point {point['Key']} to ({point['X']}, {point['Y']}, {point['Z']})")

    def adjust_dummy_position(self, position1, position2, dummy, distance1, total_distance):
        """Adjust the dummy position along the line between two positions."""
        if total_distance == 0:
            logger.error("Positions are at the same point. Cannot determine direction.")
            raise ValueError("Positions are at the same point. Cannot determine direction.")

        ratio = distance1 / total_distance

        # Calculate dummy position along the line
        dummy["X"] = position1["X"] + ratio * (position2["X"] - position1["X"])
        dummy["Y"] = position1["Y"] + ratio * (position2["Y"] - position1["Y"])
        dummy["Z"] = position1["Z"] + ratio * (position2["Z"] - position1["Z"])

        logger.debug(f"Adjusted dummy position {dummy['Key']} to ({dummy['X']}, {dummy['Y']}, {dummy['Z']})")


    def adjust_dummy_position_working(self, pyramidal, linear, dummy, distance_pyramidal, distance_linear):
        """Adjust the dummy position to lay between two points with specific distances."""
        direction = {
            "X": linear["X"] - pyramidal["X"],
            "Y": linear["Y"] - pyramidal["Y"],
            "Z": linear["Z"] - pyramidal["Z"]
        }

        total_distance = self.calculate_distance(pyramidal, linear)

        if total_distance == 0:
            logger.error("Pyramidal and Linear positions are the same. Cannot determine direction.")
            raise ValueError("Pyramidal and Linear positions are the same. Cannot determine direction.")

        # Normalize the direction vector
        direction["X"] /= total_distance
        direction["Y"] /= total_distance
        direction["Z"] /= total_distance

        # Update dummy position
        dummy["X"] = pyramidal["X"] + direction["X"] * distance_pyramidal
        dummy["Y"] = pyramidal["Y"] + direction["Y"] * distance_pyramidal
        dummy["Z"] = pyramidal["Z"] + direction["Z"] * distance_pyramidal

        logger.debug(f"Adjusted dummy position {dummy['Key']} to ({dummy['X']}, {dummy['Y']}, {dummy['Z']})")

    def rescale_and_adjust_dummiesX(self, distance1, distance2, label1, label2):
        """
        Rescale the assembly model and adjust dummy positions based on the provided labels and distances.

        Parameters:
            distance1 (float): The calculated distance for label1.
            distance2 (float): The calculated distance for label2.
            label1 (str): Label for the first CBU.
            label2 (str): Label for the second CBU.
        """
        target_distance = distance1 + distance2
        logger.info(f"Target distance for rescaling: {target_distance}")

        # Find positions with label1 and label2
        position_label1 = next((p for p in self.positions if p.get("Label") == label1), None)
        position_label2 = next((p for p in self.positions if p.get("Label") == label2), None)

        if position_label1 is None or position_label2 is None:
            logger.error("Could not find positions with specified labels.")
            raise ValueError("Could not find positions with specified labels.")

        # Calculate current distance between position_label1 and position_label2
        current_distance = self.calculate_distance(position_label1, position_label2)
        if current_distance == 0:
            logger.error("Positions with labels are at the same point.")
            raise ValueError("Positions with labels are at the same point.")

        # Compute scaling factor
        scaling_factor = target_distance / current_distance
        logger.info(f"Scaling factor calculated: {scaling_factor}")

        # **Store original direction vectors between positions and dummies**
        direction_vectors = {}
        for position in self.positions:
            position_key = position["Key"]
            for dummy_key in position.get("ClosestDummies", []):
                dummy = next((p for p in self.positions if p.get("Key") == dummy_key), None)
                if dummy:
                    vector = {
                        "X": dummy["X"] - position["X"],
                        "Y": dummy["Y"] - position["Y"],
                        "Z": dummy["Z"] - position["Z"]
                    }
                    norm = math.sqrt(vector["X"]**2 + vector["Y"]**2 + vector["Z"]**2)
                    if norm != 0:
                        vector["X"] /= norm
                        vector["Y"] /= norm
                        vector["Z"] /= norm
                    else:
                        vector = {"X": 0, "Y": 0, "Z": 0}
                    direction_vectors[(position_key, dummy_key)] = vector

        # **Rescale all positions uniformly**
        for position in self.positions:
            self.rescale_from_center(position, scaling_factor)

        # Save the intermediate rescaled model
        self.save_json(self.output_temp_file)

        # **Adjust dummy positions using stored direction vectors**
        for position in self.positions:
            position_key = position["Key"]
            # Determine the distance to use
            if position.get("Label") == label1:
                distance = distance1
            elif position.get("Label") == label2:
                distance = distance2
            else:
                continue  # Skip positions that are not label1 or label2
            for dummy_key in position.get("ClosestDummies", []):
                dummy = next((p for p in self.positions if p.get("Key") == dummy_key), None)
                if dummy:
                    direction_vector = direction_vectors.get((position_key, dummy_key))
                    if direction_vector:
                        self.adjust_dummy_position_single(position, dummy, distance, direction_vector)

        # Save the final rescaled model
        self.save_json(self.output_final_file)
        logger.info(f"Final scaled model saved to: {self.output_final_file}")

    def rescale_and_adjust_dummies_working(self, label1_dummy, label2_dummy, label1, label2):
        """
        Rescale the assembly model and adjust dummy positions based on the provided labels and distances.
        
        Parameters:
            label1_dummy (float): The average centroid-to-dummy distance for Set1.
            label2_dummy (float): The calculated absolute distance (b') for Set2.
            label1 (str): Label for Set1.
            label2 (str): Label for Set2.
        """
        target_distance = label1_dummy + label2_dummy
        logger.info(f"Target distance for rescaling: {target_distance}")

        # Find the first occurrence of label1 and its neighbor with label2
        position_label1 = next((p for p in self.positions if p.get("Label") == label1), None)
        position_label2 = None
        if position_label1:
            for neighbor in position_label1.get("Neighbors", []):
                if neighbor.get("Label") == label2:
                    position_label2 = next((p for p in self.positions if p.get("Key") == neighbor.get("Key")), None)
                    break

        if position_label1 is None or position_label2 is None:
            logger.error("Could not find positions with specified labels.")
            raise ValueError("Could not find positions with specified labels.")

        # Calculate current distance between position_label1 and position_label2
        current_distance = self.calculate_distance(position_label1, position_label2)
        if current_distance == 0:
            logger.error("Positions with labels are at the same point.")
            raise ValueError("Positions with labels are at the same point.")

        # Compute scaling factor
        scaling_factor = target_distance / current_distance
        logger.info(f"Scaling factor calculated: {scaling_factor}")

        # Rescale all positions uniformly
        for position in self.positions:
            self.rescale_from_center(position, scaling_factor)

        # Save the intermediate rescaled model
        self.save_json(self.output_temp_file)

        # Adjust dummy positions
        for position in self.positions:
            if position.get("Label") == label1:
                for neighbor in position.get("Neighbors", []):
                    if neighbor.get("Label") == label2:
                        neighbor_position = next((p for p in self.positions if p.get("Key") == neighbor.get("Key")), None)
                        if neighbor_position:
                            common_dummy_key = next((d for d in position.get("ClosestDummies", []) 
                                                    if d in neighbor_position.get("ClosestDummies", [])), None)
                            if common_dummy_key:
                                dummy_position = next((p for p in self.positions if p.get("Key") == common_dummy_key), None)
                                if dummy_position:
                                    self.adjust_dummy_position(position, neighbor_position, dummy_position, label1_dummy, label2_dummy)

        # Save the final rescaled model
        self.save_json(self.output_final_file)
        logger.info(f"Final scaled model saved to: {self.output_final_file}")

    def adjust_dummy_position_single(self, position, dummy, distance, direction_vector):
        """Adjust the dummy position along the stored direction vector from the position."""
        # Adjust the dummy position
        dummy["X"] = position["X"] + direction_vector["X"] * distance
        dummy["Y"] = position["Y"] + direction_vector["Y"] * distance
        dummy["Z"] = position["Z"] + direction_vector["Z"] * distance

        logger.debug(f"Adjusted dummy position {dummy['Key']} to ({dummy['X']}, {dummy['Y']}, {dummy['Z']})")

    def rescale_and_adjust_dummies(self, distance1, distance2, label1, label2):
        """
        Rescale the assembly model and adjust dummy positions based on the provided labels and distances.

        Parameters:
            distance1 (float): The calculated distance for label1.
            distance2 (float): The calculated distance for label2.
            label1 (str): Label for the first CBU.
            label2 (str): Label for the second CBU.
        """
        target_distance = distance1 + distance2
        logger.info(f"Target distance for rescaling: {target_distance}")

        # Collect all positions with label1 and label2
        positions_label1 = [p for p in self.positions if p.get("Label") == label1]
        positions_label2 = [p for p in self.positions if p.get("Label") == label2]

        if not positions_label1 or not positions_label2:
            logger.error("Could not find positions with specified labels.")
            raise ValueError("Could not find positions with specified labels.")

        # Find connected pairs (positions with label1 that have neighbors with label2)
        connected_pairs = []
        for position1 in positions_label1:
            for neighbor_info in position1.get("Neighbors", []):
                neighbor = next((p for p in self.positions if p.get("Key") == neighbor_info.get("Key")), None)
                if neighbor and neighbor.get("Label") == label2:
                    connected_pairs.append((position1, neighbor))

        if connected_pairs:
            # Calculate average current distance between connected pairs
            current_distances = [self.calculate_distance(p1, p2) for p1, p2 in connected_pairs]
            current_distance = sum(current_distances) / len(current_distances)
            logger.info(f"Average current distance between connected pairs: {current_distance}")
        else:
            # If no connected pairs, use the first positions of label1 and label2
            position1 = positions_label1[0]
            position2 = positions_label2[0]
            current_distance = self.calculate_distance(position1, position2)
            logger.warning("No connected pairs found. Using first positions of label1 and label2 for scaling.")
            connected_pairs = [(position1, position2)]
            logger.info(f"Current distance between first positions: {current_distance}")

        if current_distance == 0:
            logger.error("Positions with labels are at the same point.")
            raise ValueError("Positions with labels are at the same point.")

        # Compute scaling factor
        scaling_factor = target_distance / current_distance
        logger.info(f"Scaling factor calculated: {scaling_factor}")

        # **Store original direction vectors between positions and dummies**
        direction_vectors = {}
        for position in self.positions:
            position_key = position["Key"]
            for dummy_key in position.get("ClosestDummies", []):
                dummy = next((p for p in self.positions if p.get("Key") == dummy_key), None)
                if dummy:
                    vector = {
                        "X": dummy["X"] - position["X"],
                        "Y": dummy["Y"] - position["Y"],
                        "Z": dummy["Z"] - position["Z"]
                    }
                    norm = math.sqrt(vector["X"]**2 + vector["Y"]**2 + vector["Z"]**2)
                    if norm != 0:
                        vector["X"] /= norm
                        vector["Y"] /= norm
                        vector["Z"] /= norm
                    else:
                        vector = {"X": 0, "Y": 0, "Z": 0}
                    direction_vectors[(position_key, dummy_key)] = vector

        # **Rescale all positions uniformly**
        for position in self.positions:
            self.rescale_from_center(position, scaling_factor)

        # Save the intermediate rescaled model
        self.save_json(self.output_temp_file)

        # **Adjust dummy positions**
        for position in self.positions:
            position_key = position["Key"]
            # Determine the distance to use
            if position.get("Label") == label1:
                distance = distance1
            elif position.get("Label") == label2:
                distance = distance2
            else:
                continue  # Skip positions that are not label1 or label2

            # Check if the position is part of a connected pair
            connected = any(position in pair for pair in connected_pairs)

            if connected:
                # Adjust common dummies between connected positions
                for pair in connected_pairs:
                    if position in pair:
                        other_position = pair[1] if position == pair[0] else pair[0]
                        common_dummy_keys = set(position.get("ClosestDummies", [])) & set(other_position.get("ClosestDummies", []))
                        for dummy_key in common_dummy_keys:
                            dummy = next((p for p in self.positions if p.get("Key") == dummy_key), None)
                            if dummy:
                                self.adjust_dummy_position(position, other_position, dummy, distance, target_distance)
            else:
                # Adjust dummies using stored direction vectors
                for dummy_key in position.get("ClosestDummies", []):
                    dummy = next((p for p in self.positions if p.get("Key") == dummy_key), None)
                    if dummy:
                        direction_vector = direction_vectors.get((position_key, dummy_key))
                        if direction_vector:
                            self.adjust_dummy_position_single(position, dummy, distance, direction_vector)

        # Save the final rescaled model
        self.save_json(self.output_final_file)
        logger.info(f"Final scaled model saved to: {self.output_final_file}")