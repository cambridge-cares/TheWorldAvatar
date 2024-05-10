import csv
import json
import os
import uuid
import re

class COFs2JSON:
    
    def __init__(self, base_dir='cof_data'):
        self.base_dir = base_dir
        os.makedirs(self.base_dir, exist_ok=True)

    def parse_stacking(self, name3):
        if 'AA' in name3:
            return 'AA'
        elif 'AB' in name3:
            return 'AB'
        return None

    def parse_string(self, input_str, existing_ids=None):
        existing_ids = existing_ids or {'GBUs': {}, 'LGBUs': {}}
        framework_type = re.search(r"^\w+", input_str).group(0)
        periodicity_match = re.search(r"\](\w)$", input_str)
        periodicity = periodicity_match.group(1) if periodicity_match else 'Unknown'
        periodicity_type = 'Continuous' if periodicity == 'n' else 'Discrete'
        inner_content = re.search(r"\[(.*?)\]", input_str).group(1)

        patterns = re.findall(r"\((.*?)\)x(\d+)", inner_content)
        gb_us = []
        lg_us = []

        for pattern in patterns:
            uuid_key = (f"LGBU_{uuid.uuid4()}" if pattern[0].startswith('L:') else f"GBU_{uuid.uuid4()}")
            existing_ids['LGBUs' if pattern[0].startswith('L:') else 'GBUs'][pattern[0]] = uuid_key
            (lg_us if pattern[0].startswith('L:') else gb_us).append({
                "UUID": uuid_key,
                "Label": pattern[0], 
                "Number": int(pattern[1])
            })

        return {
            'framework_type': framework_type,
            'GBUs': gb_us,
            'LGBUs': lg_us,
            'Periodicity': periodicity_type
        }

    def create_cof_json(self, csv_file, json_file):
        data_list = []
        combinations_seen = {}
        assembly_model_to_iri = {}
        assembly_model_data = {}

        with open(os.path.join(self.base_dir, csv_file), mode='r', newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                precursors = [p for p in [row['Precursor1'], row['Precursor2']] if p != "0"]
                if len(precursors) < int(row['NumPrecursors']):
                    precursors.append("0")

                combination = (row['Linkage'], row['Assembly_Model'], tuple(sorted(precursors)))
                stacking_type = self.parse_stacking(row['Name3'])

                if combination in combinations_seen:
                    data_list[combinations_seen[combination]]["Crystal_Occurrences"].append({
                        "COF_Crystal_UUID": f"COF_Crystal_{uuid.uuid4()}",
                        "DOI": row['DOI'],
                        "Long_Name": row['Name3'],
                        "Stacking": stacking_type
                    })
                else:
                    cof_uuid = f"COF_{uuid.uuid4()}"
                    crystal_occurrence = {
                        "COF_Crystal_UUID": f"COF_Crystal_{uuid.uuid4()}",
                        "DOI": row['DOI'],
                        "Long_Name": row['Name3'],
                        "Stacking": stacking_type
                    }

                    if row['Assembly_Model'] not in assembly_model_to_iri:
                        am_uuid = f"Assembly_Model_{uuid.uuid4()}"
                        parsed_am = self.parse_string(row['Assembly_Model'])
                        assembly_model_to_iri[row['Assembly_Model']] = am_uuid
                        assembly_model_data[am_uuid] = {
                            "AM_UUID": am_uuid,
                            "AM_Long_Formula": row['Assembly_Model'],
                            "AM_Short_Formula": row['AM_Short'],
                            **parsed_am
                        }

                    am_data = assembly_model_data[assembly_model_to_iri[row['Assembly_Model']]]

                    json_obj = {
                        "COF": cof_uuid,
                        "AssemblyModel": am_data,
                        "Number_of_Precursors": int(row['NumPrecursors']),
                        "Linkage": row['Linkage'],
                        "Precursor": precursors,
                        "Crystal_Occurrences": [crystal_occurrence]
                    }
                    data_list.append(json_obj)
                    combinations_seen[combination] = len(data_list) - 1

        with open(os.path.join(self.base_dir, json_file), 'w', encoding='utf-8') as jsonfile:
            json.dump(data_list, jsonfile, indent=4)


    def create_precursor_json(self, csv_file, json_file):
        data = {}

        with open(csv_file, 'r') as file:
            csv_reader = csv.DictReader(file)
            for row in csv_reader:
                linkage = row["Precursor"]
                if linkage not in data:
                    data[linkage] = {
                        "Precursor_UUID": f"COF_Precursor_{uuid.uuid4()}",
                        "Precursor_Label": linkage,
                        "GBU": row["GBU"],
                        "ConstructingMol": row["ConstructingMol"],
                        "inp_File": row["inp"],
                        "UnitFrom": row["UnitFrom"],
                        "hasBindingSite": []
                    }
                binding_site = {
                    "Binding_Site_Label": row["bindingSite"],
                    "bsIndex": row["bsIndex"],
                    "Dummies": row["Dummies"],
                    "Dentation": row["Dentation"],
                    "Complementaries": row["Complementaries"]
                }
                data[linkage]["hasBindingSite"].append(binding_site)

        with open(json_file, 'w') as file:
            json.dump(list(data.values()), file, indent=4)
            

    def create_linkage_json(self, csv_file, json_file):
        data = {}

        with open(csv_file, 'r') as file:
            csv_reader = csv.DictReader(file)
            for row in csv_reader:
                linkage = row["Linkage"]  # Corrected the key here
                if linkage not in data:
                    data[linkage] = {
                        "LFR_UUID": f"Linkage_{uuid.uuid4()}",
                        "LFR_Label": linkage,
                        "LGBU": row["GBU"],
                        "ConstructingMol": row["ConstructingMol"],
                        "inp_File": str(linkage) + ".inp",
                        "UnitFrom": row["UnitFrom"],
                        "hasBindingSite": []
                    }
                binding_site = {
                    "Binding_Site_Label": row["bindingSite"],
                    "bsIndex": row["bsIndex"],
                    "Dummies": row["Dummies"],
                    "Dentation": row["Dentation"],
                    "Complementaries": row["Complementaries"]
                }
                data[linkage]["hasBindingSite"].append(binding_site)

        with open(json_file, 'w') as file:
            json.dump(list(data.values()), file, indent=4)


    def load_json(self, file_name):
        file_path = os.path.join(self.base_dir, file_name)
        with open(file_path, 'r') as f:
            return json.load(f)

    def merge_json_data(self):
        cof_data = self.load_json('output_json/temp_cofs.json')
        linkage_data = self.load_json('output_json/linkages.json')
        precursor_data = self.load_json('output_json/precursors.json')

        for cof in cof_data:
            linkage_label = cof['Linkage']
            linkage_dict = self.get_linkage_dict(linkage_label, linkage_data)
            if linkage_dict:
                cof['Linkage'] = linkage_dict
            
            precursor_labels = cof['Precursor']
            precursor_dicts = []
            for precursor_label in precursor_labels:
                precursor_dict = self.get_precursor_dict(precursor_label, precursor_data)
                if precursor_dict:
                    precursor_dicts.append(precursor_dict)
            cof['Precursor'] = precursor_dicts

        self.save_json(cof_data, 'output_json/cofs.json')

    def get_linkage_dict(self, linkage_label, linkage_data):
        for linkage in linkage_data:
            if linkage['LFR_Label'] == linkage_label:
                return linkage
        return None

    def get_precursor_dict(self, precursor_label, precursor_data):
        for precursor in precursor_data:
            if precursor['Precursor_Label'] == precursor_label:
                return precursor
        return None

    def save_json(self, data, file_name):
        file_path = os.path.join(self.base_dir, file_name)
        with open(file_path, 'w') as f:
            json.dump(data, f, indent=4)
            

    def run_all(self):
        self.create_cof_json('input_csv/COFs_CLEAN.csv', 'output_json/temp_cofs.json')
        self.create_linkage_json('cof_data/input_csv/Linkages_inp_2.csv', 'cof_data/output_json/linkages.json')
        self.create_precursor_json('cof_data/input_csv/Precursors_inp.csv', 'cof_data/output_json/precursors.json')
        self.merge_json_data()
        print("All JSON files have been created and merged successfully.")


# Usage
if __name__ == "__main__":
    cof = COFs2JSON()
    cof.run_all()
