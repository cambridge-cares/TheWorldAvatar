import xml.etree.ElementTree as ET
import csv
import requests
import json
from tqdm import tqdm  

def fetch_xml_from_url(url):
    """Fetch XML data (in this case is food hygiene data provided by UK Food Standards Agency) from the provided URL."""
    response = requests.get(url)
    response.raise_for_status()
    return response.text

def xml_to_csv(xml_data, csv_file_path):
    root = ET.fromstring(xml_data)

    # Field names from the XML structure
    fieldnames = set()
    for establishment in root.findall('.//EstablishmentDetail'):
        for child in establishment:
            if child.tag == 'Geocode':
                fieldnames.add('Latitude')
                fieldnames.add('Longitude')
            else:
                fieldnames.add(child.tag)

    with open(csv_file_path, mode='w', newline='', encoding='utf-8') as file:
        csv_writer = csv.DictWriter(file, fieldnames=fieldnames)
        csv_writer.writeheader()

        # Find all establishments and prepare progress bar
        establishments = root.findall('.//EstablishmentDetail')
        for establishment in tqdm(establishments, desc="Processing establishments", unit="record"):
            data = {field: '' for field in fieldnames}  # Initialize all fields with empty string
            for child in establishment:
                if child.tag == 'Geocode':
                    latitude = child.find('Latitude').text if child.find('Latitude') is not None else ''
                    longitude = child.find('Longitude').text if child.find('Longitude') is not None else ''
                    data['Latitude'] = latitude
                    data['Longitude'] = longitude
                else:
                    text = ' '.join(child.text.split()) if child.text else ''
                    data[child.tag] = text
            csv_writer.writerow(data)

def load_config(json_path):
    """Load configuration from a JSON file."""
    with open(json_path, 'r') as file:
        config = json.load(file)
    return config

if __name__ == "__main__":

    # Path to the configuration file
    config_path = 'xml_to_csv.json'

    try:
        config = load_config(config_path)
        xml_url = config.get("xml_url")
        csv_file_path = config.get("csv_file_path")

        if not xml_url or not csv_file_path:
            raise ValueError("Both 'xml_url' and 'csv_file_path' must be provided in the configuration.")

        # Fetch XML and convert to CSV
        xml_content = fetch_xml_from_url(xml_url)
        xml_to_csv(xml_content, csv_file_path)
        print(f"XML data has been successfully converted to CSV and saved to {csv_file_path}")
    except Exception as e:
        print(f"An error occurred: {e}")
