import xml.etree.ElementTree as ET
import csv
import requests

def fetch_xml_from_url(url):
    response = requests.get(url)
    response.raise_for_status()
    return response.text

def xml_to_csv(xml_data, csv_file_path):
    root = ET.fromstring(xml_data)

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

        for establishment in root.findall('.//EstablishmentDetail'):
            data = {field: '' for field in fieldnames} # Initialize all fields with empty string
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

### URL of the XML file from the Food Standards Agency ###
### please check https://ratings.food.gov.uk/open-data for getting more food hygiene rating data ###

xml_url = 'https://ratings.food.gov.uk/api/open-data-files/FHRS297en-GB.xml'

#### Path for the output CSV file ####
csv_file_path = '/Users/jiyingchen/Desktop/Research/Use Cases/Processed Data/output.csv'
### Relace with your target csv path ####

try:
    xml_content = fetch_xml_from_url(xml_url)
    xml_to_csv(xml_content, csv_file_path)
    print(f"XML data has been successfully converted to CSV and saved to {csv_file_path}")
except Exception as e:
    print(f"An error occurred: {e}")