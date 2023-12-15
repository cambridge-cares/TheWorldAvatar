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
            fieldnames.add(child.tag)


    with open(csv_file_path, mode='w', newline='', encoding='utf-8') as file:
        csv_writer = csv.DictWriter(file, fieldnames=fieldnames)
        csv_writer.writeheader()

        for establishment in root.findall('.//EstablishmentDetail'):
            data = {field: '' for field in fieldnames} # Initialize all fields with empty string
            for child in establishment:
                text = ' '.join(child.text.split()) if child.text else ''
                data[child.tag] = text
            csv_writer.writerow(data)

### URL of the XML file from the Food Standards Agency ###
### please check https://ratings.food.gov.uk/open-data for getting more food hygiene rating data ###

xml_url = 'https://ratings.food.gov.uk/api/open-data-files/FHRS297en-GB.xml'

#### Path for the output CSV file ####
csv_file_path = '/Users/jiyingchen/Desktop/Research/HOMEWORK/Use Cases/Processed Data/output.csv'
### Relace with your target csv path ####
try:
    # Fetching XML content from URL
    xml_content = fetch_xml_from_url(xml_url)

    # Converting XML to CSV
    xml_to_csv(xml_content, csv_file_path)
    print(f"XML data has been successfully converted to CSV and saved in {csv_file_path}")
except Exception as e:
    print(f"An error occurred: {e}")