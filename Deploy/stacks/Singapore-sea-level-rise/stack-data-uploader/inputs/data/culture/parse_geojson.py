import json
from bs4 import BeautifulSoup
import re
import uuid
import argparse


# Function to parse HTML table and extract data as properties
def parse_html_table(html_string):
    soup = BeautifulSoup(html_string, 'html.parser')
    
    results = {}
    
    for row in soup.findAll('tr'):
        col = row.findAll('th')
        val = row.findAll('td')
        
        if col[0].get('colspan') is None:
            results[col[0].string] = val[0].string
    
    return results

def main(argv):
    r =argv.file_path

    # Load GeoJSON file
    with open(r, 'r') as f:
        geojson_data = json.load(f)

    address_regex = r'(\d+)\s(.+)'

    # Process each feature in the GeoJSON
    

    for feature in geojson_data['features']:
        properties = feature['properties']
        properties['original_description'] = properties.pop('Description')
        properties['name_id'] = properties.pop('Name')
        
        html_table_string = properties.get('original_description', None)  # Extract HTML table string
        

        if html_table_string:
            # Parse HTML table data and add as new properties to the feature
            table_data = parse_html_table(html_table_string)
            
            if table_data:
                if 'ADDRESS' in table_data:
                    if table_data['ADDRESS'] is not None:
                        match = re.search(address_regex, table_data['ADDRESS'])
                        if match:
                            number = match.group(1)
                            street = match.group(2)
                            table_data['addressblockhousenumber'] = number
                            table_data['addressstreetname'] = street
                if 'POSTALCODE' in table_data:
                    table_data['addresspostalcode'] = table_data.pop('POSTALCODE')

                for column_name, column_value in table_data.items():
                    column_name = column_name.lower()
                    if column_name == 'name' and 'heritagetree' in r:
                        column_name = 'scientificname'
                    new_property_name = f"{column_name}"
                    properties[new_property_name] = column_value
                
                properties['uuid'] = str(uuid.uuid4())
                
                if (properties.get('addressunitnumber') is not None) or (properties.get('addressblockhousenumber') is not None) or (properties.get('addressstreetname') is not None) or (properties.get('addresspostalcode') is not None):
                    properties['address_uuid'] = str(uuid.uuid4())
                else:
                    properties['address_uuid'] = None

    # Save modified GeoJSON with new properties
    with open(r.replace('.geojson', '_clean.geojson'), 'w') as outfile:
        json.dump(geojson_data, outfile, indent=2)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    # add arguments to the parser
    parser.add_argument("file_path")

    # parse the arguments
    args = parser.parse_args()
    main(args)