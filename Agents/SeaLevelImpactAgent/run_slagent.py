import csv
import subprocess

# Define the path to the CSV file
csv_file = ('inputs/input_request.csv')

with open(csv_file, mode='r') as file:
    csv_reader = csv.DictReader(file)
    
    for row in csv_reader:
        ssp = row.get('ssp', 'default_ssp')
        projectionyear = row.get('projectionyear', 'default_projectionyear')
        confidence = row.get('confidence', 'default_confidence')
        quantile = row.get('quantile', 'default_quantile')
        
        # Create the curl command
        curl_command = (
            f'curl -X POST "localhost:3838/sealevelimpactagent/slrimpact?'
            f'ssp={ssp}&projectionyear={projectionyear}&confidence={confidence}&quantile={quantile}"'
        )
        
        # Execute the curl command
        try:
            subprocess.run(curl_command, shell=True, check=True)
            print(f'Successfully executed: {curl_command}')
        except subprocess.CalledProcessError as e:
            print(f'Error executing: {curl_command}\n{e}')
