import csv
import os
import re
import sys

# Define the processing directory path two levels up from the current file
PROCESSING_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir, os.pardir))
# Add the processing directory to the system path
sys.path.append(PROCESSING_DIR)

# Constants
CSV_FILE_PATH       = os.path.join(PROCESSING_DIR, "Data", "r2_mops.csv")
QUERY_ENDPOINT      = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
UPDATE_ENDPOINT     = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
KG_USER             = 'bg_user'
KG_PASSWORD         = 'admin'

def get_csv_rows(csv_file_path):
    """Yield rows from the CSV file."""
    with open(csv_file_path, mode='r') as file:
        csv_reader = csv.DictReader(file)
        for row in csv_reader:
            yield row
def split_geometry(geometry) ->str:
    """Deduce GBU by splitting the Geometry value string into the two parts, 
        e.g. (4-planar)x12(2-bent)x24 => GBU1 = 4-planar, GBU 2 = 2-bent"""
    pattern             = re.compile(r'\(([^)]+)\)x(\d+)')
    gbu_num             = pattern.findall(geometry)
    return gbu_num[0], gbu_num[1]
