__author__ = "Aleksandar Kondinski"
__license__ = "MIT" 
__version__ = '0.1.0' 
__status__ = "development" 

import os
from cof_logic.cof_workflow import COFProcessor

if __name__ == "__main__":
    script_location = os.path.dirname(os.path.abspath(__file__))
    processor = COFProcessor(script_location=script_location)
    
    # read the data
    processor.cof_data_reader()
    
    # specify output path and extension
    output_path = os.path.join(script_location, 'Data', 'Generated_COFs')
    output_ext = 'extxyz'
    
    # create output path if it does not exist
    os.makedirs(output_path, exist_ok=True)
    
    # extract parameters and perform logic on data
    processor.cof_parameter_extractor(output_path, output_ext)
