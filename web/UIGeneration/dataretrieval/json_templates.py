################################################
# Authors: Feroz Farazi (msff2@cam.ac.uk)      #    
# Date: 28 Feb 2023                            #
################################################

# The purpose of this module is to provide templates for
# all different types of JSON objects

import json 

def get_json_object(title, description, type):
   
    data = {       
        "title": title,
        "description": description,
        "type": type    
    }

    return data

def get_json_object_with_enum(title, description, type, enum):
   
    data = {       
        "title": title,
        "description": description,
        "type": type,
        "enum": [
            enum
        ]       
    }

    return data

def get_json_object_with_properties(title, description, type):
   
    data = {       
        "title": title,
        "description": description,
        "type": type,
        "properties": {}
    }

    return data

def get_json_object_with_unit(description_of_value, description_of_unit, enum):
   
    data = {       
        "value": {
            "type": "number",
            "title": "Value",
            "description": description_of_value
        },
        "unit": {
            "type": "string",
            "title": "Unit",
            "description": description_of_unit,
            "enum": [
                enum
            ]       
        }
    }

    return data
