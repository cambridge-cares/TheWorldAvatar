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