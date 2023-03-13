################################################
# Authors: Feroz Farazi (msff2@cam.ac.uk)      #    
# Date: 28 Feb 2023                            #
################################################

# The purpose of this module is to provide templates for
# all different types of JSON objects

import json 

def get_json_property_values(title, description, type):
   
    data = {       
        "title": title,
        "description": description,
        "type": type    
    }

    return data

def get_json_object_property(title, description, type):
   
    data = {       
        "title": title,
        "description": description,
        "type": type,
        "properties": {}
    }

    return data


    # return '''
    #         "%s": {
    #         "title": "%s",
    #         "description": "%s",
    #         "type": "%s"
    #     }
    # ''' %(name, title, description, type)

def get_json_property_values_with_enum(title, description, type, enum):
   
    data = {       
        "title": title,
        "description": description,
        "type": type,
        "enum": [
            enum
        ]       
    }

    return data



    # return '''
    #         "%s": {
    #         "title": "%s",
    #         "description": "%s",
    #         "type": "%s",
    #         "enum": [
    #             %s
    #         ]
    #     }
    # ''' %(name, title, description, type, enum)