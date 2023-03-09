# Import modules
from unittest import skip
import ord_schema
from ord_schema import reaction_pb2 as re
from typing import Dict, List, Optional, Tuple
import csv



def get_field_labels(message: ord_schema.Message) -> Tuple[(List, List, List, List)]:
    """Converts a message to its subfields and stores their names in lists.

    Args:
        message: Proto to convert.
        trace: string; the trace of nested field names.

    Returns:
        a tuple of (scalars, messages, maps, repeats)
    """

    messages = []
    repeats = []
    maps = []
    scalars = ['ID']
    for field in message.DESCRIPTOR.fields:

        if(field.type == field.TYPE_MESSAGE and field.message_type.GetOptions().map_entry):
            map_value = field.message_type.fields_by_name["value"]
            maps.append((map_value.message_type.name, field.name))
        elif(field.type == field.TYPE_MESSAGE and not field.message_type.GetOptions().map_entry):
            if(field.label == field.LABEL_REPEATED): 
                repeats.append((field.message_type.name, field.name))
            else:
                messages.append((field.message_type.name, field.name))
        # Enums are also considered as scalars; if it is required to store them
        # separatly, the following block of code should be uncommented and the enums
        # list should be defined         
        #elif(field.type == field.TYPE_ENUM):
            # prints the enum type name
            # enums.append(field.enum_type.name)
            # print the enum field name
            # enums.append(field.name)
        else:
            scalars.append(field.name)
        
    return (scalars, messages, maps, repeats)


def create_file(name: str, scalars: List):
    """Creates a csv file with the provided name and scalars as columns.

    Args:
        name: csv file name.
        scalars: string; column labels of scalar values.

    Returns:
        an empy csv file in results directory
    """
    file = open('./results/'+name+'.csv', encoding='utf-8', mode='w', newline='')
    writer = csv.writer(file, quotechar='\"', quoting=csv.QUOTE_MINIMAL)
    writer.writerow(scalars)
    file.close()    




def create_tables(message: ord_schema.Message):
    """Converts a message to its equivalent csv tables.

    Args:
        message: Proto to convert.

    Returns:
        empty csv tables in the results folder with headers.
    """


    # this_trace = '\t'+trace
    messages = []
    repeats = []
    maps = []
    scalars = []


    # get this_trace and the lists for scalars, messages, maps, and enums    
    scalars, messages, maps, repeats = get_field_labels(message=message)
    # adding all the column labels to the header array, row

    
    # create a table for each independent message
    create_file(name=message.DESCRIPTOR.name, scalars=scalars)


    for (field, field_name) in messages+maps+repeats:
        header = ['ID', message.DESCRIPTOR.name+'_ID', field+'_ID', 'key_or_index' ]
        create_file(name=message.DESCRIPTOR.name+'_'+field_name+'_'+field, scalars=header)
        try:
            # for messages defined in the main schema
            new_message = getattr(re, field)

        except:
            # for nested messages
            new_message = getattr(message, field)


        create_tables(new_message)


def get_fields_values(message: ord_schema.Message) -> Tuple[(Dict, Tuple , List, List, List)]:
    """Converts a message to its subfields and subvalues.

    Args:
        message: Proto to convert.

    Returns:
        a tuple of (scalars_dict, scalars_tuple, messages, maps, repeats)
    """

    messages = []
    repeats = []
    maps = []
    scalars_dict = {}
    scalars_list = [message.DESCRIPTOR.name]

    
    # loop over fileds and field values of the input message    
    for field, value in message.ListFields():
        # check if the field is repeated
        if field.label == field.LABEL_REPEATED:
            # check if the field is of type dictionary
            if field.type == field.TYPE_MESSAGE and field.message_type.GetOptions().map_entry:
                # possible handling of the map keys:
                for key, subvalue in value.items():
                    #maps.append((field.name, subvalue))
                    maps.append((field.message_type.fields_by_name["value"].message_type.name, field.name, key, subvalue))
            # regular repeated field
            else:
                # possible implementation of the the repeat index
                for i, subvalue in enumerate(value):
                    #repeats.append((field.name, subvalue))
                    repeats.append((field.message_type.name, field.name,i,subvalue))

        # regular messages and scalars
        else:
            # regular message     
            if field.type == field.TYPE_MESSAGE:
                messages.append((field.message_type.name, field.name, None, value))
            # enums and scalars
            else:
                # enums
                if field.type == field.TYPE_ENUM:
                    scalars_dict.update({field.name : field.enum_type.values_by_number[value].name})
                    scalars_list.append(field.enum_type.values_by_number[value].name)
                # scalars
                else:
                    scalars_dict.update({field.name : value})
                    scalars_list.append(value)
        
        
    return (scalars_dict, tuple(scalars_list), messages, maps, repeats)

def populate_tables(message: ord_schema.Message, ID: Optional[Dict] = None, LITERAL_VALUE: Optional[Dict] = None, root_index : Optional[int] = None):
    """Converts a message to its scalar subfields and write them into corresponding csv files.

    Args:
        message: Proto to convert.
        ID: Dict; stores the most up-to-date ID of each message key: message name, value: index
        LITERAL_VALUE: Dict; stores the ID of unique scalars to avoid repetion of entries in message files key: scalar_tuple, value: index
        root_index: int; the index of the most up stream message

    Returns:
        updated csv files in the results folder
    """

    if root_index is not None:
        ID.update({message.DESCRIPTOR.name : root_index})


    messages = []
    repeats = []
    maps = []
    scalars_dict = {}
    scalars_tuple = ()

    
    # Get the scalar values (Dict, Tuple) as well as messages, maps, and repeated messages of the input message
    scalars_dict, scalars_tuple,messages, maps, repeats = get_fields_values(message=message)

    if root_index is not None:

        # Add the root index to the ID dictionary
        ID.update({message.DESCRIPTOR.name : root_index})
        # Add the the root index to the scalars dictionary
        scalars_dict.update({'ID' : 'I'+str(ID[message.DESCRIPTOR.name])})
        # Get the target csv table column labels
        labels, _,_,_ = get_field_labels(message=message)
        # Get the row of current message's scalars that will be weitten in the csv file
        row = get_row(scalars=scalars_dict, labels=labels)
    
    
        # Avoid repetition in the Scalar tables
        if (scalars_tuple in LITERAL_VALUE.keys()):
            skip
        # The items that are new to the LITERAL_VALUE dictionary
        else:
            # Add the item to the dictionry
            LITERAL_VALUE.update({scalars_tuple : ID[message.DESCRIPTOR.name]})
            # Append the row to the csv file
            append_to_file(file_name=message.DESCRIPTOR.name, row=row)


    # Loop over the submessages (values) of the current message and read their scalars for the indexing purpose
    for (message_name, field_name,key_or_index,value) in messages+maps+repeats:

        # Get the target csv table column labels
        labels, _,_,_ = get_field_labels(message=value)
        # Get the scalar values (Dict, Tuple) as well as messages, maps, and repeated messages of the input message = value
        scalars_dict, scalars_tuple,messages, maps, repeats = get_fields_values(message=value)
        # Check if the current message (value) exists in the ID dictionary
        if value.DESCRIPTOR.name in ID.keys():
            ID[value.DESCRIPTOR.name] += 1
        # Add the new message (value) in the dictionary
        else:
            ID.update({ value.DESCRIPTOR.name : 1})

        scalars_dict.update({'ID' : 'I'+str(ID[value.DESCRIPTOR.name])})
        # Get the row of current value's scalars that will be weitten in the csv file
        row = get_row(scalars=scalars_dict, labels=labels)
    
    
        # Avoid Repetition in the Scalar tables
        if (scalars_tuple in LITERAL_VALUE.keys()):
            skip
        # If the scalar value is new
        else:
            # Add the imem to the dictionry
            LITERAL_VALUE.update({scalars_tuple : ID[value.DESCRIPTOR.name]})
            # Append the row to the csv file
            append_to_file(file_name=value.DESCRIPTOR.name, row=row)        



        # The first index of the intermediary tables:
        # Check if the name of the intermidary table exists in the ID dictionary
        if message.DESCRIPTOR.name+'_'+field_name+'_'+message_name in ID.keys():
            ID[message.DESCRIPTOR.name+'_'+field_name+'_'+message_name] +=1
        # Add the name of the new intermediary table to the ID dictionary    
        else:
            ID.update({message.DESCRIPTOR.name+'_'+field_name+'_'+message_name : 1})
        
        # Get the row for intermidary tables
        row = ['I'+str(ID[value.DESCRIPTOR.name]), 'I'+str(ID[message.DESCRIPTOR.name]), 'I'+str(LITERAL_VALUE[scalars_tuple]), '['+str(key_or_index)+']']

        # Append the row intermediary table
        append_to_file(file_name=message.DESCRIPTOR.name+'_'+field_name+'_'+message_name, row=row)


        # call the recursive function
        populate_tables(message=value, ID=ID, LITERAL_VALUE=LITERAL_VALUE)


def get_row(scalars: Dict[str, str], labels: List[str]) -> List[str]:
    row = []
    for item in labels:
        if item in scalars.keys():
           row.append(scalars[item])
        else:
           row.append(None)
    return row



def append_to_file(file_name: str, row: list):
    file = open(file='./results/'+file_name+'.csv', encoding='utf-8', mode='a', newline='')
    writer = csv.writer(file, quotechar='\"', quoting=csv.QUOTE_MINIMAL)
    writer.writerow(row)
    file.close()






