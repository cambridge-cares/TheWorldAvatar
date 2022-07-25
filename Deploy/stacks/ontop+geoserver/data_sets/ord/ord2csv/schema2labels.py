from optparse import Option
from unittest import skip
from matplotlib.pyplot import sca
import ord_schema
from ord_schema import reaction_pb2 as re
from typing import Dict, Iterable, List, Optional, Tuple, Type, TypeVar, Union
import csv



def get_field_labels(message: ord_schema.Message) -> Tuple[(List, List, List, List)]:
    """Converts a message to its subfields and stores their names in lists.

    Args:
        message: Proto to convert.
        trace: string; the trace of nested field names.

    Returns:
        a tuple of (this_trace, scalars, messages, maps, enums, key, repeat)
    """

    messages = []
    repeats = []
    maps = []
    scalars = ['ID']
    for field in message.DESCRIPTOR.fields:

        if(field.type == field.TYPE_MESSAGE and field.message_type.GetOptions().map_entry):
            map_value = field.message_type.fields_by_name["value"]
            maps.append((map_value.message_type.name, map_value.name))
        elif(field.type == field.TYPE_MESSAGE and not field.message_type.GetOptions().map_entry):
            if(field.label == field.LABEL_REPEATED): 
                repeats.append((field.message_type.name, field.name))
            else:
                messages.append((field.message_type.name, field.name))
        #elif(field.type == field.TYPE_ENUM):
            # prints the enum type name
            # enums.append(field.enum_type.name)
            # print the enum field name
            # enums.append(field.name)
        else:
            scalars.append(field.name)
        
    return (scalars, messages, maps, repeats)


def create_file(name: str, scalars):
    file = open('./results/'+name+'.csv', encoding='utf-8', mode='w', newline='')
    writer_1 = csv.writer(file)
    writer_1.writerow(scalars)
    file.close()    




def create_tables(message: ord_schema.Message, message_label: Optional[str] = None):
    """Converts a message to its equivalent csv tables.

    Args:
        message: Proto to convert.
        trace: string; the trace of nested field names.

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
    #file = open('./results/'+message.DESCRIPTOR.name+'.csv', encoding='utf-8', mode='w', newline='')
    #writer_1 = csv.writer(file)
    #writer_1.writerow(scalars)
    #file.close()

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
        trace: string; the trace of nested field names.
        reaction_id: the global reaction identifier.

    Returns:
        a tuple of (trace, scalars, messages, maps, and enums)
    """
    


    #if trace == None:
    #    this_trace = message.DESCRIPTOR.name
    #else:
    #    this_trace = trace+'_'+message.DESCRIPTOR.name

    messages = []
    repeats = []
    maps = []
    scalars_dict = {}
    scalars_list = [message.DESCRIPTOR.name]

    
        
    for field, value in message.ListFields():
        if field.label == field.LABEL_REPEATED:
            if field.type == field.TYPE_MESSAGE and field.message_type.GetOptions().map_entry:
                # possible handling of the map keys:
                for key, subvalue in value.items():
                    #maps.append((field.name, subvalue))
                    maps.append((field.message_type.fields_by_name["value"].message_type.name, field.message_type.fields_by_name["value"].name, key, subvalue))
            else:
                # possible implementation of the the repeat index
                for i, subvalue in enumerate(value):
                    #repeats.append((field.name, subvalue))
                    repeats.append((field.message_type.name, field.name,i,subvalue))


        else:     
            if field.type == field.TYPE_MESSAGE:
                messages.append((field.message_type.name, field.name, None, value))
            else:
                if field.type == field.TYPE_ENUM:
                    scalars_dict.update({field.name : field.enum_type.values_by_number[value].name})
                    scalars_list.append(field.enum_type.values_by_number[value].name)
                else:
                    scalars_dict.update({field.name : value})
                    scalars_list.append(value)
        
        
    return (scalars_dict, tuple(scalars_list), messages, maps, repeats)

def populate_tables(message: ord_schema.Message, ID: Optional[Dict] = None, LITERAL_VALUE: Optional[Dict] = None, root_index : Optional[int] = None):
    """Converts a message to its scalar subfields and write them into corresponding csv files.

    Args:
        message: Proto to convert.
        trace: string; the trace of nested field names.
        reaction_id: the global indentifier of the reaction

    Returns:
        updated csv files in the results folder
    """

    #if message.DESCRIPTOR.name in ID.keys():
    #    ID[message.DESCRIPTOR.name] +=1
    #else:
    #    ID.update({message.DESCRIPTOR.name : 1})
    
    if root_index is not None:
        ID.update({message.DESCRIPTOR.name : root_index})


    # this_trace = '\t'+trace
    messages = []
    repeats = []
    maps = []
    scalars_dict = {}
    scalars_tuple = ()

    

    #labels, _,_,_ = get_field_labels(message=message)
    scalars_dict, scalars_tuple,messages, maps, repeats = get_fields_values(message=message)
    #scalars_dict.update({'ID' : ID[message.DESCRIPTOR.name]})


    #row = get_row(scalars=scalars_dict, labels=labels)

    # append the data to the corresponding table
    #print(message.DESCRIPTOR.name,'\n', scalars_tuple)

    # Avoid Repetition in the Scalar tables
    #if (scalars_tuple in LITERAL_VALUE.keys()):
    #    skip
    #else:
    #    LITERAL_VALUE.update({scalars_tuple : ID[message.DESCRIPTOR.name]})
    #    append_to_file(file='./results/'+message.DESCRIPTOR.name+'.csv', row=row)



    for (field, field_name,key_or_index,value) in messages+maps+repeats:
        # The ID of the literal value in submessage (value)
        labels, _,_,_ = get_field_labels(message=value)
        scalars_dict, scalars_tuple,messages, maps, repeats = get_fields_values(message=value)
        if value.DESCRIPTOR.name in ID.keys():
            ID[value.DESCRIPTOR.name] += 1
        else:
            ID.update({ value.DESCRIPTOR.name : 1})

        scalars_dict.update({'ID' : ID[value.DESCRIPTOR.name]})
    
    
        row = get_row(scalars=scalars_dict, labels=labels)
    
    
        # Avoid Repetition in the Scalar tables
        if (scalars_tuple in LITERAL_VALUE.keys()):
            skip
        else:
            LITERAL_VALUE.update({scalars_tuple : ID[value.DESCRIPTOR.name]})
            append_to_file(file='./results/'+value.DESCRIPTOR.name+'.csv', row=row)        



        #if field in ID.keys():
        #    ID[field]+=1
        #else:
        #    ID.update({field : 1})
        # The first index of the intermediary tables:
        if message.DESCRIPTOR.name+'_'+field_name+'_'+field in ID.keys():
            ID[message.DESCRIPTOR.name+'_'+field_name+'_'+field] +=1
        else:
            ID.update({message.DESCRIPTOR.name+'_'+field_name+'_'+field : 1})
        
        # get the row for intermidary tables
        row = [ID[message.DESCRIPTOR.name+'_'+field_name+'_'+field], ID[message.DESCRIPTOR.name], LITERAL_VALUE[scalars_tuple], key_or_index]

        
        append_to_file(file='./results/'+message.DESCRIPTOR.name+'_'+field_name+'_'+field+'.csv', row=row)



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



def append_to_file(file: str, row: list):
    file = open(file, encoding='utf-8', mode='a', newline='')
    writer = csv.writer(file, quotechar='\"', quoting=csv.QUOTE_MINIMAL)
    writer.writerow(row)
    file.close()






