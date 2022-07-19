from optparse import Option
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
    scalars = ['reaction_id','parent_message','parent_key', 'parent_item']
    for field in message.DESCRIPTOR.fields:

        if(field.type == field.TYPE_MESSAGE and field.message_type.GetOptions().map_entry):
            map_value = field.message_type.fields_by_name["value"]
            maps.append(map_value.message_type.name)
        elif(field.type == field.TYPE_MESSAGE and not field.message_type.GetOptions().map_entry):
            if(field.label == field.LABEL_REPEATED): 
                repeats.append(field.message_type.name)
            else:
                messages.append(field.message_type.name)
        #elif(field.type == field.TYPE_ENUM):
            # prints the enum type name
            # enums.append(field.enum_type.name)
            # print the enum field name
            # enums.append(field.name)
        else:
            scalars.append(field.name)
        
    return (scalars, messages, maps, repeats)







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
    row = []

    # get this_trace and the lists for scalars, messages, maps, and enums    
    scalars, messages, maps, repeats = get_field_labels(message=message)
    # adding all the column labels to the header array, row
    row = ['reaction_id', 'parent_message', 'parent_key_index', 'current_key_index', 'trace','label', 'field_name', 'value', 'child_key_index']

    
    # create a table for each independent message
    file = open('./results/'+message.DESCRIPTOR.name+'.csv', encoding='utf-8', mode='w', newline='')
    writer_1 = csv.writer(file)
    writer_1.writerow(row)
    file.close()

    for item in messages+maps+repeats:

        try:
            # for messages defined in the main schema
            new_message = getattr(re, item)

        except:
            # for nested messages
            new_message = getattr(message, item)

        create_tables(new_message)

def get_fields_values(message: ord_schema.Message, 
                    trace: Optional[str] = None, 
                    reaction_id: Optional[str] = None,
                    parent_name: Optional[str] = None,
                    parent_key_index: Optional[str] = None, 
                    current_key_index: Optional[int] = None) -> Tuple[(str, List, List, List, Dict)]:
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
    scalars = []
    
        
    for field, value in message.ListFields():
        if field.label == field.LABEL_REPEATED:
            if field.type == field.TYPE_MESSAGE and field.message_type.GetOptions().map_entry:
                # possible handling of the map keys:
                for key, subvalue in value.items():
                    #maps.append((field.name, subvalue))
                    maps.append((field.message_type.fields_by_name["value"].message_type.name, key, subvalue))
                    scalars.append((parent_name, parent_key_index, current_key_index, trace, 'MAP', field.name, field.message_type.fields_by_name["value"].message_type.name, key))
            else:
                # possible implementation of the the repeat index
                for i, subvalue in enumerate(value):
                    #repeats.append((field.name, subvalue))
                    repeats.append((field.message_type.name, i,subvalue))
                    scalars.append((parent_name, parent_key_index, current_key_index, trace, 'REPEATED', field.name, field.message_type.name, i))

        else:     
            if field.type == field.TYPE_MESSAGE:
                messages.append((field.message_type.name, None, value))
                scalars.append((parent_name, parent_key_index, current_key_index, trace, 'MESSAGE', field.name, field.message_type.name, None))
            else:
                if field.type == field.TYPE_ENUM:
                    scalars.append((parent_name, parent_key_index, current_key_index, trace, 'SCALAR', field.name, field.enum_type.values_by_number[value].name, None))
                else:
                    scalars.append((parent_name, parent_key_index, current_key_index, trace, 'SCALAR', field.name, value, None))
        
        
    return (trace,scalars, messages, maps, repeats)

def populate_tables(message: ord_schema.Message, trace: Optional[str] = None, 
                    reaction_id: Optional[str] = None,
                    parent_name: Optional[str] = None,
                    parent_key_index: Optional[str] = None,
                    current_key_index: Optional[str] = None):
    """Converts a message to its scalar subfields and write them into corresponding csv files.

    Args:
        message: Proto to convert.
        trace: string; the trace of nested field names.
        reaction_id: the global indentifier of the reaction

    Returns:
        updated csv files in the results folder
    """

    # this_trace = '\t'+trace
    messages = []
    repeats = []
    maps = []
    scalars = []

    
    if trace is None:
        trace = message.DESCRIPTOR.name


    _, scalars, messages, maps, repeats = get_fields_values(message=message, trace=trace, reaction_id = reaction_id, 
                                                                        parent_name=parent_name, current_key_index=current_key_index, 
                                                                        parent_key_index=parent_key_index)

    # append the data to the corresponding table
    append_to_file(file='./results/'+message.DESCRIPTOR.name+'.csv', reaction_id=reaction_id, scalars=scalars, message_name=parent_name)


    for (field, new_key_index,value) in messages+maps+repeats:
        # set the trace based on message type
        if new_key_index is None:
            this_trace = field
        else:
            this_trace = field+'['+str(new_key_index)+']'

        populate_tables(message=value, trace=trace+'.'+this_trace, 
        reaction_id=reaction_id, parent_name=message.DESCRIPTOR.name, 
        parent_key_index=current_key_index, current_key_index=new_key_index)


def get_row(scalars: Dict[str, str], labels: List[str]) -> List[str]:
    row = []
    for item in labels:
        if item in scalars.keys():
           row.append(scalars[item])
        else:
           row.append(None)
    return row



def append_to_file(file: str, reaction_id: str, message_name: str, scalars: List[Tuple]):
    file = open(file, encoding='utf-8', mode='a', newline='')
    writer = csv.writer(file)
    for (parent_name, parent_key_index, current_key_index, trace,label, field_name, value, child_key_index) in scalars:
        row = [reaction_id, parent_name, parent_key_index, current_key_index, trace,label, field_name, value, child_key_index]
        writer.writerow(row)
    file.close()






