import ord_schema
from ord_schema import reaction_pb2 as re
from typing import Dict, Iterable, List, Optional, Tuple, Type, TypeVar, Union
import csv



def get_field_labels(message: ord_schema.Message, trace: Optional[str] = None) -> Tuple[(str, List, List, List, List)]:
    """Converts a message to its subfields and stores their names in lists.

    Args:
        message: Proto to convert.
        trace: string; the trace of nested field names.

    Returns:
        a tuple of (this_trace, scalars, messages, maps, enums)
    """
    if trace == None:
        this_trace = message.DESCRIPTOR.name
    else:
        this_trace = trace+'_'+message.DESCRIPTOR.name

    messages = []
    enums = []
    maps = []
    scalars = ['reaction_id']
    

    for field in message.DESCRIPTOR.fields:
        if(field.type == field.TYPE_MESSAGE and field.message_type.GetOptions().map_entry):
            map_value = field.message_type.fields_by_name["value"]
            maps.append(map_value.message_type.name)
        elif(field.type == field.TYPE_MESSAGE and not field.message_type.GetOptions().map_entry):
            messages.append(field.message_type.name)
        elif(field.type == field.TYPE_ENUM):
            # prints the enum type name
            # enums.append(field.enum_type.name)
            # print the enum field name
            enums.append(field.name)
        else:
            scalars.append(field.name)
        
    return (this_trace,scalars, messages, maps, enums)







def create_tables(message: ord_schema.Message, trace: Optional[str] = None):
    """Converts a message to its equivalent csv tables.

    Args:
        message: Proto to convert.
        trace: string; the trace of nested field names.

    Returns:
        empty csv tables in the results folder with headers.
    """


    # this_trace = '\t'+trace
    messages = []
    enums = []
    maps = []
    scalars = []
    
    if trace is None:
        trace = ''
    else:
        trace = trace+'_'

    # get this_trace and the lists for scalars, messages, maps, and enums    
    this_trace, scalars, messages, maps, enums = get_field_labels(message)
    
    # ideally create a table here with the name trace+this_trace
    file = open('./results/'+trace+this_trace+'.csv', encoding='utf-8', mode='w', newline='')
    writer = csv.writer(file)
    writer.writerow(scalars+enums)
    file.close()

    for item in messages+maps:
        # Weak implementation:
        try:
            # for messages defined in the main schema
            new_message = getattr(re, item)

        except:
            # for nested messages
            new_message = getattr(message, item)

        create_tables(new_message, this_trace)

def get_fields_values(message: ord_schema.Message, trace: Optional[str] = None, reaction_id: Optional[str] = None) -> Tuple[(str, List, List, List, Dict)]:
    """Converts a message to its subfields and subvalues.

    Args:
        message: Proto to convert.
        trace: string; the trace of nested field names.
        reaction_id: the global reaction identifier.

    Returns:
        a tuple of (trace, scalars, messages, maps, and enums)
    """
    


    if trace == None:
        this_trace = message.DESCRIPTOR.name
    else:
        this_trace = trace+'_'+message.DESCRIPTOR.name

    messages = []
    enums = []
    maps = []
    scalars = {'reaction_id' : reaction_id}
    
    
        
    for field, value in message.ListFields():
        if field.label == field.LABEL_REPEATED:
            if field.type == field.TYPE_MESSAGE and field.message_type.GetOptions().map_entry:
                # possible handling of the map keys:
                for key, subvalue in value.items():
                    maps.append((field.name, subvalue))
            else:
                # possible implementation of the the repeat index
                for i, subvalue in enumerate(value):
                    messages.append((field.name, subvalue))


        else:     
            if field.type == field.TYPE_MESSAGE:
                messages.append((field.name, value))
            else:
                if field.type == field.TYPE_ENUM:
                    scalars.update({field.name : field.enum_type.values_by_number[value].name})  
                else:
                    scalars.update({field.name : value})

        
    return (this_trace,scalars, messages, maps, enums)

def populate_tables(message: ord_schema.Message, trace: Optional[str] = None, reaction_id: Optional[str] = None):
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
    enums = []
    maps = []
    scalars = {}
    
    if trace is None:
        trace = ''
    else:
        trace = trace+'_'

    # get this_trace and the lists for scalars, messages, maps, and enums    
    
    this_trace, label1, _, _, label2 = get_field_labels(message, None)
    # print(trace+this_trace,'labels\n',label1+label2)

    this_trace, scalars, messages, maps, enums = get_fields_values(message, None, reaction_id)
    # print(trace+this_trace, scalars, 'ENUMS:',enums)
    #row = []
    row = get_row(scalars, label1+label2)
   

    # ideally create a table here with the name trace+this_trace
    append_to_file('./results/'+trace+this_trace+'.csv', row)
    #file = open('./results/'+trace+this_trace+'.csv', encoding='utf-8', mode='a', newline='')
    #writer = csv.writer(file)
    #writer.writerow(row)
    #file.close()

    for (field, value) in messages+maps:
        populate_tables(value, this_trace, reaction_id)


def get_row(scalars: Dict[str, str], labels: List[str]) -> List[str]:
    row = []
    for item in labels:
        if item in scalars.keys():
           row.append(scalars[item])
        else:
           row.append(None)
    return row

def append_to_file(file: str, row: List[str]):
    file = open(file, encoding='utf-8', mode='a', newline='')
    writer = csv.writer(file)
    writer.writerow(row)
    file.close()






