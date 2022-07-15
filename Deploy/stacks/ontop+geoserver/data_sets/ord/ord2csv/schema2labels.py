import ord_schema
from ord_schema import reaction_pb2 as re
from typing import Dict, Iterable, List, Optional, Tuple, Type, TypeVar, Union
import csv



def get_field_labels(message: ord_schema.Message, trace: Optional[str] = None, message_label: Optional[str] = None) -> Tuple[(str, List, List, List, List)]:
    """Converts a message to its subfields and stores their names in lists.

    Args:
        message: Proto to convert.
        trace: string; the trace of nested field names.

    Returns:
        a tuple of (this_trace, scalars, messages, maps, enums, key, repeat)
    """
    if trace == None:
        this_trace = message.DESCRIPTOR.name
   
    else:
        this_trace = trace+'_'+message.DESCRIPTOR.name

    messages = []
    repeats = []
    maps = []
    if message_label == 'MAPS':
        scalars = ['reaction_id', 'key']
    elif message_label == 'REPEATED':
        scalars = ['reaction_id', 'item']
    else:
        scalars = ['reaction_id']
    for field in message.DESCRIPTOR.fields:

        if(field.type == field.TYPE_MESSAGE and field.message_type.GetOptions().map_entry):
            key = True
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
        
    return (this_trace,scalars, messages, maps, repeats)







def create_tables(message: ord_schema.Message, trace: Optional[str] = None, message_label: Optional[str] = None):
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
    

    if trace is None:
        trace = ''
    else:
        trace = trace+'_'

    # get this_trace and the lists for scalars, messages, maps, and enums    
    this_trace, scalars, messages, maps, repeats = get_field_labels(message=message, trace=None, message_label=message_label)
    # print(trace, this_trace,scalars, messages, maps, repeats)
    # print(trace,'\n\t',this_trace, message_label)
    
    # ideally create a table here with the name trace+this_trace
    file = open('./results/'+trace+this_trace+'.csv', encoding='utf-8', mode='w', newline='')
    writer = csv.writer(file)
    writer.writerow(scalars)
    file.close()


    for item in messages+maps+repeats:
        if item in maps:
            message_label = 'MAPS' 
        if item in repeats:
            message_label = 'REPEATED'
        #print(trace, this_trace, item, message_label)
        # Weak implementation:
        try:
            # for messages defined in the main schema
            new_message = getattr(re, item)

        except:
            # for nested messages
            new_message = getattr(message, item)

        create_tables(new_message, trace+this_trace, message_label)

def get_fields_values(message: ord_schema.Message, 
                    trace: Optional[str] = None, 
                    reaction_id: Optional[str] = None,
                    message_label: Optional[str] = None, 
                    key_or_item: Optional[Union[str, int]] = None) -> Tuple[(str, List, List, List, Dict)]:
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
    repeats = []
    maps = []
    scalars = {'reaction_id' : reaction_id}
    
    
        
    for field, value in message.ListFields():
        if field.label == field.LABEL_REPEATED:
            if field.type == field.TYPE_MESSAGE and field.message_type.GetOptions().map_entry:
                # possible handling of the map keys:
                for key, subvalue in value.items():
                    #maps.append((field.name, subvalue))
                    maps.append((field.name, key, subvalue))
            else:
                # possible implementation of the the repeat index
                for i, subvalue in enumerate(value):
                    #repeats.append((field.name, subvalue))
                    repeats.append((field.name, i,subvalue))


        else:     
            if field.type == field.TYPE_MESSAGE:
                messages.append((field.name, None, value))
            else:
                if field.type == field.TYPE_ENUM:
                    scalars.update({field.name : field.enum_type.values_by_number[value].name})  
                else:
                    if message_label == 'MAPS':
                        scalars.update({field.name : value ,'key' : key_or_item})
                        #print('key': key_or_item)
                    elif message_label == 'REPEATED':
                        scalars.update({field.name : value , 'item': key_or_item})
                        #print('item': key_or_item)
                    else:
                        scalars.update({field.name : value})

        
    return (this_trace,scalars, messages, maps, repeats)

def populate_tables(message: ord_schema.Message, trace: Optional[str] = None, 
                    reaction_id: Optional[str] = None,
                    message_label: Optional[str] = None,
                    key_or_item: Optional[Union[int,str]] = None):
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
    scalars = {}
    
    if trace is None:
        trace = ''
    else:
        trace = trace+'_'

    # get this_trace and the lists for scalars, messages, maps, and enums    
    
    this_trace, scalar_labels, _, _, _ = get_field_labels(message=message, trace=None, message_label=message_label)
    # print(trace+this_trace,'labels\n',label1+label2)

    this_trace, scalars, messages, maps, repeats = get_fields_values(message=message, trace=None, reaction_id = reaction_id, 
                                                                        message_label=message_label,key_or_item=key_or_item)
    # print(trace+this_trace, scalars, 'ENUMS:',enums)
    #row = []
    row = get_row(scalars, scalar_labels)
   

    # ideally create a table here with the name trace+this_trace
    append_to_file('./results/'+trace+this_trace+'.csv', row)
    #file = open('./results/'+trace+this_trace+'.csv', encoding='utf-8', mode='a', newline='')
    #writer = csv.writer(file)
    #writer.writerow(row)
    #file.close()

    for (field, key_or_item,value) in messages+maps+repeats:
        if (field, key_or_item,value) in maps:
            message_label = 'MAPS'
        if (field, key_or_item,value) in repeats:
            message_label = 'REPEATED'
            #print(this_trace, field, key_or_item)
        # print(this_trace, message_label)
        populate_tables(value, trace+this_trace, reaction_id, message_label, key_or_item)


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






