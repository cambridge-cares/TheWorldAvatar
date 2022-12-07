from genericpath import isfile
import pytest
import schema2labels as sl
import ord_schema
from ord_schema import reaction_pb2 as re
import os
import csv
import ord2csv as oc
import pandas as pd

# test the get_field_label function
@pytest.mark.get_file_labels
def test_get_field_labels():
    test_message = re.Reaction()
    scalars, messages, maps, repeats = sl.get_field_labels(message=test_message)

    assert repeats == [('ReactionIdentifier', 'identifiers'), ('ReactionObservation', 'observations'), ('ReactionWorkup','workups'),('ReactionOutcome', 'outcomes')]
    assert maps == [('ReactionInput', 'inputs')]
    assert messages == [('ReactionSetup', 'setup'), ('ReactionConditions', 'conditions'), ('ReactionNotes', 'notes'), ('ReactionProvenance', 'provenance')]
    assert scalars ==['ID', 'reaction_id']

    test_message = re.ReactionWorkup()
    scalars, messages, maps, repeats = sl.get_field_labels(message=test_message)

    assert repeats == []
    assert maps == []
    assert messages == [('Time', 'duration'), ('ReactionInput', 'input'), ('Amount', 'amount'), 
    ('TemperatureConditions', 'temperature'), ('StirringConditions', 'stirring')]
    assert scalars ==['ID', 'type', 'details', 'keep_phase', 'target_ph', 'is_automated']



# test the create_tables function
@pytest.mark.create_tables
def test_create_tables(): 
    counter = 0
    test_dir = "./results/"

    # remove the content of the results folder
    files = os.listdir(test_dir)
    for item in files:
        os.remove(test_dir+item)


    
    # call the table_create function
    test_message = re.Reaction()
    sl.create_tables(test_message)


    files = os.listdir(test_dir)

    for item in files:
        if os.path.isfile(test_dir+item): counter +=1
    
    # based on the structure of the ORD schema, there should be 154 tables in the results directory 
    assert counter == 154

    # remove the content of the results folder
    files = os.listdir(test_dir)
    for item in files:
        os.remove(test_dir+item)


# test the create_file function
@pytest.mark.create_file
def test_create_file():
    path = "./results/"
    file_name = "test_file"
    file_extension = ".csv"
    scalars = ['ID', 'Column1', 'Column2', 'Column2']
    # call the create_file function to create a file
    sl.create_file(name=file_name, scalars=scalars)
    # check if the file exsits
    assert os.path.isfile(path+file_name+file_extension)

    # check if the scalars list is available in the file
    with open(path+file_name+file_extension, encoding = 'utf-8', mode='r') as file:
        csvrows = csv.reader(file)
        for row in csvrows:
            assert row == scalars
        
    # remove the test file
    os.remove(path+file_name+file_extension)
    # check if the file is removed properly
    assert not os.path.isfile(path+file_name+file_extension)

# test the indexing
@pytest.mark.indexing
def test_indexing():

    path = './results/'

    # remove the content of the results folder
    files = os.listdir(path)
    for item in files:
        os.remove(path+item)

    # convert the pb file to csv files    
    oc.ord2csv()
    
    # load csv files to dataframes
    df_0 = pd.read_csv(path+"Reaction.csv")
    df_1 = pd.read_csv(path+"Reaction_inputs_ReactionInput.csv")
    df_2 = pd.read_csv(path+"ReactionInput_components_Compound.csv")
    df_3 = pd.read_csv(path+"Compound_identifiers_CompoundIdentifier.csv")
    assert df_0.iloc[18]['ID'] == df_1.iloc[56]['Reaction_ID']
    assert df_1.iloc[56]['ID'] == df_2.iloc[130]['ReactionInput_ID']
    assert df_2.iloc[130]['ID'] == df_3.iloc[241]['Compound_ID']
    # assert df_4[df_4['ID'] == df_3.iloc[241]['CompoundIdentifier_ID']]['type'].iloc[0] == 'NAME'
    # assert df_4[df_4['ID'] == df_3.iloc[241]['CompoundIdentifier_ID']]['value'].iloc[0] == 'THF'