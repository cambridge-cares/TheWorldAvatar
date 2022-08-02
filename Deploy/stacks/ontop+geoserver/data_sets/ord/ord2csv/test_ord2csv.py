from genericpath import isfile
import pytest
import schema2labels as sl
import ord_schema
from ord_schema import reaction_pb2 as re
import os

# test the get_field_label function
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

    assert counter == 154