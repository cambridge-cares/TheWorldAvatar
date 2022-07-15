# Import modules
from email.encoders import encode_noop
from encodings import utf_8
import ord_schema
from ord_schema import message_helpers, validations
from ord_schema.proto import dataset_pb2



import pandas as pd
import numpy as np
import os
import wget

import schema2labels as st


# Download dataset from ord-data

url = "https://github.com/open-reaction-database/ord-data/blob/main/data/1b/ord_dataset-1b3d2b114de1429e9b70c3b1c16c9263.pb.gz?raw=true"
# pb = wget.download(url)
# pb = 'ord_dataset-1b3d2b114de1429e9b70c3b1c16c9263.pb.gz'

# Deoxyfluorinatoin data
url = "https://github.com/open-reaction-database/ord-data/blob/main/data/fc/ord_dataset-fc83743b978f4deea7d6856deacbfe53.pb.gz?raw=true"
# pb = wget.download(url)
pb = 'ord_dataset-fc83743b978f4deea7d6856deacbfe53.pb.gz'


# Load Dataset message
data = message_helpers.load_message(pb, dataset_pb2.Dataset)


# Create the csv files

Reaction = ord_schema.reaction_pb2.Reaction()
ReactionIdentifier = ord_schema.reaction_pb2.ReactionIdentifier()
Compound = ord_schema.reaction_pb2.Compound()
ReactionProvenance = ord_schema.reaction_pb2.ReactionProvenance()

#st.create_tables(Reaction)
st.create_tables(data.reactions[0])
#  Populate the csv tables using the data of each reaction
for reaction in data.reactions:

   # Converts each reaction in the dataset to equivalent csv tables 
   st.populate_tables(reaction,None, reaction.reaction_id)





