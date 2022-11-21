# Import modules
import ord_schema
from ord_schema import message_helpers
from ord_schema.proto import dataset_pb2
import os
import wget
import schema2labels as st





def ord2csv():
   # Datafile name
   pb = 'ord_dataset-fc83743b978f4deea7d6856deacbfe53.pb.gz'
   # If the file does not exist in the directory, download it
   if (not os.path.isfile("./inputs/"+pb)):
      # Download dataset from ord-data
      url = "https://github.com/open-reaction-database/ord-data/blob/main/data/fc/"+pb+"?raw=true"
      pb = wget.download(url=url, out="./inputs/")
      print(pb)
   else:
      pb = './inputs/'+pb

   
   
   # Load the data; message_helpers is part of the ORD schema
   data = message_helpers.load_message(pb, dataset_pb2.Dataset)
   
   
   # Create the csv files by providing a message sample (an instance of a populated message or and initilized message)
   st.create_tables(data.reactions[0])
   
   
   #  Populate the csv tables using the data of each reaction
   # ID stores the index value of the most recent message that is stored in csv files
   ID = {} # key: message name, value: index
   # VALUE stores the unique key:value pairs to ensure avoiding repetion of literals
   VALUE = {} # key: a tuple of all literal values of a message, value : index of the literal value 
   
   # Loop over all the reactions inside the dataset
   
   for i, reaction in enumerate(data.reactions):
      # i is the index of the root message; in this case, reaction
      # Converts each reaction in the dataset to equivalent csv tables 
      st.populate_tables(reaction, ID, VALUE,i+1)


if __name__ == "__main__":
   ord2csv()