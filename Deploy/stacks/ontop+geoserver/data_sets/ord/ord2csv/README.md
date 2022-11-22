# ord2csv convertor tool

## Dependencies
### ORD Schema
#### Installation from PyPi
    $ pip install ord-schema
#### Development version
To install in editable/development mode:

    $ git clone https://github.com/open-reaction-database/ord-schema.git
    $ cd ord-schema
    $ pip install -e .

### Google API Python Client
Install the Google API Python Client from PyPi using the following command:

    $ pip install google-api-python-client

### PyTest, Wget, OS, Pandas
    $ pip install pytest wget os-sys pandas

## Important Protocol Buffer (ProtoBuf) Documentation Links
In protocol buffer data defintion each class of information is defined as a `Message`. The `Message` class has its own attributes and methodes as well as a subclass that is called `Descriptor`. `Descriptor` has also very useful methods and attributes that are implemented in the `schema2label.py` library file. The links to the protobuf python API are provided below: 

### Message
[https://googleapis.dev/python/protobuf/latest/google/protobuf/message.html#](https://googleapis.dev/python/protobuf/latest/google/protobuf/message.html#)
### Descriptor
[https://googleapis.dev/python/protobuf/latest/google/protobuf/descriptor.html](https://googleapis.dev/python/protobuf/latest/google/protobuf/descriptor.html)
## Docker container 
Since `ord2csv` has numerous dependencies and running the code might require complicated procedures, a docker solution is also provided. For using the container, first you need to build the image using the following command:
### Building the docker image
    $ docker build --tag ord2csv .

Once the image is ready, you can easily run the image using the following command by mapping the local volumes to the image volumes:
### Running the docker command
    $ docker run -it -v $(pwd)/results:/results -v $(pwd)/inputs:/inputs ord2csv