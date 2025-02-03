##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 04 Dec 2020                      #
##########################################

"""This module is designed to convert entities of any domain and their data and metadata into RDF.
It requires the entities and their data to be provided as inputs in an ABox CSV template file."""

import io
import os
import csv
from pathlib import Path as PathlibPath
import textwrap
from rdflib import Graph, URIRef
from rdflib.extras.infixowl import OWL_NS
from rdflib.namespace import Namespace, XSD, RDF, RDFS
import entityrdfizer.aboxgenerator.PropertyReader as propread
import entityrdfizer.aboxgenerator.ABoxGeneration as aboxgen
from entityrdfizer.aboxgenerator import tboxtools

'''
There are two traditional ways to form the IRI for instances:
1) HASH-separated:
    if abox address contains "#" at the end
2) SLASH-separated
    if abox address contains "/" at the end
Depending on the abox and tbox address in the header.
'''

"""
Optional TBox to verify ABox entities vs classes and properties in TBox.
Requires module tboxtools.
Not fully implemented yet. """
tBox = None

"""Declared column headers as constants"""
HEADERS = ['Source', 'Type', 'Target', 'Relation', 'Value', 'Data Type']
TOTAL_NO_OF_COLUMNS = len(HEADERS)
COLUMN_LETTERS = "ABCDEF"

"""Predefined types source entries"""
TYPE_ONTOLOGY = 'Ontology'
TYPE_INSTANCE = 'Instance'
TYPE_DATA = 'Data Property'

"""Utility constants"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
HTTP = 'http://'
HTTPS = 'https://'

# TODO change this setting depending on the Ontology provided:
# Allowed values: "USE_SLASH" and "USE_HASH".
CLASS_TO_HTTP = "USE_SLASH"
INSTANCE_TO_HTTP = "USE_SLASH"


def standard_data_types():
    """This function defines all known (pre-defined) data types.
    Any other type must have a full http:// IRI address.
    The structure of the dictionary:
    - key: the data type as in the XSD/RDF specification, but without prefix.
    - value is a list:
      - value[0] - full URI name (using rdflib.namespace to convert),
      - value[1] - same as key, but in lowcase,
      - value[2] - short prefix + name in lowcase.
    The short names (like 'integer' or 'rdfs:literal' must be in low case,
    even if originally it has upper case.
    In csv file these types can be in either case.
    """

    output = {}
    xsd = ['anyURI', 'base64Binary', 'boolean', 'byte', 'dateTime',
           'dateTimeStamp', 'decimal', 'double', 'float', 'hexBinary', 'int',
           'integer', 'language', 'long', 'Name', 'NCName', 'negativeInteger',
           'NMTOKEN', 'nonNegativeInteger', 'nonPositiveInteger',
           'normalizedString', 'positiveInteger', 'short', 'string', 'token'
           'unsignedByte', 'unsignedInt', 'unsignedLong', 'unsignedShort',
           'date', 'gYearMonth', 'gMonthDay', 'gYear', 'gMonth', 'gDay']
    for key in xsd:
        output[key] = [getattr(XSD, key), key.lower(), 'xsd:' + key.lower()]
    rdf = ['langString', 'PlainLiteral', 'XMLLiteral']
    for key in rdf:
        output[key] = [getattr(RDF, key), key.lower(), 'rdf:' + key.lower()]
    rdfs = ['Literal']
    # Note: this is list of Properties, not Data Types:
    rdfs += ['label', 'domain', 'range', 'comment', 'seeAlso', 'isDefinedBy',
             'member', 'Datatype', 'Resource', 'Class']
    for key in rdfs:
        output[key] = [getattr(RDFS, key), key.lower(), 'rdfs:' + key.lower()]

    return output

DATA_TYPES = standard_data_types()

""" Global variables """
"""Declared an array to maintain the list of already created instances"""
instances = {}
instancesShort = {}
g = Graph()
SHOW_WARNING = True
WARNING_COUNT = 0


def is_empty(value):
    """True if the argument is empty string, or not a string"""
    global WARNING_COUNT
    if isinstance(value, str):
        if value.strip() is None or value.strip() == '':
            return True
    else:
        if SHOW_WARNING:
            print(f"Error: Invalid type of '{value}', expected a string.")
        WARNING_COUNT += 1
        return True
    return False


def is_http(value):
    """ Return True if the input is a string looking like an http address
    (starting from http://) """
    if not isinstance(value, str):
        return False
    if value.strip().lower().startswith(HTTP) or \
       value.strip().lower().startswith(HTTPS):
        return True
    return False


def instance_to_http(value, file_line):
    """Convert the class instance into a full IRI address"""
    global WARNING_COUNT
    if is_empty(propread.getTBoxIRI().strip()):
        if SHOW_WARNING:
            print(f"Warning: Abox is not defined, for the instance {file_line}.")
        WARNING_COUNT += 1
    if not isinstance(value, str):
        if SHOW_WARNING:
            print(f"Error in instace_to_http(): input must be str. Got: '{value}'.")
        WARNING_COUNT += 1

    if INSTANCE_TO_HTTP == "USE_SLASH":
        full_path = propread.getABoxIRI() + SLASH + format_iri(str(value).strip())
    elif INSTANCE_TO_HTTP == "USE_HASH":
        full_path = propread.getABoxIRI() + HASH + format_iri(str(value).strip())
    else:
        print("Error in instance_to_http(): unknown " +
              f"INSTANCE_TO_HTTP = {INSTANCE_TO_HTTP} {file_line}")
        WARNING_COUNT += 1
    return full_path


def class_to_http(value, file_line):
    """Convert the class name into a full IRI address"""
    global WARNING_COUNT
    if is_empty(propread.getTBoxIRI().strip()):
        if SHOW_WARNING:
            print(f"Warning: Tbox is not defined, for the class name {file_line}.")
        WARNING_COUNT += 1
    if not isinstance(value, str):
        if SHOW_WARNING:
            print("Error in class_to_http(): input must be str. " +
                  f"Got: '{value}' {file_line}.")
        WARNING_COUNT += 1

    if CLASS_TO_HTTP == "USE_SLASH":
        full_path = propread.getTBoxIRI() + SLASH + format_iri(str(value))
    elif CLASS_TO_HTTP == "USE_HASH":
        full_path = propread.getTBoxIRI() + HASH + format_iri(str(value))
    else:
        print("Error in class_to_http(): unknown " +
              f"CLASS_TO_HTTP = {CLASS_TO_HTTP} {file_line}")
        WARNING_COUNT += 1
    return full_path


def set_tbox_iri(value, file_line):
    """Assign the tbox address, prints warnings for repeated assignments"""
    global WARNING_COUNT, CLASS_TO_HTTP
    if is_empty(value.strip()):
        if SHOW_WARNING:
            print(f"Warning: Empty ontology name {file_line}.")
        WARNING_COUNT += 1
        return
    old = propread.getTBoxIRI()
    if not is_empty(old):
        if SHOW_WARNING:
            print(f"Warning: Over-written ontology name '{old}' {file_line}.")
            if old.strip() != value.strip():
                print(f"         New tbox address is '{value}'.")
        WARNING_COUNT += 1
    value = value.strip()

    if value.endswith("/"):
        CLASS_TO_HTTP = "USE_SLASH"
        propread.setTBoxIRI(value[:-1])
    elif value.endswith("#"):
        CLASS_TO_HTTP = "USE_HASH"
        propread.setTBoxIRI(value[:-1])
    else:
        CLASS_TO_HTTP = "USE_SLASH"
        propread.setTBoxIRI(value)


def set_abox_iri(value, file_line):
    """Assign the abox address, prints warnings for repeated assignments"""
    global WARNING_COUNT, INSTANCE_TO_HTTP
    if is_empty(value.strip()):
        if SHOW_WARNING:
            print(f"Warning: Empty ontology name {file_line}.")
        WARNING_COUNT += 1
        return
    old = propread.getABoxIRI()
    if not is_empty(old):
        if SHOW_WARNING:
            print(f"Warning: Over-written ontology name '{old}' {file_line}.")
            if old.strip() != value.strip():
                print(f"         New abox address is '{value}'.")
        WARNING_COUNT += 1
    value = value.strip()

    if value.endswith("/"):
        INSTANCE_TO_HTTP = "USE_SLASH"
        propread.setABoxIRI(value[:-1])
    elif value.endswith("#"):
        INSTANCE_TO_HTTP = "USE_HASH"
        propread.setABoxIRI(value[:-1])
    else:
        INSTANCE_TO_HTTP = "USE_SLASH"
        propread.setABoxIRI(value)


def is_header_valid(row, file_line):
    """This function checks the validity of header in the input ABox CSV file"""
    global WARNING_COUNT
    output = True

    if len(row) < TOTAL_NO_OF_COLUMNS:
        if SHOW_WARNING:
            print("Error: Csv headers must have at least " +
                  f"{TOTAL_NO_OF_COLUMNS} columns {file_line}.")
        WARNING_COUNT += 1
        return False

    for i in range(TOTAL_NO_OF_COLUMNS):
        if row[i].strip().lower() != HEADERS[i].lower():
            if SHOW_WARNING:
                print(f"Error: In column {COLUMN_LETTERS[i]} the header must " +
                      f"be '{HEADERS[i]}', but got '{row[i]}' {file_line}.")
            WARNING_COUNT += 1
            output = False

    return output


def split_name_uuid(value, file_line):
    """Extract the NAME and UUID from the full name,
    assuming the instance notation follows the pattern 'NAME_UUID4' as recommended in
    https://www.dropbox.com/scl/fi/hivvg0qsms7tp2dsne641/2023-08-02-Abox.pptx?rlkey=ip5k1qj5rcsj9kqo5ptw3xojg&dl=0
    Note: UUID4 has a standard structure: 8char-4char-4char-4char-12char
    """
    global WARNING_COUNT
    if len(value) > 36:
        name = value[:-37]
        uuid = value[-36:].lower()
        if uuid[8] == "-" and uuid[13] == "-" and uuid[18] == "-" and uuid[23] == "-":
            if value[-37] != "_":
                if SHOW_WARNING:
                    print("Warning: Missing '_' as NAME_UUID in instance" +
                          f" '{value}' {file_line}.")
                WARNING_COUNT += 1
            if uuid[14] != "4" or uuid[19] not in "89ab":
                if SHOW_WARNING:
                    print(f"Warning: Instance '{value}' uses UUID version",
                          f"other than UUID4 {file_line}.")
                WARNING_COUNT += 1
        elif value.startswith("http://www.ontology-of-units-of-measure.org") or \
             value.startswith("om:"):
            # Do nothing, these don't need to follow NAME_UUID pattern
            pass
        else:
            if SHOW_WARNING:
                print(f"Warning: Instance '{value}' does not follow pattern" +
                      f" NAME_UUID {file_line}.")
            WARNING_COUNT += 1
    else:
        if SHOW_WARNING:
            print(f"Warning: Instance '{value}' does not follow pattern" +
                  f" NAME_UUID {file_line}.")
        WARNING_COUNT += 1
        name = value
        uuid = ""

    return name, uuid


def check_existing_instances(value, file_line):
    """Check for duplicates in the instance names."""
    global WARNING_COUNT
    if value.strip() in instances:
        if SHOW_WARNING:
            print(f"Warning: Repeatedly initialized instance '{value.strip()}'" +
                  f" {file_line}.")
        WARNING_COUNT += 1
    name, uuid = split_name_uuid(value, file_line)
    return

    # Redundant. Instance with the same base name may have different UUID:
    if uuid != "":
        # This is a UUID4 type of instance name
        for inst in instances:
            short, _ = split_name_uuid(inst, "")
            if short == name and value != inst:
                if SHOW_WARNING:
                    print("Warning: Instance with same name but different UUID: " +
                          f"'{value}', existing instance: '{inst}' {file_line}.")
                WARNING_COUNT += 1


def save_instances(filename):
    """Saves all used instances for manual/visual inspection. Expects csv filename."""
    global WARNING_COUNT
    _, ext = os.path.splitext(filename)
    if ext != ".csv":
        if SHOW_WARNING:
            print("Warning: the file to save instances should have .csv extension,",
                  f"but got '{filename}'.")
        WARNING_COUNT += 1
    with open(filename, "w", encoding="utf-8") as f:
        for key, value in instances.items():
            f.write(key + "," + value + "\n")


def process_data(row, file_line):
    """This function converts a row into an entity or a link between
       two entities or a data or annotation property value"""
#   1. Checking input parameters:
    global WARNING_COUNT, instances, tBox
    if len(row) < TOTAL_NO_OF_COLUMNS:
        if SHOW_WARNING:
            print(f"Warning: Skipping an incomplete or empty line {file_line}.")
        WARNING_COUNT += 1
        return
    # Below this line the number of values in row is at least TOTAL_NO_OF_COLUMNS.

    if is_empty(row[0]) or is_empty(row[1]) or is_empty(row[2]):
        if SHOW_WARNING:
            print(f"Warning: Skipping line {file_line} due to empty Col A or B or C.")
        WARNING_COUNT += 1
        return
    # Below this line Cols A,B,C are NOT empty, no need to check again.

#   2. Processing
#   2a) TYPE_ONTOLOGY
    if row[1].strip().lower() == TYPE_ONTOLOGY.lower():
        if not is_empty(row[3]) and is_empty(row[4]) and is_empty(row[5]):
            if is_http(row[2]):
                if row[3].strip().lower() == 'http://www.w3.org/2002/07/owl#imports':
                    g.set((g.identifier, OWL_NS['imports'], URIRef(row[2])))
                    # Sets the IRI of the TBox:
                    set_tbox_iri(row[2], file_line)
                    # Sets the name of instance of Ontology as the ABox File Name:
                    propread.setABoxFileName(row[0])
                elif row[3].strip().lower() == 'base':
                    set_abox_iri(row[2], file_line)
                else:
                    if SHOW_WARNING:
                        print(f"Warning: Invalid value '{row[3]}' " +
                              f"{file_line} Col D. Expected 'base' or " +
                              "'http://www.w3.org/2002/07/owl#imports'.")
                    WARNING_COUNT += 1
            else:
                if SHOW_WARNING:
                    print(f"Warning: Invalid value '{row[2]}' " +
                          f"{file_line} Col C. Expected an http:// address.")
                WARNING_COUNT += 1

        else:
            if SHOW_WARNING:
                print("Warning: Invalid ontology definition,",
                      f"expect input in Col D and empty Col E,F {file_line}.")
            WARNING_COUNT += 1

#   2b) TYPE_INSTANCE: instance of class and relation between class instances
    elif row[1].strip().lower() == TYPE_INSTANCE.lower():
        if is_empty(row[3]) and is_empty(row[4]) and is_empty(row[5]):
            # rows 3,4,5 are empty, i.e. this is an instance of class
            if is_http(row[0]):
                instance = row[0]
                http_flag = True
            else:
                instance = instance_to_http(row[0], file_line)
                http_flag = False
            if is_http(row[2]):
                full_type = row[2]
            else:
                full_type = class_to_http(row[2], file_line)
            if http_flag or propread.readInstanceLabelCreationOption().strip().lower() == 'no':
                aboxgen.create_instance_without_name(g, URIRef(full_type), URIRef(instance))
            else:
                aboxgen.create_instance(g, URIRef(full_type), URIRef(instance), row[0])
            check_existing_instances(row[0], file_line)
            instances[row[0].strip()] = row[2].strip()

            if tBox:
                if not tBox.isKnownClass(row[2]):
                    if SHOW_WARNING:
                        print(f"Warning: class {row[2]} is not defined in TBox.")
                    WARNING_COUNT += 1
        # End of instance_of_class option, below only relation between instances

        elif is_http(row[2]) or row[2].strip() in instances:
            if row[0] not in instances:
                if SHOW_WARNING:
                    print(f"Warning: Value {row[0]} in Cell A is not defined {file_line}.")
                WARNING_COUNT += 1
                return
            # Instance linking will be skipped, if no relation is provided in the relation column:
            if is_empty(row[3]):
                if SHOW_WARNING:
                    print(f"Warning: Skipped line due to empty Cell D {file_line}.")
                WARNING_COUNT += 1
                return

            if is_http(row[0]):
                row0_http = row[0].strip()
            else:
                row0_http = instance_to_http(row[0], file_line)
            if is_http(row[2]):
                row2_http = row[2].strip()
            else:
                row2_http = instance_to_http(row[2], file_line)

            if is_http(row[3]):
                row3_http = row[3].strip()
            else:
                #row3_http = instance_to_http(row[3], file_line)
                row3_http = class_to_http(row[3], file_line)
                # FIXME: class_to_instance should be flexible to use HASH/SLASH.
                #print(f"Warning: using instance instead of class {row[3]} {file_line}.")
                #WARNING_COUNT += 1

            aboxgen.link_instance(g, URIRef(row3_http),
                                     URIRef(row0_http), URIRef(row2_http))

            if tBox:
                #if row[0] not in instances:
                #    if SHOW_WARNING:
                #        print(f"Warning: '{row[0]}' is not defined in tbox {file_line}")
                #    WARNING_COUNT += 1
                #if row[2] not in instances:
                #    if SHOW_WARNING:
                #        print(f"Warning: '{row[2]}' is not defined in tbox {file_line}")
                #    WARNING_COUNT += 1
                if row[0] in instances and row[2] in instances:
                    tmp = tBox.checkTriple(instances[row[0]], row[3],
                                           instances[row[2]], file_line)
                    if SHOW_WARNING and tmp > 0:
                        print(f"Warning: {tmp} in tBox.checkTriple {file_line}.")
                    WARNING_COUNT += tmp

        else:  # Warnings only:
            if not row[2].strip() in instances:
                if SHOW_WARNING:
                    print(f"Warning: Undefined '{row[2]}' {file_line} Col C.")
                WARNING_COUNT += 1
            elif not is_http(row[2]):
                if SHOW_WARNING:
                    print(f"Warning: Unknown value '{row[2]}' {file_line} Col C.")
                WARNING_COUNT += 1
            else:
                if SHOW_WARNING:
                    print(f"Unknown warning {file_line}.")
                WARNING_COUNT += 1

    elif row[1].strip().lower() == TYPE_DATA.lower():
        if is_empty(row[4]):
            if SHOW_WARNING:
                print(f"Warning: Col E is empty {file_line}.")
            WARNING_COUNT += 1
            return

        prop = get_data_type(row[0], file_line)
        if is_http(prop):
            prop = prop.strip()
        elif ":" in prop:
            # Do nothing
            pass
        else:
            prop = class_to_http(prop, file_line)

        if is_http(row[2]):
            instance = row[2].strip()
        elif row[2].strip() in instances:
            instance = instance_to_http(row[2], file_line)
        else:
            if SHOW_WARNING:
                print(f"Warning: Invalid Col C '{str(row[2])}' {file_line}.",
                      "Expected a previously defined instance or an http:// address.")
            WARNING_COUNT += 1
            instance = row[2]

        if is_empty(row[5]):
            aboxgen.link_data(g, URIRef(prop),
                              URIRef(instance), row[4].strip())
        else:
            aboxgen.link_data_with_type(g, URIRef(prop),
                                        URIRef(instance), row[4].strip(),
                                        get_data_type(row[5], file_line))

        if tBox:
            if row[2] in instances:
                tmp = tBox.checkTriple(instances[row[2]], row[0], row[5], file_line)
                if SHOW_WARNING and tmp > 0:
                    print(f"Warnings: {tmp} in tBox.checkTriple {file_line}.")
                WARNING_COUNT += tmp
            else:
                if SHOW_WARNING:
                    print(f"Warnings: '{row[2]}' not defined in tbox {file_line}.")
                WARNING_COUNT += 1

    else:
        if SHOW_WARNING:
            print(f"Warning: Invalid value '{row[1]}' {file_line} Col B.",
                  "Expected one of: 'Ontology', 'Instance', 'Data Property'.")
        WARNING_COUNT += 1


def get_data_type(data_type, file_line):
    """Returns the corresponding data type syntax for a given data type"""
    global WARNING_COUNT
    low_case = data_type.strip().lower()
    for key, value in DATA_TYPES.items():
        if low_case in value[1:]:
            return value[0]
    if not is_http(data_type):
        if SHOW_WARNING:
            print(f"Warning: data type '{data_type}' is not predefined.",
                  f"Expected a full http:// iri address {file_line}.")
        WARNING_COUNT += 1
    return data_type


def format_iri(iri):
    """Formats an IRI string to discard characters that are not allowed in an IRI"""
    iri = iri.replace(",", " ")
    iri = iri.replace(" ", "")
    return iri


def create_namespace(IRI):
    """Converts an IRI into a namespace"""
    # print(IRI)
    return Namespace(IRI)


def convert_into_rdf(input_file_path, output_file_path=None, tbox_file_path=None):
    """This function checks the validity of the CSV template header and iterates
    over each data row until the whole content of the template is converted into RDF.
    Some example input and output file paths are provided below:
    input_file_path = "C:/.../TheWorldAvatar/JPS_Ontology/KBTemplates/ABox/ABoxOntoSpecies.csv"
    output_file_path = "C:/.../TheWorldAvatar/JPS_Ontology/KBTemplates/ABoxRDFFiles"
    """
    global WARNING_COUNT, tBox
    input_path = PathlibPath(input_file_path)
    input_name = os.path.basename(input_file_path)
    # Check if the input file path exists. If no file, skip further processing:
    if not os.path.exists(input_path):
        if SHOW_WARNING:
            print(f"Error! Input ABox file does not exist: '{input_file_path}'.")
        WARNING_COUNT += 1
        return

    # Checks if the output file path exists. If the path does not exist, create the path:
    if output_file_path:
        output_file_path = PathlibPath(output_file_path)
        if not os.path.exists(output_file_path):
            os.makedirs(output_file_path)
    else:
        output_file_path = os.path.dirname(input_file_path)
    output_file_path = os.path.join(output_file_path, input_name+propread.readABoxFileExtension())

    # FIXME Read the tbox files as input parameters.
    # tbox_file_path = "ontocrystal.csv"
    # tbox_file_path = ["ontocrystal.csv", "ontozeolite.csv"]  # <= ontocrystal must be read first
    # tbox_file_path = ["ontozeolite.csv", "ontocrystal.csv"]

    if tbox_file_path:
        tBox = tboxtools.TBoxTools()
        tmp = tBox.readExcelList(tbox_file_path)
        if tmp > 0:
            print(f"Got {tmp} errors while reading tbox file(s).")
        tBox.parseInputData()
        if tBox.is_empty():
            tBox = None

        #if os.path.isfile(tbox_file_path):
        #    tBox.readExcel(tbox_file_path)
        #    tBox.parseInputData()
        #else:
        #    print(f"Error! Input TBox file does not exist: '{tbox_file_path}'.")
    #print(tBox.triples)
    #print(tBox.classRel)

    try:
        _read_as_encoding(input_path, encode="utf-8")
        #print("Finished UTF-8 encoding")
    except UnicodeDecodeError:
        _read_as_encoding(input_path)
        #print("Finished DEFAULT encoding")
    except Exception as e:
        print("Failed to read abox csv file. Exception ", e)

    g.serialize(destination=output_file_path, format="application/rdf+xml")
    print(f"Conversion complete. Abox created at '{output_file_path}'.")
    print(f"Total number of warnings in entityrdfizer = {WARNING_COUNT}.")
    #save_instances( "list_of_instances.csv" )

def _read_as_encoding(input_path, encode=None):
    input_name = os.path.basename(input_path)
    with open(input_path, 'rt', encoding=encode) as csvfile:
        rows = csv.reader(csvfile, skipinitialspace=True)
        for line_count, csv_row in enumerate(rows):
            _serialize_csv_row(csv_row, input_name, line_count)
            if (line_count >= 200000) and (line_count % 100000 == 0):
                # Long execution may look like hanging.
                # This is a user message to avoid confusion:
                print("  ..completed line", line_count)
        else:
            if line_count > 200000:
                print("Finished reading .csv file, start generating .owl")
    return

def convert_csv_string_into_rdf(csv_string):
    # This is a trick so that I can use csv.reader
    # to split string by ',' delimiter even if it is in quotes
    csvfile = io.StringIO(csv_string)
    rows = csv.reader(csvfile, skipinitialspace=True)
    for line_count, csv_row in enumerate(rows):
        _serialize_csv_row(csv_row, csv_string, line_count)
    csvfile.close()
    return str(g.serialize(format="application/rdf+xml"), 'utf-8')


def _serialize_csv_row(csv_row, filename, line_count):
    # (line_count+1) makes the 1-st line equal to 1 (for warnings):
    file_line = "in " + filename + " line " + str(line_count + 1)
    if line_count == 0:
        if not is_header_valid(csv_row, file_line):
            raise ValueError(textwrap.dedent(f"""
                    Error: Found invalid csv header: {csv_row},
                    so it will terminate now."""))
        global g
        g = Graph()

    if line_count > 0:
        process_data(csv_row, file_line)

if __name__ == "__main__":
    # Testing:
    print("==================== convert rdfs:label :")
    print(get_data_type("rdfs:label", " test line 459"))
    print("Test is_empty():")
    is_empty(111)

    print("Test instance_to_http():")
    instance_to_http(112, "in file on line")

    print("Test class_to_http():")
    class_to_http(113, "in file on line")

    print("Test set_tbox_iri():")
    set_tbox_iri("", "in file on line")
    set_tbox_iri("undefined", "in file on line")

    print("Test set_abox_iri():")
    set_abox_iri("", "in file on line")
    set_abox_iri("undefined", "in file on line")

    print("Test is_header_valid():")
    is_header_valid(["first", "second", "third"], "in unit testing")
    is_header_valid(["first", "second", "third", "forth", "fifth", "sixth"], "in unit testing")

    print("Test save_instances():")
    save_instances("tosave")
    save_instances("tosave.txt")

    print("Test convert_into_rdf():")
    convert_into_rdf("invalid/path")

    print("Test split_name_uuid():")
    n, u = split_name_uuid("UnitCell_PTY_628b0598-8429-3748-8570-af693be90476", "in file_line")
    n, u = split_name_uuid("UnitCell_PTY_628b0598-8429-4748-7570-af693be90476", "in file_line")
    n, u = split_name_uuid("UnitCell_PTY-628b0598-8429-4748-8570-af693be90476", "in file_line")
    n, u = split_name_uuid("UnitCell_PTY_628b0598-8429-4748-8570-af693be90476", "in file_line")
    # print(f"name = '{n}', uuid = '{u}'" )

    print("=======================================")
    print("Test convert_into_rdf():")
    # filename = "ontospecies_abox.csv"
    csv_file = "ontowarnings/ontowarnings_abox.csv"
    convert_into_rdf(csv_file)
