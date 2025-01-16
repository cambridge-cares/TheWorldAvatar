"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/01
"""

import csv
import os
import uuid
import logging

logging.basicConfig(level=logging.WARNING)
# logging.basicConfig(level=logging.INFO)

# TODO change uuid from dict to a separate class. For example UUID.
# To avoid error while using it.


def new_uuid():
    return str(uuid.uuid4())
    # === end of UuidDB.new_uuid()

class UuidDB:
    """ This class makes a reproducible UUID values for instances.
    For given class_name and item_name the UUID is taken from the
    list of previously saved such instances.
    The database is stored in a .csv format and can be reloaded.

    UuidDB (UUID Data Base) has 5 main functions:
    - loadUUID(): from a file (before the first use).
                  Extension .csv.
    - saveUUID(): to a file (must be used at the end,
                  otherwise corrections will be lost).
    - getUUID():  find out whether instance exists in database,
                  return the name and uuid.
    - addUUID():  do same as getUUID + add new element to database
                  return the new name and also new UUID.
 ###add element to database and
    - getUUID_Random() - what is this??

    """
    __slots__ = ["dbFilename", "uuidDB"]

    def __init__(self, filename = None):
        """
        The input parameter may be:
        - string (i.e. the filename of the existing database):
          Load the file and use the data as the database.
        - an existing database of type UuidDB
          (copy it to the current database?)
        - None or not specified.
          Warning and open a default data base 'uuid-default.csv'
          or create a new file if file does not exist.
          Later save to the same file (create new, if necessary).
        """

        default = os.path.join("uuid" + "-default.csv")
        # default = os.path.join("ontozeolite", "uuid", "default.csv")

        if filename is None or "" == filename:
            self.dbFilename = default

        elif isinstance(filename, str):
            if os.path.isfile(filename):
                self.dbFilename = filename
            elif os.path.isdir(filename):
                logging.error(" Uuid database path: '%s' is a directory."
                              " Using default.", filename)
                self.dbFilename = default
            else:
                logging.error(" Uuid database file does not exist: '%s'." +
                              " Create a new one.", filename)
                self.dbFilename = filename

        else:
            logging.error(" Uuid path must be a string, but got '%s'.",
                          str(type(filename)))
            self.dbFilename = filename

        self.loadDB(self.dbFilename)

        # === end of UuidDB.__init__()

    def valueIsValid(self, value):
        """
        Checking for illegal characters (' ' ',' '.' ';')
        TODO: ('{' '}").

        """
        if not isinstance(value, str):
            logging.error("Value must be string, but got '%s'.", str(value))
            return False

        short = value.strip()
        if len(short) == 0:
            logging.info(" Value '%s' is empty.", value)
            return False

        if short.find(" ") >= 0:
            logging.info(" Value '%s' contains space.", value)
            return False

        if short.find(",") >= 0:
            logging.info(" Value '%s' contains comma.", value)
            return False

        if short.find(";") >= 0:
            logging.info(" Value '%s' contains semicolon.", value)
            return False

        return True
        # === end of UuidDB.valueIsValid()

    def loadDB(self, file_path):
        """
        Load the database.
        """
        if not os.path.isfile(file_path):
            logging.info("UUID DB does not exist, initialize a new DB")
            self.uuidDB = {}
            return self.uuidDB

        array = readCsv(file_path)

        # Convert the list from csv into a dictionary:
        self.uuidDB = {}
        if not isinstance(array, list):
            return self.uuidDB
        if len(array) == 0:
            return self.uuidDB

        # Checking for the title line in the csv file:
        line = array[0]
        if line[0].strip().lower() ==    "Class name".strip().lower() and \
           line[1].strip().lower() == "Instance name".strip().lower() and \
           line[2].strip().lower() ==          "UUID".strip().lower():
            start_line = 1
        else:
            start_line = 0

        for line in array[start_line:]:
            if len(line) == 3:
                c_name = line[0]
                i_name = line[1]
                uuid_str  = line[2]
                if len(line[0]) > 0 and len(line[1]) > 0 and len(line[2]) > 0:
                    if c_name not in list(self.uuidDB.keys()):
                        self.uuidDB[c_name] = {}
                    if i_name not in list(self.uuidDB[c_name].keys()):
                        self.uuidDB[c_name][i_name] = uuid_str
                    else:
                        if uuid_str != self.uuidDB[c_name][i_name]:
                            logging.error("Mismatch uuid in database")
            else:
                logging.error(" Wrong entry in uuidDB: '%s'.", str(line))

        return self.uuidDB
        # === end of UuidDB.loadDB()

    def saveDB(self, filename=""):
        """
        Save the database to file.
        """
        if "" == filename:
            # file_path = os.path.join("ontozeolite", "uuid", "default.csv")
            file_path = self.dbFilename
        else:
            file_path = filename

        folder = os.path.dirname(file_path)
        if "" != folder:
            if not os.path.isdir(folder):
                os.makedirs(folder, exist_ok=True)

        array = []
        array.append(["Class name", "Instance name", "UUID"])
        for c_name in list(self.uuidDB.keys()):
            for i_name in list(self.uuidDB[c_name]):
                array.append([c_name, i_name, self.uuidDB[c_name][i_name]])

        nLinesWarn = 20 * 1000
        if len(array) > nLinesWarn:
            logging.warning("The size of the uuid database is over %s" +
                            " entries: %s.", str(nLinesWarn), str(len(array)))

        writeCsv(file_path, array)

        # === end of UuidDB.saveDB()

    def getUUID(self, class_name, instance_name):
        """
        class_name    - Is saved in the database for easier error detection,
                        but it will not be added to the instance_name.
                        It is your responsibility to write a correct instance_name.
        instance_name - Instance, exactly how it should look in the database
                       and later in ontology.
        return None if the instance does not exist
        """

        # Display the database (for debugging):
        if False:
            print(">>>>>>>>> Database status: >>>>>> ")
            for key1, db_class in self.uuidDB.items():
                for key2, db_name in db_class.items():
                    print("   ", key1, key2, ">>>", db_name)

        if not isinstance(self.uuidDB, dict):
            logging.error(" uuid database must be a dictionary, but got %s." +
                          " Failed to get UUID.", str(type(self.uuidDB)))
            # self.uuidDB = {}
            return None

        # Check the input names:
        if not self.valueIsValid(class_name):
            logging.error(" Class '%s' is not valid. Failed to get UUID. a1",
                          class_name)
            return None

        if not self.valueIsValid(instance_name):
            # logging.warning(" Instance '" + instance_name +
            #                 "' is not valid. Failed to get UUID. a2")
            logging.info(" Instance '%s' is not valid." +
                         " Removing wrong characters. a2", instance_name)
            instance_name = valueToValid(instance_name)
            # return None

        # Check whether the instance_name exists in other classes:
        # Warning! For big database this verification is slow.
        #for k1 in list(self.uuidDB.keys()):
        #    for k2 in list(self.uuidDB[k1].keys()):
        #        if k2 == instance_name and k1 != class_name:
        #            logging.warning(" Instance '" + k2 + "' in '" + class_name + \
        #                             "' may be in conflict with existing '" + \
        #                             k1 + "." + k2 + "'.")

        if False:
            for k1, db_class in self.uuidDB.items():
                for k2 in db_class:
                    if k2 == instance_name and k1 != class_name:
                        logging.warning(" Instance '%s' in '%s' may be in" +
                                        " conflict with existing '%s '%s'.",
                                        k2, class_name, k1, k2)
                        # Report only the first case.
                        break

        uuid_str = ""  # Default value of uuid_str

        # Find and return the new UUID for the class_name.instance_name
        if class_name in self.uuidDB:
            if instance_name in self.uuidDB[class_name]:
                uuid_str = self.uuidDB[class_name][instance_name]

        return str(uuid_str)
        # === end of UuidDB.getUUID()

    def addUUID(self, class_name, instance_name, new_uuid=""):
        """
            new_uuid - Recommended uuid for this instance.
            If uuidDB has an element with this class+instance+uuid -> use it.
            If uuidDB has class+instance different uuid -> warning.
            If uuidDB does not have class+instance -> create with given new_uuid.
            Return: a tuple of two names:
              - the new full name of instance: instance_uuid
              - the same uuid as a second entity of the tuple
        """

        if not isinstance(self.uuidDB, dict):
            logging.error(" uuid database must be a dictionary, but got %s" +
                          " (line A2).", str(type(self.uuidDB)))
            self.uuidDB = {}
            pass

        # This will be checked inside getUUID()
        # if not self.valueIsValid(class_name):
        #    logging.error(" Class '" + class_name + "' is not valid. a3")

        # This will be checked inside getUUID()
        # if not self.valueIsValid(instance_name):
        #    logging.error(" Instance '" + instance_name + "' is not valid. a4")

        if not isinstance(new_uuid, str):
            logging.error(" new_uuid must be a string, but got '%s'.",
                          str(type(new_uuid)))
            return "",""

        # Check whether the instance_name exists in other classes:
        uuid_str = self.getUUID(class_name, instance_name)

        # Check for duplicates in existing database:
        #for k1 in list(self.uuidDB.keys()):
        #    for k2 in list(self.uuidDB[k1].keys()):
        #        if k1 != class_name and k2 == instance_name:
        #            logging.warning(" Instance '" + k2 + "' in '" + \
        #                             class_name + "' is in " + \
        #                             "conflict with existing '" + k1 + "'.")

        # Find are return or assign new UUID for the class_name.instance_name

        #if "" != new_uuid and uuid_str != new_uuid:
        #    logging.error(" Existing element in database has different uuid:" +
        #                   " class '" + class_name + "'," +
        #                   " instance '" + instance_name + "'," +
        #                   " in DB '" + uuid_str + "', requested '" + new_uuid + "'.")

        # Assign the value to the database:
        if "" == new_uuid:
            if "" == uuid_str:
                uuid_str = str(uuid.uuid4())
        else:
            if None == uuid_str or "" == uuid_str:
                # Do nothing, the id does not exist or has errors
                pass
            elif uuid_str != new_uuid:
                logging.error(" Existing element in DB has different uuid:" +
                              " class '%s', instance '%s', has UUID in DB" +
                              " '%s', while the new is '%s'. The new UUID" +
                              " has priority and is overwritten in database.",
                              class_name, instance_name, uuid_str, new_uuid)

            uuid_str = new_uuid

        if class_name not in self.uuidDB:
            self.uuidDB[class_name] = {}

        self.uuidDB[class_name][instance_name] = uuid_str

        output = instance_name + "_" + str(uuid_str)

        return output, uuid_str

        #if class_name in list(self.uuidDB.keys()): <= must be true
        if True:
            if instance_name in list(self.uuidDB[class_name]):

                if "" != new_uuid and uuid_str != new_uuid:
                    logging.error(" Existing element in database has" +
                                  " different uuid: class '%s', instance" +
                                  " '%s', in DB '%s', requested '%s'.",
                                  class_name, instance_name, uuid_str, new_uuid)
            else:
                if "" == new_uuid:
                    uuid_str = uuid.uuid4()
                else:
                    uuid_str = new_uuid
                self.uuidDB[class_name][instance_name] = uuid_str

        else:
            if "" == new_uuid:
                uuid_str = uuid.uuid4()
            else:
                uuid_str = new_uuid

            self.uuidDB[class_name] = {}
            self.uuidDB[class_name][instance_name] = uuid_str

        output = instance_name + "_" + str(uuid_str)

        return output #, uuid_str
        # === end of UuidDB.addUUID()

    def tmpUUID(self, name):
        """
        Create an instance and uuid without adding them to the database.
        Why is this necessary?
        """

        uuid_str = str(uuid.uuid4())

        output = name + "_" + uuid_str

        #logging.warning("Not implemented getUUID(), using '" + str(uuid_str) + "'.")

        return output, uuid_str
        # === end of UuidDB.tmpUUID()

    # === end of class UuidDB


def valueToValid(value):
    output = value
    output = output.replace(" ", "")
    output = output.replace(",", "")
    output = output.replace(";", "")

    return output.strip()


def getCifLineRanges(fileIn):
    ranges = []

    if not os.path.isfile(fileIn):
        logging.error("File '%s' does not exist " +
                      "in tools.getCifLineRanges()", fileIn)
        return ranges

    f = open(fileIn)
    ranges.append(0)
    lineCount = 0
    for line in f:
        # TODO detect beginning of the new CIF file

        lineCount += 1
        pass

    ranges.append(lineCount)

    f.close()

    return ranges
    # === end of getCifLineRanges()


'''
'''
def getUUID(uuidDB, class_name, instanceName, new_uuid = ""):
    """
  new_uuid - Recommended uuid for this instance.
            If uuidDB has an element with this class+instance+uuid -> use it.
            If uuidDB has class+instance different uuid -> warning.
            If uuidDB does not have class+instance -> create with given new_uuid.
  class_name - Is saved in the database for easier error detection,
              but it will not be added to the instanceName.
              It is your responsibility to write a correct instanceName.
  instanceName - Instance, exactly how it should look in the database and later in ontology.
    """
    # Check the input names:
    if not valueIsValid(class_name):
        logging.error(" Class '%s' is not valid. a5", class_name)

    if not valueIsValid(instanceName):
        logging.error(" Instance '%s' is not valid. a6", instanceName)

    if not isinstance(uuidDB, dict):
        logging.error(" uuid database must be a dictionary, but got %s" +
                      " (line A1).", str(type(uuidDB)))
        return ""

    if not isinstance(new_uuid, str):
        logging.error(" new_uuid must be a string, but got %s.",
                      str(type(new_uuid)))

    # Check whether the instanceName exists in other classes:
    tmp = []
    #for k1 in list(uuidDB.keys()):
    #  for k2 in list(uuidDB[k1].keys()):
    #    if k1 != class_name and k2 == instanceName:
    #      logging.warning(" Instance '" + k2 + "' in '" + class_name + "' is in " +
    #                       "conflict with existing '" + k1 + "." + k2 + "'.")

    for k1, db_class in uuidDB.items():
        if k1 != class_name:
            #for k2, ins_name in cls_name.items():
            for k2 in db_class.keys():
                if k2 == instanceName:
                    logging.warning(" Instance '%s' in '%s' is in" +
                                    " conflict with existing '%s.%s'.",
                                    k2, class_name, k1, k2 )
                    break  # <= Find only the first error (to speed up)


    # Find and return or assign new UUID for the class_name.instanceName
    #if class_name in list(uuidDB.keys()):
    #  if instanceName in list(uuidDB[class_name]):
    #    uuid_str = uuidDB[class_name][instanceName]
    #    if "" != new_uuid and uuid_str != new_uuid:
    #      logging.error(" Existing element in database has different uuid:" +
    #                     " class '" + class_name + "'," +
    #                     " instance '" + instanceName + "'," +
    #                     " in DB '" + uuid_str + "', requested '" + new_uuid + "'.")

    if class_name in uuidDB:
        if instanceName in list(uuidDB[class_name]):
            uuid_str = uuidDB[class_name][instanceName]
            if "" != new_uuid and uuid_str != new_uuid:
                logging.error(" Existing element in DB has different uuid:" +
                              " class '%s', instance '%s', in DB '%s'," +
                              " requested '%s'.",
                              class_name, instanceName, uuid_str, new_uuid)
        else:
            if "" == new_uuid:
                uuid_str = uuid.uuid4()
            else:
                uuid_str = new_uuid
            uuidDB[class_name][instanceName] = uuid_str

    else:
        if "" == new_uuid:
            uuid_str = uuid.uuid4()
        else:
            uuid_str = new_uuid

        uuidDB[class_name] = {}
        uuidDB[class_name][instanceName] = uuid_str

    output = instanceName + "_" + str(uuid_str)

    return output #, uuid_str
    pass # getUUID()


def getUUID_random(code):
    """
    Create an instance and uuid without adding them to the database.
    Why is this necessary?
    """

    # output = ""
    # output = "uuid-" + code

    uuid_str = str(uuid.uuid4())

    output = code + "_" + str(uuid_str)

    logging.warning("Not implemented getUUID(), using '%s'.", str(uuid_str))

    return output
    # === end of getUUID_random()


def strSplit(inline, sep = [" ", "\t"], quotes = ["'", '"']):
    """

    """
    words = []
    w = []
    #inQuote1 = False
    #inQuote2 = False
    inQuote = ""
    if isinstance(sep, list):
        sepArr = sep
    elif isinstance(sep, str):
        sepArr = [sep]
    else:
        logging.error(" Unknown type '%s' of sep '%s'." +
                      " Expected str or list.", str(type(sep)), str(sep))

    for ic, c in enumerate(inline.strip()):
        if inQuote == "":
            #if "'" == c or '"' == c:
            if c in quotes:
                inQuote = c
            elif c in sepArr:
                if len(w) > 0:
                    words.append("".join(w))
                    w = []
            else:
                w.append(c)

        else:
            #if "'" == c or '"' == c:
            if c in quotes:
                inQuote = ""
                words.append("".join(w))
                w = []

            #elif " " == c:
            #  w.append(c)

            else:
                w.append(c)
    else:
        if len(w) > 0:
            words.append("".join(w))

    return words
    # === end of strSplit()


# A global variable to track number of errors while saving files:
write_csv_err_count = 0


def writeCsv(filename, array):
    global write_csv_err_count
    logging.info(" writeCSV() to '%s'", filename)

    try:
        with open(filename, "w", newline="", encoding="utf-8") as f:

            csvw = csv.writer(f, delimiter=",", quotechar='"',
                              quoting=csv.QUOTE_MINIMAL)

            for a in array:
                csvw.writerow(a)

    except IOError:
        tmp_file = "test-tmp.csv"
        logging.error(" File '%s' is protected. " +
                      "Using temporary instead: '%s'.", filename, tmp_file)
        write_csv_err_count += 1
        if 1 == write_csv_err_count:
            writeCsv(tmp_file, array)
        else:
            logging.error(" I give up. " + "You need to close the files.")

    # === end of writeCsv()


def readCsv(filename, encoding="utf-8"):
    output = []
    if os.path.exists(filename):
        try:
            # print(">>> In tools.py going to open file", filename)
            # with open(filename, "r", encoding="utf-8") as f:
            if encoding == "utf-8":
                f = open(filename, "r", encoding="utf-8")
                # print("Opening as utf")
            else:
                f = open(filename, "r")
                # print("Opening as default")
            csvr = csv.reader(f)

            for ir, row in enumerate(csvr):
                # print(filename, ir, row)
                output.append(row)
            f.close()

        except Exception as e:
            print("Failed to open file '", filename, "' as utf-8", sep="")
            print("An error:", e)
            raise e
    else:
        print("readCSV: file does not exist: '" + filename + "'.")

    return output
    # === end of loadCSV()


def get_csv_init(base_name, t_prefix, a_prefix):
    """ Create the header of the csv for abox writer.
        It is used for creation of any new abox.

    """

    # Remove slash at the end of the abox prefix to avoid double slash "//"
    if a_prefix.endswith("/"):
        a_prefix = a_prefix[:-1]

    output = []
    output.append(["Source", "Type", "Target", "Relation",
                   "Value", "Data Type"])

    output.append([base_name, "Ontology", t_prefix,
                   "http://www.w3.org/2002/07/owl#imports", "", ""])

    output.append([base_name, "Ontology", a_prefix, "base", "", ""])

    return output
    # === end of getCsvInit()

if __name__ == "__main__":

    # Test and example of use:
    db = loadUUID()

    print(getUUID(db, "ClassA", "a1"))
    print(getUUID(db, "ClassA", "a2"))
    print(getUUID(db, "ClassA", "a3"))
    print(getUUID(db, "ClassB", "b1"))
    print(getUUID(db, "ClassB", "a1"))
    # print(getUUID(db, "ClassC", "c1"))
    # print(getUUID(db, "ClassC", "c2"))
    # print(getUUID(db, "ClassC", "c3"))
    print(getUUID("ss", "ClassC", "c3"))

    for k1 in db.keys():
        # print("Class '" + k1 +"': ")
        for k2 in db[k1].keys():
            # print(" ", k2, ":", db[k1][k2])
            pass

    saveUUID(db)

    input_strings = ["1 2 3 4 5", "1   2  3   4  5\n",
                     "1 '2 3' 4 5", "1 2 3 '4   5'"]
    for input_string in input_strings:
        out = strSplit(input_string)
        print(f"Input: {input_string}, Out: {str(out)}")
        # print(out)
