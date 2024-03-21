""" Module implements the datatypes defined in OntoCrystal ontology.
Includes: OntoMeasureWithUncertainty, OntoVector, OntoMatrix, OntoPlot.
See also test_ontocrystal_datatypes.py for the tests and usage examples.
"""


import logging
import tools

# logging.basicConfig(level=logging.DEBUG)
# logging.basicConfig(level=logging.INFO)
# logging.basicConfig(level=logging.WARNING)
logging.basicConfig(level=logging.ERROR)

# Ontology for Crystal,
crystOntoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"

omOntoPrefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
OM2_KEYWORDS = ["angstrom", "cubicAngstrom", "reciprocalAngstrom",
                "degree", "dimensionOne", "percent", "reciprocalCubicAngstrom",
                "reciprocalCubicNanometre",
                "squareAngstrom", "squareMetrePerGram",
                "squareMetrePerCubicCentimetre", "gramPerCubicCentimetre",
                "hasUnit",
                "Measure", "Unit"
               ]


def is_http(value):
    """ Checking whether the input string is a web-address, 
    i.e. begins with "http://"
    """
    if value.strip().lower().startswith("http://") or \
       value.strip().lower().startswith("https://"):
        return True
    return False


def omInitUnits():
    output = []
    #uuid_om_angstrom = tools.getUUID(self.uuidDB, self.omOntoPrefix + "Unit", "angstrom")
    # This must be initialized in om-2, not here.
    output += omInitUnit("om:angstrom")
    output += omInitUnit("om:reciprocalAngstrom")
    output += omInitUnit("om:cubicAngstrom")
    output += omInitUnit("om:degree")
    output += omInitUnit("om:dimensionOne")
    output += omInitUnit("om:percent")
    output += omInitUnit("om:reciprocalCubicNanometre")
    output += omInitUnit("om:squareAngstrom")
    output += omInitUnit("om:squareMetrePerGram")
    output += omInitUnit("om:squareMetrePerCubicCentimetre")
    output += omInitUnit("om:gramPerCubicCentimetre")

    ''' Old version:
        output.append([omOntoPrefix + "angstrom", "Instance",
                         omOntoPrefix + "Unit", "", "", ""])
        output.append([omOntoPrefix + "reciprocalAngstrom", "Instance",
                         omOntoPrefix + "Unit", "", "", ""])
        output.append([omOntoPrefix + "cubicAngstrom", "Instance",
                         omOntoPrefix + "Unit", "", "", ""])
        output.append([omOntoPrefix + "degree", "Instance",
                         omOntoPrefix + "Unit", "", "", ""])
        output.append([omOntoPrefix + "dimensionOne", "Instance",
                         omOntoPrefix + "Unit", "", "", ""])
    '''

    return output

def omInitUnit(unit):
    """
    A function specially for OM-2 ontology. Define the unit in abox.

    This initialization is redundant. Originally units are loaded from OM-2.
    Ideally this function is not necessary, but for debugging/testing it is
    easier to define it here and don't load OM-2 .owl every time.
    """
    logging.warning(" Initializing unit '%s', it should be" +
                    " removed in the production run.", unit)

    if unit.strip().startswith("om:"):
        new_unit = unit.strip()[3:]
    else:
        logging.error(" omInitUnit() expects unit with prefix 'om:'," +
                       " but got '%s'.", unit)
        new_unit = unit

    output = []

    output.append([omOntoPrefix +  new_unit, "Instance",
                  omOntoPrefix + "Unit", "", "", ""])
    output.append(["rdfs:label", "Data Property",
                  omOntoPrefix + new_unit, "", new_unit, "xsd:string"])

    return output
    # === end of omInitUnit()


def omSetUnit(subject, unit):
    """
    A function specially for OM-2 ontology. Assign the unit to given subject.

    Function expects an instance of class OM-2:Unit from units-of-measurement.
    It is user's responsibility to confirm that this unit exists in OM-2.
    Input must be with short prefix, i.e. for unit 'xxx' input is 'om:xxx'.
    This is to remind user that the unit is from the OM-2 ontology.
    Return:
    A row to be saved in the abox .csv file.
    """
    # print(">>>>>>>>>> start omSetUnit ", subject, unit)
    if unit.strip().startswith("om:"):
        new_unit = unit.strip()[3:]
    else:
        logging.error(" omSetUnit() expects unit with prefix 'om:'," +
                      " but got '%s'.", unit)
        new_unit = unit

    if new_unit not in OM2_KEYWORDS:
        logging.error(" Unknown unit '%s'. You may update the" +
                      " OM2_KEYWORDS array if unit is missing" +
                      " in ontocrystal_datatypes.py and csv_maker.py.", unit)

    output = []

    output.append([subject, "Instance", omOntoPrefix + new_unit,
                                        omOntoPrefix + "hasUnit", "", ""])
    return output
    # === end of omSetUnit()

def omNameConvert(unit_in):
    """
    A function specially for OM-2 ontology. Adds the full path in front.
    Input unit must start wtih 'om:'.
    """
    short = unit_in.strip()
    if short.startswith("om:"):
        if short[3:] in OM2_KEYWORDS:
            unit_out = "http://www.ontology-of-units-of-measure.org/resource/om-2/" + \
                      short[3:]
        else:
            logging.error(" Unknown unit '%s', require a full IRI.", unit_in)
            if short in OM2_KEYWORDS:
                logging.error("      Did you forget the 'om:' prefix?")
    else:
        unit_out = unit_in

    return unit_out
    # === end of omNameConvert()

class OntoMeasureWithUncertainty:
    """
    self.uuid - is the unique name of the entity of this class.
    """

    __slots__ = ["uuidDB", "uuid", "class_name", "item_name",
                 "abox_prefix", "tbox_prefix",
                 "value", "error", "unit"
                ]

    def __init__(self, item_name, class_name="",
                 tbox_prefix=None, abox_prefix=None,
                 uuidDB=None):
        """
        itemName  - (required) name of the new variable.

        class_name - (optional) the class name. If not specified,
                    the basic OntoCrystal/MeasureWithUncertainty is used.

        tbox_prefix   - It is possible to assign prefix for TBox for the class and
                    the properties.
                    If not specified, the default OntoCrystal prefix is used.
                    Otherwise whateven specified will be prefixed.
                    In particular, if empty string is given then none will be
                    prefixed and the prefix will be assigned later
                    by the abox writer (see 'entityrdfizer').
                    User is responsible to provide the correct entry,
                    the correctness may also be checked by the tboxtools module.

                    #(optional) the prefix for TBox, where a class derived
                    from this class is defined.
                    ###### If not specified OntoCrystal is used.
                    #If not specified, the entity will not have any prefix
                    # (i.e. will use the global tbox prefix from abox csv file)

        abox_prefix   - (optional) the prefix for ABox, where this entiry should
                    be stored.
                    If not specified, the entity will not have any prefix
                        (i.e. will use the global abox prefix from csv file)

        uuidDB    - The database of uuid(s) where this Measure is saved.
                    If not specified, a default filename will be used (not recommended).

        """

        if "" == item_name:
            self.item_name = "Unknown"
            logging.error(" Empty id in '%s', in class" +
                          " OntoMeasureWithUncertainty.", self.item_name)
        else:
            self.item_name = omNameConvert(item_name)
            self.item_name = item_name

        if "" == class_name:
            self.class_name = "MeasureWithUncertainty"
            logging.warning(" Empty class in '%s', using class" +
                            " MeasureWithUncertainty.", self.item_name)
        else:
            self.class_name = omNameConvert(class_name)
            self.class_name = class_name

        if tbox_prefix is None:
            self.tbox_prefix = crystOntoPrefix
        elif "" == tbox_prefix:
            # TODO This check is redundant, can usee the else option:
            self.tbox_prefix = ""
             #logging.warning(" Empty tbox_prefix in '" + self.item_name + "'," + \
            #                 " using global TBox from csv file.")
        else:
            self.tbox_prefix = tbox_prefix

        if abox_prefix is None:
            self.abox_prefix = ""
            #logging.warning(" Empty abox_prefix in '" + self.item_name + "'," + \
            #                 " using global ABox from csv file.")
        else:
            self.abox_prefix = abox_prefix

        if uuidDB is None:
            logging.error(" Empty uuidDB in '%s' in class" +
                          " OntoMeasureWithUncertainty. Using default.",
                          self.item_name)
        elif isinstance(uuidDB, tools.UuidDB):
            self.uuidDB = uuidDB
        else:
            logging.error("Wrong type of uuidDB, expected 'UuidDB', got '%s'.",
                          str(type(uuidDB)))

        self.value = None
        self.error = None
        self.unit  = None

        # === end of OntoMeasureWithUncertainty.__init__()

    def setValue(self, value, unit="", error=""):
        if "" == value:
            logging.info(" Empty value in '%s'.", self.item_name)
        else:
            self.value = value

        if "" == error:
            logging.info(" Empty error in '%s'.", self.item_name)
        else:
            self.error = error

        if "" == unit:
            logging.info(" Empty unit in '%s'.", self.item_name)
        else:
            self.unit = unit

        # === end of OntoMeasureWithUncertainty.setValue()

    #def getValue(self):
    #  return self.value

    def get_csv_arr(self, subject, predicate):
        """
        subject   - Is the full hame of instance of class,
                    which contains this entity of MeasureWithUncertainty class.
        predicate - Is the Object Property linking the Subject and
                    the current entity of MeasureWithUncertainty."

        # These property values are not guaranteed:
        if subject.find("CrystalInformation") < 0:
          logging.warning(" Subject in arrUnitCell() is '" + subject + "'," +
                           " expecting the name to contain '" + "CrystalInfromation" + "'.")

        """
        if is_http(predicate):
            short = predicate.split("/")[-1]
        else:
            short = predicate

        if not short.startswith("has"):
            logging.warning(" Predicate in OntoMeasure.get_csv_arr() is '%s'," +
                            " but expecting it to have 'has'.", predicate)

        output = []

      #output += omInitUnit("om:cubicAngstrom")

      # FIXME ??? The tbox prefix should be OntoCrystal if the item is defined
      #           in OntoCrystal,
      #           And can be overwritten for types defined in other ontologies.
      #           But this means I have to save somewhere a full list of
      #           classes defined in OntoCrystal.
      #
      # myClass = "MeasureWithUncertainty"
      # self.uuid = tools.getUUID(self.uuidDB, self.class_name, self.item_name) #self.value["name"])
        self.uuid,_ = self.uuidDB.addUUID(self.tbox_prefix + self.class_name,
                                          self.abox_prefix + self.item_name)
        output.append([self.uuid, "Instance", self.tbox_prefix + self.class_name,
                       "", "", ""])
        output.append([subject, "Instance", self.uuid, predicate, "", ""])

        if self.value is not None:
            output.append([omOntoPrefix + "hasNumericalValue", "Data Property",
                           self.uuid, "", self.value, "xsd:decimal"])

        if self.error is not None:
            output.append([crystOntoPrefix + "hasUncertaintyValue", "Data Property",
                           self.uuid, "", self.error, "xsd:decimal"])

        if self.unit is not None:
            output += omSetUnit(self.uuid, self.unit)

        #logging.error(" arrValue() is not implemented yet")

        return output
        # === end of OntoMeasureWithUncertainty.get_csv_arr()

    # === end of class OntoMeasureWithUncertainty

class OntoVector:
    """
    item_name  - (required) name of the new variable.

    class_name - (optional) the class name. If not specified, Vector is used.

    tbox_prefix   - TODO Same as OntoMeasureWithUncertainty.

                (optional) the prefix for TBox, where a class derived from this class is defined.
                ###### If not specified OntoCrystal is used.
                If not specified, the entity will not have any prefix
                      (i.e. will use the global tbox prefix from csv file)

    abox_prefix   - TODO Same as OntoMeasureWithUncertainty.
                (optional) the prefix for ABox, where this entiry should be stored.
                If not specified, the entity will not have any prefix
                      (i.e. will use the global abox prefix from csv file)

    uuidDB    - The database of uuids where all entities are saved.

    uuid4     - the unique UUID4 code of the entity of entity of this class.

    uuid      - the unique name of the entity of this class.
                Consists of the name and the uuid4: itemName_UUID4.

    comp_list  - Components as a list (for components generated form array)

    comp_dict  - Components as a dictionary (for components with label)


    """
    __slots__ = ["uuidDB", "uuid", "uuid4", "item_name", "class_name", #"value",
                "tbox_prefix", "abox_prefix",
                "ontoPrefix", "uuid_error", # <- old options, need to remove
                "unit", "vectorLabel",
                "comp_list", "compErrList", "compUnitList",
                "comp_dict" #, "compErrDict"
              ]
    def __init__(self, class_name, item_name,
                 uuidDB=None,
                 tbox_prefix=None, abox_prefix=None,
                 unit=None, vectorLabel=None
                 #prefix = "",
                 #myUnit = "",
                 #myLabel = ""
                 ):
        """
        uuidDB    - Either a path to new/existing uuidDataBase (as string),
                    or an existing uuidDB (as dict or UUID class)
                    If not specified (or None), a default filename
                    'defailt-uuid.csv' will be used, or a new file will
                    be created with this name (not recommended).
        unit      - a unit with "om:" prefix.

        """

        if "" == item_name:
            logging.error(" In OntoVector the entity name is not specified." +
                          " Critical error!")
            #self.value["name"] = "Unknown"
            return
        else:
            #self.value = dict()
            #self.value["name"] = itemName

            self.item_name = item_name


        if "" == class_name:
            logging.warning(" In OntoVector the class name is not specified" +
                            " for '%s'. Using default 'Vector'.",
                            self.item_name)
            #self.value["class"] = "Vector"
            self.class_name = "Vector"
        else:
            #self.value["class"] = className
            self.class_name = class_name


        """
    if None == uuidDB:
      logging.warning(" In OntoVector uuidDB is not specified" + \
                      " for '" + self.itemName + "'.")
      1/0
      " to create a new uuid here"
    elif isinstance(uuidDB, str):
      logging.warning(" In OntoVector uuidDB is not specified" + \
                       " for '" + self.itemName + "'.")
      1/0
      " to create a new uuid here"
        """
        if isinstance(uuidDB, tools.UuidDB):
            self.uuidDB = uuidDB

        else:
            """
            logging.error(" Invalid value or type of uuidDB in OntoVector." + \
                           " Extected class UuidDB, but got '" + \
                           str(type(uuidDB)) + "'.")
            self.uuidDB = uuidDB
            """

            self.uuidDB = tools.UuidDB(uuidDB)
            logging.warning(" Creating a new UuidDB database, in file '%s'.",
                            self.uuidDB.dbFilename)

        if "" == unit or unit is None:
            logging.warning(" In OntoVector unit is not specified for '%s'.",
                            self.item_name)
            self.unit = None
          #self.value["unit"] = "Unknown"
        else:
            #self.value["unit"] = omNameConvert(unit)
            #self.value["unit"] = unit
            self.unit = unit

        if vectorLabel is None:
            #self.value["label"] = "Unknown"
            if "UnitCellLatticeVector" == self.class_name:
                logging.error(" In OntoVector label is not specified" +
                              " for '%s'.", self.item_name)
            self.vectorLabel = vectorLabel
            #self.vectorLabel = None
        else:
            #self.value["label"] = vectorLabel
            self.vectorLabel = vectorLabel
            if self.class_name in ["MeasureVector", "MillerIndices"]:
                logging.error(" In OntoVector class '%s'" +
                              " should not have vectorLabel, but given '%s'.",
                              self.class_name, self.vectorLabel)

        if tbox_prefix is None:
            self.tbox_prefix = crystOntoPrefix
            #logging.warning(" Empty tbox_prefix in '" + self.itemName + "'," + \
            #                 " using global TBox from csv file.")
        else:
            self.tbox_prefix = tbox_prefix

        if abox_prefix is None:
            self.abox_prefix = ""
            #logging.warning(" Empty abox_prefix in '" + self.itemName + "'," + \
            #                 " using global ABox from csv file.")
        else:
            self.abox_prefix = abox_prefix

        # The vector class is defined in the OntoCrystal ontology:
        self.ontoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"
        #if "" == prefix:
        #  logging.error(" In OntoVector ontology prefix is not specified" +
        #                 " for '" + self.itemName + "'.")
        #else:
        #  self.ontoPrefix = prefix

        #self.value["comp"] = list()


        #self.value["list"] = dict()
        self.comp_list      = []
        self.compErrList   = []
        self.compUnitList  = []

        self.comp_dict      = {}
        #self.compErrDict   = {}

        self.uuid       = None
        self.uuid4      = None
        # This is used to avoid double creation of the Uncertainty Vector:
        self.uuid_error = None

        #self.error = {}

        # === end of OntoVector.__init__()

    def addComponentList(self, valList, unit=None, errList=None):
        """
        The size of errList must be the same as valList,
        The errList may contain 'None' values for some entries.
        In this case the output will not have an uncertainty.
        """
        # logging.error(" in CsvMaker addComponentList() is not implemented")

        if [] != self.comp_list:
            logging.warning(" Over-writing existing values in vector '%s'.",
                            self.item_name)
            print("   >", self.comp_list)

        if [] != self.compErrList:
            logging.warning(" Over-writing existing errors in vector '%s'.",
                            self.item_name)

        if [] != self.compUnitList:
            logging.warning(" Over-writing existing units in vector '%s'.",
                            self.item_name)

        #-----------------------------------
        if isinstance(valList, list):
            self.comp_list = list(valList)
        else:
            logging.error(" In OntoVector.addComponentList() expect a list," +
                          " got '%s'.", str(type(valList)))
            self.comp_list = None

        #for il, el in enumerate(valList):
        #  self.value["list"] = None
        #  pass

        #-----------------------------------
        if errList is None:
            # Do nothing, no error list
            self.compErrList = None

        elif isinstance(errList, list):
            if len(valList) != len(errList):
                logging.error(" In OntoVector.addComponentList() the size" +
                              " of err %d is not equal to size of val %d," +
                              " skipping error list.",
                              len(errList), len(valList))
            else:
                self.compErrList = list(errList)

        else:
            logging.error(" In OntoVector.addComponentList() expect an" +
                          " error list, got '%s'.", str(type(errList)))
            self.compErrList = None

        #-----------------------------------
        if "" == unit or unit is None:
            logging.info(" No unit in vector component list")
            self.compUnitList = None
        elif isinstance(unit, list):
            # Individual unit for each component, as a list
            if len(valList) != len(unit):
                logging.error(" In OntoVector.addComponentList() the size" +
                              " of unit %d is not equal to size of val %d," +
                              " skipping unit list.", len(unit), len(valList))
            self.compUnitList = list(unit)
        elif isinstance(unit, str):
            if unit.startswith("om:"):
                #logging.error(" Not implemented unit in vector list")
                self.compUnitList = [unit] * len(valList)
            else:
                logging.error(" In OntoVector.addComponentList() unknown" +
                              " unit value '%s', expecting prefix om:", unit)
                self.compUnitList = None
        else:
            logging.error(" Unknown case for unit '%s'.", str(unit))
            # Do nothing. Need a warning?

        # === end of OntoVector.addComponentList()

    def addComponent(self, label, value="", unit="", error="", index=None):
        """
        Adding a component to a vector.
        For a vector made of a list see function addComponentList().

        TODO label and index. Only one of them is required..

        label - required argument. It defines the component subscript.
        index - optional argument.
        value - optional argument. The value of this component.
        error - Optional. The component's error-bar (uncertainty). It has the same unit.
        unit  - Optional. An instance defined in ontology OM-2.
                Accepted values either full path, or only the instance name.
                The OM-2 prefix will be appended in get_csv_arr().
        """

        err_count = 0

        if not isinstance(self.comp_dict, dict):
            self.comp_dict = {}

        if isinstance(label, str):
            if "" == label:
                logging.error(" Not specified label for vector component in" +
                              " '%s'. 22", self.item_name)
                err_count += 1

        if isinstance(value, str):
            if "" == value:
                logging.error(" Not specified value for vector component" +
                              " in '%s'.", self.item_name)
                err_count += 1
        else:
            logging.info("value = '%s' is not a string in '%s'.",
                         str(value) ,self.item_name)

        if label in self.comp_dict:
            logging.error("Over-writing an existing component with label" +
                          " '%s' for Vector '%s'.",label , self.item_name)

        self.comp_dict[label] = {}
        if "" != label:
            self.comp_dict[label]["label"] = str(label)
        if "" != value:
            self.comp_dict[label]["value"] = str(value)
        if "" != error:
            self.comp_dict[label]["error"] = str(error)
        if "" != unit:
            self.comp_dict[label]["unit"] = str(unit)
        if index is not None:
            self.comp_dict[label]["index"] = index

        return err_count
        # === end of OntoVector.addComponent()

    '''
    def addComponentOld(self, label, value="", unit="", error=""):
        logging.info("Starting addComponent '" + self.itemName + "' label: '" + label + "'")
        """
        Adding a component to a vector.
        TODO: how to create a vector made of multiple values as a list?
              probably it should be a separate function
              addComponentList(self, list, unit = "", error = "")

        label - required input argument. It defines the component subscript.
        value - required input argument. The value of this component.
        unit  - Optional. An instance defined in ontology OM-2.
                Accepted values either full path, or only the instance name.
                The OM-2 prefix will be appended in get_csv_arr().
        error - Optional. The component's error-bar (uncertainty). It has the same unit.
        """
        errCount = 0
        if isinstance(value, str):
            if "" == value:
                logging.error(" Not specified value for vector component" +
                              " in '%s'.", self.itemName)
                errCount += 1
        else:
            logging.error("value = '%s' is not a string in '%s'.",
                          str(value), self.itemName)

        if isinstance(label, str):
            if "" == label:
                logging.error(" Not specified label for vector component" +
                              " in '%s'. 33", self.itemName)
                errCount += 1

            keys = [x["label"] for x in self.value["comp"]]
            #keys = list(self.value["comp"].keys())
            if label in keys:
                logging.warning(" Repeatedly adding (overwriting) a component" +
                                " '%s' in vector '%s'.", label, self.itemName)
                logging.warning("Components-2 of vector =" + str(self.value["comp"]) + \
                                " new value = '" + value + "'.")
                #errCount += 1 # No need to count as error. It may be intensional.

        else:
            logging.error("label = '%s' is not a string in '%s'.",
                          str(label), self.itemName)

        if 0 == errCount:
            newComp = dict()
            newComp["label"] = label
            newComp["value"] = value
            if "" == unit:
                logging.info(" Not specified unit for vector component" +
                             " in '%s'. 44", self.itemName)
                pass
            else:
                newComp["unit"]  = unit

            if "" != error:
                newComp["error"] = error
                logging.info(" Components of vector = '%s' value = '%s'.",
                             str(self.value["comp"]), str(value))
                if "" != unit:
                    # Generally speaking the error bars may have different units,
                    # though such situation looks strange:
                    #print(" Assigned unit to", label)
                    newComp["errorunit"]  = unit
                    pass

            self.value["comp"].append(newComp)

            """
        self.value["comp"][label] = dict()
        self.value["comp"][label]["value"] = value
        if "" == unit:
          logging.warning(" Not specified label for vector component in '" + self.value["name"] + "'.")
        else:
          self.value["comp"][label]["unit"]  = unit

        if "" != error:
          self.value["comp"][label]["error"] = error
          logging.warning("Components of vector = ", self.value["comp"], " value = '" + value + "'.")
          if "" != unit:
            # Generally speaking the error bars may have different units,
            # though such situation looks strange:
            #print(" Assigned unit to", label)
            self.value["comp"][label]["errorunit"]  = unit
            pass
        """
        return errCount
        pass # OntoVector.addComponent()
    '''

    def get_csv_arr(self, subject, predicate):
        #print("================")
        """
        subject   - Is the full hame of instance of class,
                    which contains this Vector class.
        predicate - Is the Object Property linking the Subject and the current UnitCell.
                    Typically is should be contain "has".

        # Value is not guaranteed:
        if subject.find("CrystalInformation") < 0:
          logging.warning(" Subject in arrUnitCell() is '" + subject + "'," +
                           " expecting the name to contain '" + "CrystalInfromation" + "'.")
        """

    #if "has" != predicate:
        if is_http(predicate):
            short = predicate.split("/")[-1]
        else:
            short = predicate

        if not short.startswith("has"):
            logging.warning(" Predicate in OntoVector.get_csv_arr() is '%s'," +
                            " but expecting it to have 'has' prefix.",
                            predicate)

        """
        Create a vector with specified values.
        'subject' - is the full proper name of the parent class pointing to the new vector,
        'preidcate' - is the full object property to link the parent and the new vector,
        The input 'value' is a dictionary with values:
        ["class"] : The class of the newly created vector. Depending on the class name
                    some properties may not be available.
        ["name"] : The instance name (without UUID, UUID will be generated automatically)
                   Internally, all components and other classes (if any)
                   will be using the same UUID.
                   The 'name' should be saved somewhere internally to prevent
                   creation of vectors with repeating names.
        ["comp"]["x"]     : value
        ["comp"]["alpha"] : value
        ["comp"][0]       : value
        ["unit"]          : (optional) unit for the entire vector
        ["comp"]["x"]["value"]  : value of the individual component
        ["comp"]["x"]["unit"]  : (optional) for individual component
        ["comp"]["list"]  : the value is a list(), defines the vector using integer index
        ["comp"]["start"] : if ["comp"] is a "list" this is the index of the first component. Default is 1.
        ["+/-"]           : appends another vector 'uncertainty' to this file.
        ["label"]         : A label assigned to the entire vector (valid only for UnitCellLatticeVector class)

        Other keys are also possible. There will be warnings for unknown/unsupported keys.
        """
        output = []
        #logging.error(" get_csv_arr() is not implemented yet")
        # FIXME TODO

        self.uuid, self.uuid4 = self.uuidDB.addUUID(
                                    self.tbox_prefix + self.class_name,
                                    self.abox_prefix + self.item_name)

        output.append([self.uuid, "Instance",
                       self.tbox_prefix + self.class_name, "", "", ""])
        output.append([subject, "Instance", self.uuid, predicate, "", ""])

        # Global unit of the vector:
        if self.unit is not None:
            output += omSetUnit(self.uuid, self.unit)

        if self.vectorLabel is not None:
            output.append([self.ontoPrefix + "hasVectorLabel", "Data Property",
                           self.uuid, "", self.vectorLabel, "xsd:string"])

        if self.comp_list is not None:
            output += self._getCsvValueList()

        if self.comp_dict is not None:
            output += self._getCsvValueDict()

        return output

        '''
    # Old version: Verification of the class and its components:
    if self.value["class"] in ["Vector"]:
      # Classes derived from the basic 'Vector':
      if "error" in keys:
        logging.error(" Vector class '" + self.value["class"] + "' has an error value: " +
                       self.value["error"] + ". Use class 'VectorWithUncertainty' " +
                       "instead or a class derived from it.")
      pass

    elif self.value["class"] in ["UnitCellAngles", "UnitCellLengths",
                                 "UnitCellAngles", "PositionVector"]:
      # Classes derived from 'VectorWithUncertainty':

      self.value["error"] = dict() # the error bars a.k.a. uncertainty. Optional parameter.
      pass

    elif self.value["class"] in ["UnitCellLatticeVector"]:
      # Classes derived from 'UnitCellLatticeVector':

      self.value["error"] = dict() # the error bars a.k.a. uncertainty. Optional parameter.

      if "label" in keys:
        output.append([self.ontoPrefix + "hasVectorLabel", "Data Property",
                       self.uuid, "", self.value["label"], "xsd:string"])

      if "index" in keys:
        output.append([self.ontoPrefix + "hasComponentIndex", "Data Property",
                       self.uuid, "", self.value["index"], "xsd:integer"])

      #else:
      #  logging.warning(" Label is not defined for '" + self.value["name"] +
      #                  "' of class '" + myClass + "'.")
      pass

    elif "MillerIndices" == self.value["class"]:
      pass
    else:
      logging.error(" Unknown vector class '" + self.value["class"] + "' "
                      "for vector '" + self.value["name"] + "'.")
      pass

    return output
    '''
        # === end of OntoVector.get_csv_arr()

    def _getCsvValueList(self):
        output = []

        for iv, v in enumerate(self.comp_list):
            if v is None:
                logging.error("Invalid value '%s' in '%s[%d]'.",
                              str(v), self.item_name, iv)
                continue

            uuid_comp, _ = self.uuidDB.addUUID("VectorComponent",
                                               self.item_name + "_comp_" + str(iv+1),
                                               newUuid=self.uuid4)

            output.append([self.abox_prefix + uuid_comp, "Instance",
                          self.ontoPrefix + "VectorComponent", "", "", ""])

            output.append([self.uuid, "Instance", self.abox_prefix + uuid_comp,
                          self.ontoPrefix + "hasVectorComponent", "", ""])

            if self.compUnitList is not None:
                u = self.compUnitList[iv]
                if u is not None:
                    output += omSetUnit(self.abox_prefix + uuid_comp, u)

            output.append([self.ontoPrefix + "hasComponentIndex", "Data Property",
                          self.abox_prefix + uuid_comp, "", (iv + 1), "xsd:integer"])

            output.append([self.ontoPrefix + "hasComponentValue", "Data Property",
                          self.abox_prefix + uuid_comp, "", v, "rdfs:Literal"])

            if self.compErrList is not None:
                e = self.compErrList[iv]
                if e is not None:
                    output.append([self.ontoPrefix + "hasComponentUncertainty",
                                  "Data Property", self.abox_prefix + uuid_comp,
                                  "", e, "rdfs:Literal"])


        return output
        # === end of OntoVector._getCsvValueList()

    def _getCsvValueDict(self):
        output = []

        for ik, k in enumerate(self.comp_dict.keys()):
            if k is None:
                logging.error("Invalid value '%s' in '%s'.",
                              str(k), self.item_name)
                continue

            uuid_comp, _ = self.uuidDB.addUUID("VectorComponent",
                                               self.item_name + "_comp_" + k,
                                               newUuid=self.uuid4)

            output.append([self.abox_prefix + uuid_comp, "Instance",
                          crystOntoPrefix + "VectorComponent", "", "", ""])

            output.append([self.uuid, "Instance", self.abox_prefix + uuid_comp,
                          crystOntoPrefix + "hasVectorComponent", "", ""])

            if "unit" in self.comp_dict[k]:
                u = self.comp_dict[k]["unit"]
                if u is not None:
                    output += omSetUnit(self.abox_prefix + uuid_comp, u)

            if "label" in self.comp_dict[k]:
                output.append([self.ontoPrefix + "hasComponentLabel",
                                 "Data Property", self.abox_prefix + uuid_comp,
                                 "", self.comp_dict[k]["label"], "xsd:string"])

            if "value" in self.comp_dict[k]:
                output.append([self.ontoPrefix + "hasComponentValue",
                                 "Data Property", self.abox_prefix + uuid_comp,
                                 "", self.comp_dict[k]["value"], "rdfs:Literal"])

            if "error" in self.comp_dict[k]:
                output.append([self.ontoPrefix + "hasComponentUncertainty",
                                 "Data Property", self.abox_prefix + uuid_comp, "",
                                 self.comp_dict[k]["error"], "rdfs:Literal"])

            if "index" in self.comp_dict[k]:
                output.append([self.ontoPrefix + "hasComponentIndex",
                                 "Data Property", self.abox_prefix + uuid_comp, "",
                                 self.comp_dict[k]["index"], "xsd:integer"])
           # output.append([self.ontoPrefix + "hasComponentIndex", "Data Property",
           #                self.abox_prefix + uuid_comp, "", (iv + 1), "xsd:integer"])

        return output
        # === end of OntoVector._getCsvValueDict()

    # === end of class OntoVector

class OntoMatrix:
    __slots__ = ["uuidDB", "uuid", "uuid4", "myId", "unit",
                 "class_name", "item_name",
                 "compList", "compErrList", "compUnitList", "compDict",
                 "tbox_prefix", "abox_prefix", "ontoPrefix"
                ]
    def __init__(self, class_name, item_name, uuidDB=None,
                 tbox_prefix=None, abox_prefix=None,
                 unit=None):

        """
        if is_http(predicate):
            short = predicate.split("/")[-1]
        else:
            short = predicate

        if not short.startswith("has"):
            logging.warning(" Predicate in OntoMatrix.get_csv_arr() is '" + \
                             predicate + "'," + " but expecting it to have '" + \
                             "has" + "'.")
        """
        if "" == item_name:
            logging.error(" In OntoVector the entity name is not specified." + \
                           " Critical error!")
            return
        else:
            self.item_name = item_name


            if "" == class_name:
                logging.warning(" In OntoMatrix the class name is not" +
                                 " specified for '%s'." +
                                 " Using default 'Matrix'.", self.item_name)
                self.class_name = "MeasureMatrix"
            else:
                self.class_name = class_name

        if isinstance(uuidDB, tools.UuidDB):
            self.uuidDB = uuidDB
        else:
            self.uuidDB = tools.UuidDB(uuidDB)
            logging.warning(" Creating a new UuidDB database, in file" +
                            " '%s'.", self.uuidDB.dbFilename)

        if "" == unit or unit is None:
            logging.warning(" In OntoMatrix unit is not specified" +
                            " for '%s'.", self.itemName)
            self.unit = None
        else:
            self.unit = unit

        if tbox_prefix is None:
            self.tbox_prefix = ""
        else:
            self.tbox_prefix = tbox_prefix

        if abox_prefix is None:
            self.abox_prefix = ""
        else:
            self.abox_prefix = abox_prefix

        # The Matrix class is defined in the OntoCrystal ontology:
        self.ontoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"

        self.compList     = []
        self.compErrList  = []
        self.compUnitList = []
        self.compDict     = {}

        # === end of OntoMatrix.__init__()

    def addComponent(self, label, value=None, error=None, unit=None):
        err_count = 0

        if not isinstance(self.compDict, dict):
            self.compDict = {}

        if isinstance(label, str):
            if "" == label:
                logging.error(" Not specified label for vector component" +
                              " in '%s'. 22", self.item_name)
                err_count += 1
        else:
            logging.warning("label = '%s' is not a string in '%s.",
                            str(label), self.item_name)

        if label in self.compDict:
            logging.error("Over-writing an existing component with label" +
                          "'%s' for Matrix '%s'.", label, self.item_name)

        self.compDict[label] = {}
        if value is not None:
            self.compDict[label]["value"] = value

        if error is not None:
            self.compDict[label]["error"] = error

        if unit is not None:
            self.compDict[label]["unit"] = unit

        # === end of OntoMatrix.addComponent()

    def addComponentList(self, valList=None, errList=None, unit=None):

        if [] != self.compList:
            logging.warning(" Over-writing existing values in vector '%s'",
                            self.item_name)
            print("   >", self.compList)

        if [] != self.compErrList:
            logging.warning(" Over-writing existing errors in vector '%s'",
                            self.item_name)

        if [] != self.compUnitList:
            logging.warning(" Over-writing existing units in vector '%s'.",
                            self.item_name)

        #-----------------------------------
        if isinstance(valList, list):
            self.compList = []

            for l in valList:
                if isinstance(l, list):
                    self.compList.append(list(l))
                else:
                    logging.error(" In OntoVector.addComponentList() expect" +
                                  " a list of list, got '%s' for value.",
                                  str(type(l)))
        else:
            logging.error(" In OntoVector.addComponentList() expect a list," +
                          " got '%s' for value.", str(type(valList)))

        #-----------------------------------
        if isinstance(errList, list):
            self.compErrList = []

            for il, l in enumerate(errList):
                if 0 == il and len(valList) != len(errList):
                    logging.error(" In OntoVector.addComponentList() the" +
                                  " size of err %d is not equal to size" +
                                  " of val %d, skipping error list.",
                                  str(len(errList)), str(len(valList)))
                    break

                if isinstance(l, list):
                    if len(valList[il]) == len(l):
                        self.compErrList.append(list(l))
                    else:
                        logging.error(" In OntoVector.addComponentList() the" +
                                      " size of err %d is not equal to size" +
                                      " of val %d, skipping error list.",
                                      str(len(errList[il])), str(len(valList[il])))
                else:
                    logging.error(" In OntoVector.addComponentList() expect" +
                                  " a list of list, got '%s" "' for error.",
                                  str(type(l)))
        else:
            logging.error(" In OntoVector.addComponentList() expect a list," +
                          " got '%s' for error.", str(type(errList)))

        #-----------------------------------
        if isinstance(unit, str):
            self.compUnitList = [[unit]*len(row) for row in valList]
        elif isinstance(unit, list):
            self.compUnitList = []

            for il, l in enumerate(unit):
                if 0 == il and len(valList) != len(unit):
                    logging.error(" In OntoVector.addComponentList() the" +
                                  " size of unit list %s is not equal" +
                                  " to size of val %s, skipping unit list.",
                                  str(len(unit)), str(len(valList)))
                    break

                if isinstance(l, list):
                    if len(valList[il]) == len(l):
                        self.compUnitList.append(list(l))
                    else:
                        logging.error(" In OntoVector.addComponentList() the" +
                                      " size of unit %d is not equal to size" +
                                      " of val %d, skipping unit list.",
                                      len(unit[il]), len(l) )
                else:
                    logging.error(" In OntoVector.addComponentList() expect" +
                                  " a list of list, got '%s' for unit.",
                                  str(type(l)))

            #logging.error(" Not implemented addComponentList() e3e3e3.")
        else:
            logging.error(" In OntoVector.addComponentList() expect" +
                          " str or list, got '%s' for unit.", str(type(unit)))

        #-----------------------------------

        # === end of OntoMatrix.addComponentList()

    def get_csv_arr(self, subject, predicate):
        if is_http(predicate):
            short = predicate.split("/")[-1]
        else:
            short = predicate

        if not short.startswith("has"):
            logging.warning(" Predicate in OntoMatrix.get_csv_arr() is '%s'," +
                            " but expecting it to have 'has'.", predicate)
        output = []

        self.uuid, self.uuid4 = self.uuidDB.addUUID(
                                self.tbox_prefix + self.class_name,
                                self.abox_prefix + self.item_name)

        output.append([self.uuid, "Instance",
                       self.tbox_prefix + self.class_name, "", "", ""])
        output.append([subject, "Instance", self.uuid, predicate, "", ""])

        # Global unit of the vector:
        if self.unit is not None:
            output += omSetUnit(self.uuid, self.unit)

        #if None != self.vectorLabel:
        #    output.append([self.ontoPrefix + "hasVectorLabel", "Data Property",
        #    self.uuid, "", self.vectorLabel, "xsd:string"])


        logging.error(" Not implemented OntoMatrix.get_csv_arr ttttttt")

        if self.compList is not None:
            output += self._getCsvValueList()

        if self.compDict is not None:
            output += self._getCsvValueDict()

        return output
        # === end of OntoMatrix.get_csv_arr()

    def _getCsvValueList(self):
        output = []
        #logging.error("Not implemented Matrix._getCsvValueList() ttt3")

        for ir, row in enumerate(self.compList):
            for ic, val in enumerate(row):
                if val is None:
                    logging.error("Invalid value '%s' in '%s'.", str(val), self.itemName)
                    continue

                uuid_comp,_ = self.uuidDB.addUUID("MatrixComponent",
                              self.item_name + "_comp_" + str(ir+1) + "_" + str(ic+1),
                              newUuid = self.uuid4)

                output.append([self.abox_prefix + uuid_comp, "Instance",
                               self.ontoPrefix + "MatrixComponent", "", "", ""])

                output.append([self.uuid, "Instance", self.abox_prefix + uuid_comp, \
                               self.ontoPrefix + "hasMatrixComponent", "", ""])

                if self.compUnitList is not None:
                    u = self.compUnitList[ir][ic]
                    if u is not None:
                        output += omSetUnit(self.abox_prefix + uuid_comp, u)

                output.append([self.ontoPrefix + "hasRowIndex", "Data Property",
                               self.abox_prefix + uuid_comp, "", (ir + 1), "xsd:integer"])

                output.append([self.ontoPrefix + "hasColumnIndex", "Data Property",
                               self.abox_prefix + uuid_comp, "", (ic + 1), "xsd:integer"])

                output.append([self.ontoPrefix + "hasComponentValue", "Data Property",
                               self.abox_prefix + uuid_comp, "", val, "rdfs:Literal"])

                if self.compErrList is not None:
                    e = self.compErrList[ir][ic]
                    if e is not None:
                        output.append([self.ontoPrefix + "hasComponentUncertainty",
                                       "Data Property", self.abox_prefix + uuid_comp,
                                       "", e, "rdfs:Literal"])

        return output
        # === end of OntoMatrix._getCsvValueList()

    def _getCsvValueDict(self):
        output = []
        logging.error("Not implemented OntoMatrix._getCsvValueDict() ttt4")

        return output
        # === end of OntoMatrix._getCsvValueDict()
    """
    """

    # === end of class OntoMatrix


class OntoPlot:
    """
    uuid - is the unique name of the entity of this class.
    """
    __slots__ = ["uuidDB", "plotId", "uuid",  # "",
                 "curveList",  # ""
                 ]

    def __init__(self, uuidDB=None, plotId=None):
        self.uuidDB = uuidDB
        self.plotId = plotId

    # prefix = "", uuidDB = None, myClass = "",
    #                    myName = "", myUnit = ""):
        self.curveList = []

        # === end of OntoPlot.__init__()

    def addCurves(self, listAbscissa, listOrdinate):
        if not isinstance(listAbscissa, list):
            logging.error(" In addCurve listAbscissa must be a list.")
            return
        if not isinstance(listOrdinate, list):
            logging.error(" In addCurve listOrdinate must be a list.")
            return

        for a in listAbscissa:
            if not isinstance(a, list):
                logging.error(" In addCurve listAbscissa must be" +
                              "a list of lists.")
                return

            for o in listOrdinate:
                if not isinstance(o, list):
                    logging.error(" In addCurve listOrdinate must be" +
                                  "a list of lists.")
                    return

                self.curveList.append([a, o])

        # === end of OntoPlot.addCurves()

    def get_csv_arr(self, subject, predicate):
        """
        subject   - Is the full hame of instance of class,
                    which contains this plot class.
        predicate - Is the Object Property linking the Subject and the
                    current Plot. Typically is should be contain "has".
        """
        if subject.find("XRDSpectrum") < 0:
            logging.warning(" OntoPlot expects a subject 'XRDSpectrum', " +
                            "but got '%s'.", subject)
        if predicate.find("hasPlot") < 0:
            logging.warning(" OntoPlot expects a predicate 'hasPlot', " +
                            "but got '%s'.", predicate)

        output = []

        if self.plotId is None:
            logging.warning(" Neither plotId, nor '' is specified. " +
                            "I use a random number as label.")
            plotLabel = tools.getUUID_random("")
        else:
            plotLabel = self.cifLabel
    # elif label != None:
    #    atomLabel = str(label)

        self.uuid, _ = self.uuidDB.addUUID("PlotXY", "PlotXY_" + plotLabel)
        # self.uuid = tools.getUUID(self.uuidDB, "PlotXY",
        #                           "PlotXY_" + plotLabel)
        output.append([self.uuid, "Instance", "PlotXY", "", "", ""])

        output.append([subject, "Instance", self.uuid, predicate, "", ""])

        for ic, curve in enumerate(self.curveList):
            plotX = OntoVector(uuidDB=self.uuidDB,
                               class_name="AbscissaData",
                               item_name=plotLabel + "_Plot" + str(ic+1) + "X",
                               unit="om:dimensionOne")

            plotX.addComponentList(curve[0], unit="om:degree")

            output += plotX.get_csv_arr(uuid_plot,
                               self.crystOntoPrefix + "hasAbscissaData")

            plotY = OntoVector(uuidDB=self.uuidDB,
                               class_name="OrdinateData",
                               item_name=plotLabel + "_Plot" + str(ic+1) + "Y",
                               unit="om:dimensionOne")

            plotY.addComponentList(curve[1], unit="om:dimensionOne")

            output += plotY.get_csv_arr(uuid_plot,
                                         self.crystOntoPrefix + "hasOrdinate")

            output.append([plotX.uuid, "Instance", plotY.uuid,
                          self.ontoPrefix + "isAbscissaOf", "", ""])

        return output
        # === end of OntoPlot.get_csv_arr()

    # === end of class OntoPlot

if __name__ == "__main__":
    pass
