""" Module implements the datatypes defined in OntoCrystal ontology.

Includes: OntoMeasureWithUncertainty, OntoVector, OntoMatrix, OntoPlot.
See also test_ontocrystal_datatypes.py for the tests and use examples.

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

        tbox_prefix - It is possible to assign prefix for TBox for the class and
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

    def get_csv_arr(self, subject, predicate, new_uuid=None):
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
        if new_uuid:
            self.uuid = self.abox_prefix + self.item_name + "_" + new_uuid
        else:
            self.uuid, _ = self.uuidDB.addUUID(self.tbox_prefix + self.class_name,
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

    tbox_prefix - TODO Same as OntoMeasureWithUncertainty.

                 (optional) the prefix for TBox, where a class derived from this class is defined.
                ###### If not specified OntoCrystal is used.
                If not specified, the entity will not have any prefix
                      (i.e. will use the global tbox prefix from csv file)
                It is applied only for the main element of this class,
                because other (VectorComponent, etc) are defined in OntoCrystal.

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
    __slots__ = ["uuidDB", "class_name", "item_name", "iri", "uuid4",
                 #"value",
                 "tbox_prefix", "abox_prefix",
                 # "ontoPrefix",
                 "uuid_error", # <- old options, need to remove
                 "unit", "vector_label",
                 "comp_list", "comp_err_list", "comp_unit_list",
                 "comp_dict" #, "compErrDict"
                ]
    def __init__(self, item_name, class_name=None, uuidDB=None,
                 tbox_prefix=None, abox_prefix=None,
                 unit=None, vector_label=None):
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
            return
        else:
            self.item_name = item_name


        if class_name is None:
            logging.warning(" In OntoVector the class name is not specified" +
                            " for '%s'. Using default 'Vector'.",
                            self.item_name)
            self.class_name = "Vector"
        else:
            if isinstance(class_name, str):
                self.class_name = class_name.strip()
            else:
                logging.error(" In OntoVector class name is invalid '%s'.",
                              str(class_name))
                self.class_name = str(class_name)

        if isinstance(uuidDB, tools.UuidDB):
            self.uuidDB = uuidDB
        else:
            self.uuidDB = tools.UuidDB(uuidDB)
            logging.warning(" Creating a new UuidDB database, in file '%s'.",
                            self.uuidDB.dbFilename)

        if "" == unit or unit is None:
            logging.warning(" In OntoVector unit is not specified for '%s'.",
                            self.item_name)
            self.unit = None
        else:
            self.unit = unit

        if vector_label is None:
            if "UnitCellLatticeVector" == self.class_name:
                logging.error(" In OntoVector label is not specified" +
                              " for '%s'.", self.item_name)
            self.vector_label = vector_label
        else:
            self.vector_label = vector_label
            if self.class_name in ["MeasureVector", "MillerIndices"]:
                logging.error(" In OntoVector class '%s'" +
                              " should not have vector_label, but given '%s'.",
                              self.class_name, self.vector_label)

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
        #self.ontoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"
        #if "" == prefix:
        #  logging.error(" In OntoVector ontology prefix is not specified" +
        #                 " for '" + self.itemName + "'.")
        #else:
        #  self.ontoPrefix = prefix

        #self.value["comp"] = list()


        #self.value["list"] = dict()
        self.comp_list      = []
        self.comp_err_list  = []
        self.comp_unit_list = []

        self.comp_dict      = {}
        #self.compErrDict   = {}

        self.iri        = None  # Meaning?
        self.uuid4      = None  # Meaning?
        # This is used to avoid double creation of the Uncertainty Vector:
        self.uuid_error = None  # Meaning?

        #self.error = {}

        # === end of OntoVector.__init__()

    def addComponentList(self, val_list, unit=None, err_list=None):
        """
        The size of err_list must be the same as val_list,
        The err_list may contain 'None' values for some entries.
        In this case the output will not have an uncertainty.
        """
        # logging.error(" in CsvMaker addComponentList() is not implemented")

        if [] != self.comp_list:
            logging.warning(" Over-writing existing values in vector '%s'.",
                            self.item_name)
            #print("   >", self.comp_list)

        if [] != self.comp_err_list:
            logging.warning(" Over-writing existing errors in vector '%s'.",
                            self.item_name)

        if [] != self.comp_unit_list:
            logging.warning(" Over-writing existing units in vector '%s'.",
                            self.item_name)

        #-----------------------------------
        if isinstance(val_list, list):
            self.comp_list = list(val_list)
        else:
            logging.error(" In OntoVector.addComponentList() expect a list," +
                          " got '%s'.", str(type(val_list)))
            self.comp_list = None

        #for il, el in enumerate(val_list):
        #  self.value["list"] = None
        #  pass

        #-----------------------------------
        if err_list is None:
            # Do nothing, no error list
            self.comp_err_list = None

        elif isinstance(err_list, list):
            if len(val_list) != len(err_list):
                logging.error(" In OntoVector.addComponentList() the size" +
                              " of err %d is not equal to size of val %d," +
                              " skipping error list.",
                              len(err_list), len(val_list))
            else:
                self.comp_err_list = list(err_list)

        else:
            logging.error(" In OntoVector.addComponentList() expect an" +
                          " error list, got '%s'.", str(type(err_list)))
            self.comp_err_list = None

        #-----------------------------------
        if "" == unit or unit is None:
            logging.info(" No unit in vector component list")
            self.comp_unit_list = None
        elif isinstance(unit, list):
            # Individual unit for each component, as a list
            if len(val_list) != len(unit):
                logging.error(" In OntoVector.addComponentList() the size" +
                              " of unit %d is not equal to size of val %d," +
                              " skipping unit list.", len(unit), len(val_list))
            self.comp_unit_list = list(unit)
        elif isinstance(unit, str):
            if unit.startswith("om:"):
                #logging.error(" Not implemented unit in vector list")
                self.comp_unit_list = [unit] * len(val_list)
            else:
                logging.error(" In OntoVector.addComponentList() unknown" +
                              " unit value '%s', expecting prefix om:", unit)
                self.comp_unit_list = None
        else:
            logging.error(" Unknown case for unit '%s'.", str(unit))
            # Do nothing. Need a warning?

        # === end of OntoVector.addComponentList()

    def addComponent(self, label, value="", unit="", error="", index=None):
        """
        Adding an individual component to a vector.
        Internally uses a dictionary.
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

    def get_csv_arr(self, subject, predicate, new_uuid=None):
        """
        subject   - the full hame of instance of class,
                    which contains this Vector class.
        predicate - the Object Property linking the Subject and the current UnitCell.
                    Typically is should be contain "has".
        new_uuid  - the UUID value to be used for this vector, and its properties.

        # Value is not guaranteed:
        if subject.find("CrystalInformation") < 0:
            logging.warning(" Subject in arrUnitCell() is '" + subject + "'," +
                            " expecting the name to contain '" + "CrystalInfromation" + "'.")
        """

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
        if new_uuid:
            self.uuid4 = new_uuid
            #self.iri = self.tbox_prefix + self.class_name
            self.iri = self.abox_prefix + self.item_name + "_" + self.uuid4
        else:
            self.iri, self.uuid4 = self.uuidDB.addUUID(
                                    self.tbox_prefix + self.class_name,
                                    self.abox_prefix + self.item_name)

        output.append([self.iri, "Instance",
                       self.tbox_prefix + self.class_name, "", "", ""])
        output.append([subject, "Instance", self.iri, predicate, "", ""])

        # Global unit of the vector:
        if self.unit is not None:
            output += omSetUnit(self.iri, self.unit)

        if self.vector_label is not None:
            output.append([crystOntoPrefix + "hasVectorLabel", "Data Property",
                           self.iri, "", self.vector_label, "xsd:string"])

        if self.comp_list is not None:
            output += self._get_csv_value_list(new_uuid=new_uuid)

        if self.comp_dict is not None:
            output += self._get_csv_value_dict(new_uuid=new_uuid)

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

    def _get_csv_value_list(self, new_uuid=None):
        output = []

        for iv, v in enumerate(self.comp_list):
            if v is None:
                logging.error("Invalid value '%s' in '%s[%d]'.",
                              str(v), self.item_name, iv)
                continue

            if new_uuid:
                # print("new_uuid =",new_uuid)
                self.uuid4 = new_uuid
                comp_iri = self.abox_prefix + self.item_name + \
                           "_" + str(iv+1) + "_" + self.uuid4
            else:
                comp_iri, _ = self.uuidDB.addUUID("VectorComponent",
                                                  self.abox_prefix +  self.item_name + "_" + str(iv+1),
                                                  new_uuid=self.uuid4)

            output.append([comp_iri, "Instance",
                          crystOntoPrefix + "VectorComponent", "", "", ""])

            output.append([self.iri, "Instance", comp_iri,
                          crystOntoPrefix + "hasVectorComponent", "", ""])

            if self.comp_unit_list is not None:
                u = self.comp_unit_list[iv]
                if u is not None:
                    output += omSetUnit(comp_iri, u)

            output.append([crystOntoPrefix + "hasComponentIndex", "Data Property",
                          comp_iri, "", (iv + 1), "xsd:integer"])

            output.append([crystOntoPrefix + "hasComponentValue", "Data Property",
                           comp_iri, "", v, "rdfs:Literal"])

            if self.comp_err_list is not None:
                e = self.comp_err_list[iv]
                if e is not None:
                    output.append([crystOntoPrefix + "hasComponentUncertainty",
                                  "Data Property", comp_iri,
                                  "", e, "rdfs:Literal"])

        return output
        # === end of OntoVector._get_csv_value_list()

    def _get_csv_value_dict(self, new_uuid=None):
        #print("self.abox_prefix =", self.abox_prefix, ">", self.class_name, self.item_name)
        output = []

        for ik, k in enumerate(self.comp_dict.keys()):
            if k is None:
                logging.error("Invalid value '%s' in '%s'.",
                              str(k), self.item_name)
                continue

            if new_uuid:
                self.uuid4 = new_uuid
                uuid_comp = self.abox_prefix + self.item_name + \
                           "_" + k + "_" + self.uuid4
            else:
                uuid_comp, _ = self.uuidDB.addUUID("VectorComponent",
                                                   self.item_name + "_" + k,
                                                   new_uuid=self.uuid4)

            output.append([uuid_comp, "Instance",
                          crystOntoPrefix + "VectorComponent", "", "", ""])

            output.append([self.iri, "Instance", uuid_comp,
                          crystOntoPrefix + "hasVectorComponent", "", ""])

            if "unit" in self.comp_dict[k]:
                u = self.comp_dict[k]["unit"]
                if u is not None:
                    output += omSetUnit(uuid_comp, u)

            if "label" in self.comp_dict[k]:
                output.append([crystOntoPrefix + "hasComponentLabel",
                                 "Data Property", uuid_comp,
                                 "", self.comp_dict[k]["label"], "xsd:string"])

            if "value" in self.comp_dict[k]:
                output.append([crystOntoPrefix + "hasComponentValue",
                                 "Data Property", uuid_comp,
                                 "", self.comp_dict[k]["value"], "rdfs:Literal"])

            if "error" in self.comp_dict[k]:
                output.append([crystOntoPrefix + "hasComponentUncertainty",
                                 "Data Property", uuid_comp, "",
                                 self.comp_dict[k]["error"], "rdfs:Literal"])

            if "index" in self.comp_dict[k]:
                output.append([crystOntoPrefix + "hasComponentIndex",
                                 "Data Property", uuid_comp, "",
                                 self.comp_dict[k]["index"], "xsd:integer"])
           # output.append([crystOntoPrefix + "hasComponentIndex", "Data Property",
           #                uuid_comp, "", (iv + 1), "xsd:integer"])

        return output
        # === end of OntoVector._get_csv_value_dict()

    # === end of class OntoVector

class OntoMatrix:
    __slots__ = ["uuidDB", "uuid", "uuid4", "myId", "unit",
                 "class_name", "item_name",
                 "comp_list", "comp_err_list", "comp_unit_list", "comp_dict",
                 "tbox_prefix", "abox_prefix"  #, "ontoPrefix"
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
            logging.error(" In OntoMatrix the entity name is not specified." + \
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
                            " for '%s'.", self.item_name)
            self.unit = None
        else:
            self.unit = unit

        if tbox_prefix is None:
            self.tbox_prefix = crystOntoPrefix
        else:
            self.tbox_prefix = tbox_prefix

        if abox_prefix is None:
            self.abox_prefix = ""
        else:
            self.abox_prefix = abox_prefix

        # The Matrix class is defined in the OntoCrystal ontology:
        #self.ontoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"

        self.comp_list     = []
        self.comp_err_list  = []
        self.comp_unit_list = []
        self.comp_dict     = {}

        # === end of OntoMatrix.__init__()

    def addComponent(self, label, value=None, error=None, unit=None):
        err_count = 0

        if not isinstance(self.comp_dict, dict):
            self.comp_dict = {}

        if isinstance(label, str):
            if "" == label:
                logging.error(" Not specified label for vector component" +
                              " in '%s'. 22", self.item_name)
                err_count += 1
        else:
            logging.warning("label = '%s' is not a string in '%s.",
                            str(label), self.item_name)

        if label in self.comp_dict:
            logging.error("Over-writing an existing component with label" +
                          "'%s' for Matrix '%s'.", label, self.item_name)

        self.comp_dict[label] = {}
        if value is not None:
            self.comp_dict[label]["value"] = value

        if error is not None:
            self.comp_dict[label]["error"] = error

        if unit is not None:
            self.comp_dict[label]["unit"] = unit

        # === end of OntoMatrix.addComponent()

    def addComponentList(self, val_list=None, err_list=None, unit=None):

        if [] != self.comp_list:
            logging.warning(" Over-writing existing values in vector '%s'",
                            self.item_name)
            print("   >", self.comp_list)

        if [] != self.comp_err_list:
            logging.warning(" Over-writing existing errors in vector '%s'",
                            self.item_name)

        if [] != self.comp_unit_list:
            logging.warning(" Over-writing existing units in vector '%s'.",
                            self.item_name)

        #-----------------------------------
        if isinstance(val_list, list):
            self.comp_list = []

            for l in val_list:
                if isinstance(l, list):
                    self.comp_list.append(list(l))
                else:
                    logging.error(" In OntoMatrix.addComponentList() expect" +
                                  " a list of list, got '%s' for value.",
                                  str(type(l)))
        else:
            logging.error(" In OntoMatrix.addComponentList() expect a list," +
                          " got '%s' for value.", str(type(val_list)))
        #print(">>>>>>>>>>> Matrix.addCompList", val_list)
        #print("            Matrix.addCompList", self.comp_list)

        #-----------------------------------
        if err_list is None:
            pass
        elif isinstance(err_list, list):
            self.comp_err_list = []

            for il, l in enumerate(err_list):
                if 0 == il and len(val_list) != len(err_list):
                    logging.error(" In OntoMatrix.addComponentList() the" +
                                  " size of err %d is not equal to size" +
                                  " of val %d, skipping error list.",
                                  str(len(err_list)), str(len(val_list)))
                    break

                if isinstance(l, list):
                    if len(val_list[il]) == len(l):
                        self.comp_err_list.append(list(l))
                    else:
                        logging.error(" In OntoMatrix.addComponentList() the" +
                                      " size of err %d is not equal to size" +
                                      " of val %d, skipping error list.",
                                      str(len(err_list[il])), str(len(val_list[il])))
                else:
                    logging.error(" In OntoMatrix.addComponentList() expect" +
                                  " a list of list, got '%s" "' for error.",
                                  str(type(l)))
        else:
            logging.error(" In OntoMatrix.addComponentList() expect a list," +
                          " got '%s' for error.", str(type(err_list)))

        #-----------------------------------
        if unit is None:
            pass
        elif isinstance(unit, str):
            self.comp_unit_list = [[unit]*len(row) for row in val_list]
        elif isinstance(unit, list):
            self.comp_unit_list = []

            for il, l in enumerate(unit):
                if 0 == il and len(val_list) != len(unit):
                    logging.error(" In OntoMatrix.addComponentList() the" +
                                  " size of unit list %s is not equal" +
                                  " to size of val %s, skipping unit list.",
                                  str(len(unit)), str(len(val_list)))
                    break

                if isinstance(l, list):
                    if len(val_list[il]) == len(l):
                        self.comp_unit_list.append(list(l))
                    else:
                        logging.error(" In OntoMatrix.addComponentList() the" +
                                      " size of unit %d is not equal to size" +
                                      " of val %d, skipping unit list.",
                                      len(unit[il]), len(l) )
                else:
                    logging.error(" In OntoMatrix.addComponentList() expect" +
                                  " a list of list, got '%s' for unit.",
                                  str(type(l)))

            #logging.error(" Not implemented addComponentList() e3e3e3.")
        else:
            logging.error(" In OntoMatrix.addComponentList() expect" +
                          " str or list, got '%s' for unit.", str(type(unit)))

        #-----------------------------------

        # === end of OntoMatrix.addComponentList()

    def get_csv_arr(self, subject, predicate, new_uuid=None):
        if is_http(predicate):
            short = predicate.split("/")[-1]
        else:
            short = predicate

        if not short.startswith("has"):
            logging.warning(" Predicate in OntoMatrix.get_csv_arr() is '%s'," +
                            " but expecting it to have 'has'.", predicate)
        output = []

        if new_uuid:
            self.uuid4 = new_uuid
            self.uuid = self.abox_prefix + self.item_name + "_" + new_uuid
        else:
            self.uuid, self.uuid4 = self.uuidDB.addUUID(
                                    self.tbox_prefix + self.class_name,
                                    self.abox_prefix + self.item_name)

        output.append([self.uuid, "Instance",
                       self.tbox_prefix + self.class_name, "", "", ""])
        output.append([subject, "Instance", self.uuid, predicate, "", ""])

        # Global unit of the vector:
        if self.unit is not None:
            output += omSetUnit(self.uuid, self.unit)

        #if None != self.vector_label:
        #    output.append([self.ontoPrefix + "hasVector_label", "Data Property",
        #    self.uuid, "", self.vector_label, "xsd:string"])


        #logging.error(" Not implemented OntoMatrix.get_csv_arr ttttttt")

        if self.comp_list is not None:
            output += self._get_csv_value_list(new_uuid=new_uuid)

        if self.comp_dict is not None:
            output += self._get_csv_value_dict(new_uuid=new_uuid)

        return output
        # === end of OntoMatrix.get_csv_arr()

    def _get_csv_value_list(self, new_uuid=None):
        output = []
        #logging.error("Not implemented Matrix._getCsvValueList() ttt3")

        for ir, row in enumerate(self.comp_list):
            for ic, val in enumerate(row):
                if val is None:
                    logging.error("Invalid value '%s' in '%s'.", str(val), self.item_name)
                    continue

                if new_uuid:
                    uuid_comp = self.abox_prefix  # + "MatrixComponent_"
                    #if self.item_name is not None and self.item_name != "":
                    uuid_comp += self.item_name + "_"
                    uuid_comp += str(ir+1) + "_" + str(ic+1) + "_"
                    uuid_comp += new_uuid
                else:
                    uuid_comp, _ = self.uuidDB.addUUID("MatrixComponent",
                                   self.item_name + "_" + str(ir+1) + "_" + str(ic+1),
                                   new_uuid = self.uuid4)

                output.append([uuid_comp, "Instance",
                               crystOntoPrefix + "MatrixComponent", "", "", ""])

                output.append([self.uuid, "Instance", uuid_comp, \
                               crystOntoPrefix + "hasMatrixComponent", "", ""])

                if self.comp_unit_list is not None and self.comp_unit_list != []:
                    u = self.comp_unit_list[ir][ic]
                    if u is not None:
                        output += omSetUnit(uuid_comp, u)

                output.append([crystOntoPrefix + "hasRowIndex", "Data Property",
                               uuid_comp, "", (ir + 1), "xsd:integer"])

                output.append([crystOntoPrefix + "hasColumnIndex", "Data Property",
                               uuid_comp, "", (ic + 1), "xsd:integer"])

                output.append([crystOntoPrefix + "hasComponentValue", "Data Property",
                               uuid_comp, "", val, "rdfs:Literal"])

                if self.comp_err_list is not None and self.comp_err_list != []:
                    e = self.comp_err_list[ir][ic]
                    if e is not None:
                        output.append([crystOntoPrefix + "hasComponentUncertainty",
                                       "Data Property", uuid_comp,
                                       "", e, "rdfs:Literal"])

        return output
        # === end of OntoMatrix._getCsvValueList()

    def _get_csv_value_dict(self, new_uuid=None):
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
