import pydantic
from typing import Any, Dict
from rdflib import Graph, Literal, URIRef
from rdflib.namespace import RDF

import hashlib
import base64

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

# TODO add below IRIs to pyasyncagent.data_model.iris, also TBox CSV/OWL if applicable
OM_CELSIUSTEMPERATURE = UNITS_OF_MEASURE + 'CelsiusTemperature'
ONTODERIVATION_BELONGSTO = ONTODERIVATION + 'belongsTo'
ONTODERIVATION_ISDERIVEDUSING = ONTODERIVATION + 'isDerivedUsing'
ONTODERIVATION_HASSTATUS = ONTODERIVATION + 'hasStatus'
DOEAGENT_SERVICE = 'http://www.theworldavatar.com/resource/agents/Service__DoE#Service'
EXEAGENT_SERVICE = 'http://www.theworldavatar.com/resource/agents/Service__Execution#Service'
OM_MINUTETIME = UNITS_OF_MEASURE + 'minute-Time'
OM_MASS = UNITS_OF_MEASURE + 'Mass'
OM_DENSITY = UNITS_OF_MEASURE + 'Density'
OM_SPECIFICAMOUNTOFMONEY = UNITS_OF_MEASURE + 'SpecificAmountOfMoney'
OM_AMOUNTOFSUBSTANCE = UNITS_OF_MEASURE + 'AmountOfSubstance'
OM_MOLE = UNITS_OF_MEASURE + 'mole'
OM_MOLEPERLITRE = UNITS_OF_MEASURE + 'molePerLitre'
OM_LITRE = UNITS_OF_MEASURE + 'litre'
OM_KILOGRAMPERLITRE = UNITS_OF_MEASURE + 'kilogramPerLitre'
OM_KILOGRAM = UNITS_OF_MEASURE + 'kilogram'
OM_KILOGRAMPERMOLE = UNITS_OF_MEASURE + 'kilogramPerMole'
OM_MILLILITRE = UNITS_OF_MEASURE + 'millilitre'
ONTOUOM_GRAMPERMOLE = ONTOUOM + 'gramPerMole' # TODO add this to OM TBox
ONTOSPECIES_HASMOLECULARWEIGHT = ONTOSPECIES + 'hasMolecularWeight'
ONTOSPECIES_UNITS = ONTOSPECIES + 'units'
ONTOSPECIES_VALUE = ONTOSPECIES + 'value'
ONTOUOM_POUNDSTERLINGPERLITRE = ONTOUOM + 'pound_sterling_per_litre' # TODO OntoRxn:RunMaterialCost should have the unit of OM_POUNDSTERLING
OM_ONE = UNITS_OF_MEASURE + 'one'
OM_DEGREECELSIUS = UNITS_OF_MEASURE + 'degreeCelsius'
OM_DEGREEFAHRENHEIT = UNITS_OF_MEASURE + 'degreeFahrenheit'
OM_KELVIN = UNITS_OF_MEASURE + 'kelvin'
ONTOUOM_KILOGRAMPERLITREPERMINUTE = ONTOUOM + 'kilogram_per_litre_per_minute'
OM_POUNDSTERLING = UNITS_OF_MEASURE + 'poundSterling'

class InstanceIRIInitialisationError(Exception):
    instance_iri_missing = """IRI of instance_iri is missing, please put it as pyasyncagent.data_model.utils.INSTANCE_IRI_TO_BE_INITIALISED \
    and provide namespace_for_init if you would like the instance_iri to be generated, \
    otherwise please put one that matches an existing instance IRI in the knowledge graph, \
    this error occurred when initialising a/an instance of: """
    clz_missing = 'IRI of clz is missing when initialising an instance of: '
    namespace_for_init_missing = 'IRI of namespace_for_init is missing when initialising an instance of: '
    both_clz_and_namespace_missing = 'Both IRI of clz and namespace_for_init are missing when initialising an instance of: '
    namespace_for_init_should_not_be_provided = 'IRI of namespace_for_init should NOT be provided as already provided instance_iri: '

    def __init__(self, value: str, message: str) -> None:
        full_message = message + value
        super().__init__(full_message)

class BaseOntology(pydantic.BaseModel):
    """The initialisation and validator sequence:
        (I) start to run BaseOntology.__init__(__pydantic_self__, **data) with **data as the raw input arguments;
        (II) run until super().__init__(**data), note data is updated within BaseOntology before sending to super().init(**data);
        (III) now within pydantic.BaseModel __init__:
            (i) run root_validator (for those pre=True), in order of how the root_validators are listed in codes;
            (ii) in order of how the fields are listed in codes:
                (1) run validator (for those pre=True) in order of how the validators (for the same field) are listed in codes;
                (2) run validator (for those pre=False) in order of how the validators (for the same field) are listed in codes;
            (iii) (if we are instantiating a child class of BaseOntology) load default values in the child class (if they are provided)
                  and run root_validator (for those pre=False) in order of how the root_validators are listed in codes,
                  e.g. clz='clz provided in the child class' will be added to 'values' of the input argument of root_validator;
        (IV) end pydantic.BaseModel __init__;
        (V) end BaseOntology __init__"""
    instance_iri: str = None
    clz: str = None
    namespace_for_init: str = None

    def __init__(__pydantic_self__, **data: Any) -> None:
        _instance_iri = data['instance_iri'] if 'instance_iri' in data else None
        _clz = data['clz'] if 'clz' in data else __pydantic_self__.__class__.__fields__['clz'].default
        _namespace_for_init = data['namespace_for_init'] if 'namespace_for_init' in data else None
        if _instance_iri is None:
            raise InstanceIRIInitialisationError(
                value=str(__pydantic_self__.__class__), message=InstanceIRIInitialisationError.instance_iri_missing
            ) if _clz is None else InstanceIRIInitialisationError(value=_clz, message=InstanceIRIInitialisationError.instance_iri_missing)
        elif data['instance_iri'] == INSTANCE_IRI_TO_BE_INITIALISED:
            if _clz is None and _namespace_for_init is None:
                raise InstanceIRIInitialisationError(value=str(__pydantic_self__.__class__), message=InstanceIRIInitialisationError.both_clz_and_namespace_missing)
            elif _clz is None:
                raise InstanceIRIInitialisationError(value=str(__pydantic_self__.__class__), message=InstanceIRIInitialisationError.clz_missing)
            elif _namespace_for_init is None:
                raise InstanceIRIInitialisationError(value=_clz, message=InstanceIRIInitialisationError.namespace_for_init_missing)
            else:
                data['instance_iri'] = initialiseInstanceIRI(data['namespace_for_init'], _clz)
        else: # meaning _instance_iri is not None and it's not provided for initialising a new instance to be uploaded to the KG
            if _namespace_for_init is not None:
                raise InstanceIRIInitialisationError(value=_instance_iri, message=InstanceIRIInitialisationError.namespace_for_init_should_not_be_provided)

        super().__init__(**data)

    def _exclude_keys_for_compare_(self, *keys_to_exclude):
        list_keys_to_exclude = list(keys_to_exclude) if not isinstance(keys_to_exclude, list) else keys_to_exclude
        list_keys_to_exclude.append('instance_iri')
        list_keys_to_exclude.append('namespace_for_init')
        return set(tuple(list_keys_to_exclude))

    def __hash__(self):
        return self._make_hash_sha256_(self.dict(exclude=self._exclude_keys_for_compare_()))

    def __eq__(self, other: Any) -> bool:
        return self.__hash__() == other.__hash__()

    def _make_hash_sha256_(self, o):
        # adapted from https://stackoverflow.com/a/42151923
        hasher = hashlib.sha256()
        hasher.update(repr(self._make_hashable_(o)).encode())
        return base64.b64encode(hasher.digest()).decode()

    def _make_hashable_(self, o):
        # adapted from https://stackoverflow.com/a/42151923

        if isinstance(o, (tuple, list)):
            return tuple((self._make_hashable_(e) for e in o))

        if isinstance(o, dict):
            # TODO below is a shortcut for the implementation, the specific _exclude_keys_for_compare_ of nested classes are not called
            # but for OntoCAPE_SinglePhase this is sufficient for the comparison (as 'instance_iri' and 'namespace_for_init' are excluded by default)
            # to do it properly, we might need recursion that calls all _exclude_keys_for_compare_ while iterate the nested classes
            for key in self._exclude_keys_for_compare_():
                if key in o:
                    o.pop(key)
            return tuple(sorted((k,self._make_hashable_(v)) for k,v in o.items()))

        if isinstance(o, (set, frozenset)):
            return tuple(sorted(self._make_hashable_(e) for e in o))

        return o

class OM_Measure(BaseOntology):
    clz: str = OM_MEASURE
    # instead of the actual class, str is used to host the concept IRI of om:Unit for simplicity
    hasUnit: str
    hasNumericalValue: float

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # IRI-ise the IRI of OM:Measure instance to be used by rdflib package
        measure_iri = URIRef(self.instance_iri)

        # Add below triples following units of measure practices:
        # <measureIRI> <rdf:type> <om:Measure> .
        # <measureIRI> <om:hasUnit> <unit> .
        # <measureIRI> <om:hasNumericalValue> <val> .
        g.add((measure_iri, RDF.type, URIRef(OM_MEASURE)))
        g.add((measure_iri, URIRef(OM_HASUNIT), URIRef(self.hasUnit)))
        g.add((measure_iri, URIRef(OM_HASNUMERICALVALUE), Literal(self.hasNumericalValue)))

        return g

class OM_Quantity(BaseOntology):
    clz: str = OM_QUANTITY
    hasValue: OM_Measure

class OM_Volume(OM_Quantity):
    clz: str = OM_VOLUME

class OM_Diameter(OM_Quantity):
    clz: str = OM_DIAMETER

class OM_Length(OM_Quantity):
    clz: str = OM_LENGTH

class OM_CelsiusTemperature(OM_Quantity):
    clz: str = OM_CELSIUSTEMPERATURE

class OM_Duration(OM_Quantity):
    clz: str = OM_DURATION

class OM_QuantityOfDimensionOne(OM_Quantity):
    clz: str = OM_QUANTITYOFDIMENSIONONE

class OM_Mass(OM_Quantity):
    clz: str = OM_MASS

class OM_Density(OM_Quantity):
    clz: str = OM_DENSITY

class OM_SpecificAmountOfMoney(OM_Quantity):
    clz: str = OM_SPECIFICAMOUNTOFMONEY

class OM_AmountOfSubstance(OM_Quantity):
    clz: str = OM_AMOUNTOFSUBSTANCE
