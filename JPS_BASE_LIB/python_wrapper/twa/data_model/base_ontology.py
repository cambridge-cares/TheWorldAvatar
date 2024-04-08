from __future__ import annotations

from typing import Any, Dict, List, Set, Tuple, Union, TypeVar, ClassVar, Type
from typing_extensions import Annotated
from annotated_types import Len

from pydantic import BaseModel, Field, PrivateAttr, model_validator
import rdflib
from rdflib import Graph, URIRef, Literal, BNode
from rdflib.namespace import RDF, RDFS, OWL, XSD, DC

from datetime import datetime

import hashlib
import base64
import copy
import time

from twa.data_model.utils import construct_namespace_iri, construct_rdf_type, init_instance_iri
from twa.data_model.iris import TWA_BASE_URL
from twa.kg_operations import PySparqlClient


T = TypeVar('T')
""" A type variable to represent any type in Python. This is used as placeholder for any concept in the ontologies. """

class KnowledgeGraph(BaseModel):
    """
    This class is used to represent a knowledge graph consists of Pydantic objects in the Python memory.

    Attributes:
        ontology_lookup: A class variable to store the lookup dictionary of ontologies
        class_lookup: A class variable to store the lookup dictionary of classes
        property_lookup: A class variable to store the lookup dictionary of properties
    """
    # NOTE the ClassVar is initialised as None and assigned as empty dict when it is first used
    # this is to avoid the problem of mutable default arguments which is then shared across all subclasses
    ontology_lookup: ClassVar[Dict[str, BaseOntology]] = None
    class_lookup: ClassVar[Dict[str, BaseClass]] = None
    property_lookup: ClassVar[Dict[str, BaseProperty]] = None

    @classmethod
    def construct_object_lookup(cls) -> Dict[str, BaseClass]:
        """
        This method is used to retrieve all BaseClass (pydantic) objects created in Python memory.

        Returns:
            A dictionary of BaseClass (pydantic) objects with their IRIs as keys
        """
        if cls.class_lookup is None:
            return {}
        return {i: o for clz in cls.class_lookup.values() if bool(clz.object_lookup) for i, o in clz.object_lookup.items()}

    @classmethod
    def get_object_from_lookup(cls, iri: str) -> Union[BaseClass, None]:
        """
        This method is used to retrieve an object from Python memory given its IRI.

        Args:
            iri (str): IRI of the object to be retrieved

        Returns:
            The pydantic object of the given IRI if exist, otherwise return None.
        """
        return cls.construct_object_lookup().get(iri, None)

    @classmethod
    def clear_object_lookup(cls):
        """ This method is used to clear the object lookup dictionary in Python memory. """
        for cls in cls.class_lookup.values():
            cls.clear_object_lookup()

    @classmethod
    def register_ontology(cls, ontology: BaseOntology):
        """
        This method registers an ontology to the knowledge graph in Python memory.

        Args:
            ontology (BaseOntology): The ontology object to be registered
        """
        if cls.ontology_lookup is None:
            cls.ontology_lookup = {}
        cls.ontology_lookup[ontology.get_namespace_iri()] = ontology

    @classmethod
    def register_class(cls, ontolgy_class: BaseClass):
        """
        This method registers a BaseClass (the Pydantic class itself) to the knowledge graph in Python memory.

        Args:
            ontolgy_class (BaseClass): The class to be registered
        """
        if cls.class_lookup is None:
            cls.class_lookup = {}
        cls.class_lookup[ontolgy_class.get_rdf_type()] = ontolgy_class

    @classmethod
    def register_property(cls, prop: BaseProperty):
        """
        This method registers a BaseProperty (the Pydantic class itself) to the knowledge graph in Python memory.

        Args:
            prop (BaseProperty): The property to be registered
        """
        if cls.property_lookup is None:
            cls.property_lookup = {}
        cls.property_lookup[prop.get_predicate_iri()] = prop


# TODO optimise the way of spedifying range of object/data properties
def as_range_of_object_property(t: T, min_cardinality: int = 0, max_cardinality: int = None) -> Set[Union[T, str]]:
    """
    This function is used to specify the range of an object property in the ontology.

    Args:
        t (T): The type of the object property
        min_cardinality (int): The minimum cardinality of the object property
        max_cardinality (int): The maximum cardinality of the object property

    Returns:
        The object property with the specified range in the form of a set, with `str` as an alternative type
    """
    if min_cardinality < 0 or max_cardinality is not None and max_cardinality < 0:
        raise ValueError('min_cardinality and max_cardinality must be greater than or equal to 0')
    return Annotated[Set[Union[t, str]], Len(min_cardinality, max_cardinality)]


def as_range_of_data_property(t: T, min_cardinality: int = 0, max_cardinality: int = None) -> Set[T]:
    """
    This function is used to specify the range of a data property in the ontology.

    Args:
        t (T): The type of the data property
        min_cardinality (int): The minimum cardinality of the data property
        max_cardinality (int): The maximum cardinality of the data property

    Returns:
        The data property with the specified range in the form of a set
    """
    if min_cardinality < 0 or max_cardinality is not None and max_cardinality < 0:
        raise ValueError('min_cardinality and max_cardinality must be greater than or equal to 0')
    return Annotated[Set[t], Len(min_cardinality, max_cardinality)]


_list = copy.deepcopy(rdflib.term._GenericPythonToXSDRules)
for pType, (castFunc, dType) in _list:
    if pType == str:
        _list.remove((pType, (castFunc, dType)))
        _list.append((pType, (castFunc, XSD.string.toPython())))


def _castPythonToXSD(python_clz):
    for pType, (castFunc, dType) in _list:
        if python_clz == pType:
            return dType


class BaseOntology(BaseModel):
    """
    This class is used to represent an ontology which consists of a list of BaseClass and ObjectProperty/DataProperty.

    Attributes:
        base_url: The base URL to be used to construct the namespace IRI, the default value is 'https://www.theworldavatar.com/kg/'
        namespace: The namespace of the ontology, e.g. 'ontolab'
        class_lookup: A dictionary of BaseClass classes with their rdf:type as keys
        object_property_lookup: A dictionary of ObjectProperty classes with their predicate IRI as keys
        data_property_lookup: A dictionary of DataProperty classes with their predicate IRI as keys
        rdfs_comment: The comment of the ontology
        owl_versionInfo: The version of the ontology
    """
    base_url: ClassVar[str] = TWA_BASE_URL
    namespace: ClassVar[str] = None
    class_lookup: ClassVar[Dict[str, BaseClass]] = None
    object_property_lookup: ClassVar[Dict[str, ObjectProperty]] = None
    data_property_lookup: ClassVar[Dict[str, DataProperty]] = None
    rdfs_comment: ClassVar[str] = None
    owl_versionInfo: ClassVar[str] = None

    @classmethod
    def get_namespace_iri(cls) -> str:
        """ This method is used to retrieve the namespace IRI of the ontology. """
        return construct_namespace_iri(cls.base_url, cls.namespace)

    @classmethod
    def register_class(cls, ontolgy_class: BaseClass):
        """
        This method registers a BaseClass (the Pydantic class itself) to the BaseOntology class.
        It also registers the BaseClass to the KnowledgeGraph class.

        Args:
            ontolgy_class (BaseClass): The BaseClass class to be registered
        """
        if cls.class_lookup is None:
            cls.class_lookup = {}
        cls.class_lookup[ontolgy_class.get_rdf_type()] = ontolgy_class
        KnowledgeGraph.register_class(ontolgy_class)

    @classmethod
    def register_object_property(cls, prop: ObjectProperty):
        """
        This method registers an ObjectProperty (the Pydantic class itself) to the BaseOntology class.
        It also registers the ObjectProperty to the KnowledgeGraph class.

        Args:
            prop (ObjectProperty): The ObjectProperty class to be registered
        """
        if cls.object_property_lookup is None:
            cls.object_property_lookup = {}
        cls.object_property_lookup[prop.get_predicate_iri()] = prop
        KnowledgeGraph.register_property(prop)

    @classmethod
    def register_data_property(cls, prop: DataProperty):
        """
        This method registers a DataProperty (the Pydantic class itself) to the BaseOntology class.
        It also registers the DataProperty to the KnowledgeGraph class.

        Args:
            prop (DataProperty): The DataProperty class to be registered
        """
        if cls.data_property_lookup is None:
            cls.data_property_lookup = {}
        cls.data_property_lookup[prop.get_predicate_iri()] = prop
        KnowledgeGraph.register_property(prop)

    @classmethod
    def export_to_owl(cls, file_path: str, format: str = 'ttl'):
        # TODO: offer the option to export ontology to a triplestore directly
        """
        This method is used to export the ontology to an ontology file.
        It operates at the TBox level, i.e. it only exports the classes and properties of the ontology.

        Args:
            file_path (str): The path of the ontology file to be exported to
            format (str): The format of the ontology file, the default value is 'ttl'
        """
        g = Graph()
        # metadata
        g.add((URIRef(cls.get_namespace_iri()), RDF.type, OWL.Ontology))
        g.add((URIRef(cls.get_namespace_iri()), DC.date, Literal(datetime.now().isoformat())))
        if bool(cls.rdfs_comment):
            g.add((URIRef(cls.get_namespace_iri()), RDFS.comment, Literal(cls.rdfs_comment)))
        if bool(cls.owl_versionInfo):
            g.add((URIRef(cls.get_namespace_iri()), OWL.versionInfo, Literal(cls.owl_versionInfo)))
        # handle all classes
        if bool(cls.class_lookup):
            for clz in cls.class_lookup.values():
                g = clz.export_to_owl(g)
        # handle all object properties
        if bool(cls.object_property_lookup):
            for prop in cls.object_property_lookup.values():
                g = prop.export_to_owl(g)
        # handle all data properties
        if bool(cls.data_property_lookup):
            for prop in cls.data_property_lookup.values():
                g = prop.export_to_owl(g)

        # serialize
        g.serialize(destination=file_path, format=format)


class BaseProperty(BaseModel, validate_assignment=True):
    # NOTE validate_assignment=True is to make sure the validation is triggered when range is updated
    """
    Base class that is inherited by ObjectProperty and DataProperty.

    Attributes:
        is_defined_by_ontology: The ontology that defines the property
        predicate_iri: The predicate IRI of the property
        domain: The domain of the property
        range: The range of the property
    """

    is_defined_by_ontology: ClassVar[BaseOntology] = None
    predicate_iri: str = Field(default=None)
    domain: ClassVar[Set] = None
    # setting default_factory to set is safe here, i.e. it won't be shared between instances
    # see https://docs.pydantic.dev/latest/concepts/models/#fields-with-non-hashable-default-values
    range: Set = Field(default_factory=set)

    # TODO [future] vanilla set operations don't trigger the validation as of pydantic 2.6.1
    # it also seems this will not be supported in the near future
    # see https://github.com/pydantic/pydantic/issues/496
    # for a workaround, see https://github.com/pydantic/pydantic/issues/8575
    # and https://gist.github.com/geospackle/8f317fc19469b1e216edee3cc0f1c898

    def __init__(self, **data) -> None:
        """
        The constructor of the BaseProperty class.
        It parses the range attribute to make sure it's always a set.
        """
        # below code is to make sure range is always a set even if it's a single value
        if 'range' in data:
            if not isinstance(data['range'], set):
                if not isinstance(data['range'], list):
                    data['range'] = [data['range']]
                data['range'] = set(data['range'])
        else:
            data['range'] = set()
        super().__init__(**data)

    def __hash__(self) -> int:
        return hash(tuple([self.predicate_iri] + sorted([o.__hash__() for o in self.range])))

    @model_validator(mode='after')
    def set_predicate_iri(self):
        # TODO make predicate_iri as a @property?
        if not bool(self.predicate_iri):
            self.predicate_iri = self.__class__.get_predicate_iri()
        return self

    @classmethod
    def get_predicate_iri(cls) -> str:
        """ Get the predicate IRI of the property. """
        return construct_rdf_type(
            cls.is_defined_by_ontology.get_namespace_iri(), cls.__name__)

    @classmethod
    def add_to_domain(cls, domain: BaseClass):
        """
        Add an IRI to the set of property's domain.

        Args:
            domain (BaseClass): The domain class to be added
        """
        if cls.domain is None:
            cls.domain = set()
        cls.domain.add(domain.get_rdf_type())

    @classmethod
    def is_inherited(cls, prop: Any) -> bool:
        """
        This method is used to check whether a property is a subclass of the BaseProperty class.
        > Note this method is used to replace issubclass() as pydantic has its own special logic
        most likely relates to how the abstract class is handled, e.g. issubclass(list[str], BaseProperty)
        throws `TypeError: issubclass() arg 1 must be a class`

        > For more details, see [this discussion](https://github.com/pydantic/pydantic/discussions/5970)

        Args:
            prop (Any): The property to be checked

        Returns:
            bool: Whether the property is a subclass
        """
        try:
            return issubclass(prop, cls)
        except TypeError:
            return False

    def collect_range_diff_to_graph(
        self,
        subject: str,
        cache: BaseProperty,
        g_to_remove: Graph,
        g_to_add: Graph,
        recursive_depth: int = 0
    ):
        """
        This is an abstract method that should be implemented by the subclasses.
        It is used to collect the difference between the range of the property and the cache.

        Args:
            subject (str): The subject of the property
            cache (BaseProperty): The cache of the property to compare with
            g_to_remove (Graph): The rdflib.Graph object to which the triples to be removed will be added
            g_to_add (Graph): The rdflib.Graph object to which the triples to be added will be added
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion

        Raises:
            NotImplementedError: This is an abstract method.
        """
        raise NotImplementedError("This is an abstract method.")

    @classmethod
    def export_to_owl(cls, g: Graph, is_object_property: bool = True) -> Graph:
        """
        This method is used to export the triples of the property to an OWL file.
        It operates at the TBox level.

        Args:
            g (Graph): The rdflib.Graph object to which the property will be added
            is_object_property (bool): Whether the property is an object property or a data property

        Returns:
            Graph: The rdflib.Graph object with the added triples
        """
        property_iri = cls.get_predicate_iri()
        g.add((URIRef(property_iri), RDFS.isDefinedBy, URIRef(cls.is_defined_by_ontology.get_namespace_iri())))
        # add rdf:type and super properties
        if is_object_property:
            g.add((URIRef(property_iri), RDF.type, OWL.ObjectProperty))
            idx = cls.__mro__.index(ObjectProperty)
        else:
            g.add((URIRef(property_iri), RDF.type, OWL.DatatypeProperty))
            idx = cls.__mro__.index(DataProperty)
        for i in range(1, idx):
            g.add((URIRef(property_iri), RDFS.subPropertyOf, URIRef(cls.__mro__[i].get_predicate_iri())))
        # add domain
        if len(cls.domain) > 1:
            # union of class as domain
            bn = BNode()
            bn_union = BNode()
            g.add((bn, RDF.type, OWL.Class))
            g.add((bn, OWL.unionOf, bn_union))
            bn_union_lst = [bn_union]
            for d in cls.domain:
                g.add((bn_union_lst[-1], RDF.first, URIRef(d)))
                if len(bn_union_lst) < len(cls.domain):
                    bn_union_new = BNode()
                    g.add((bn_union_lst[-1], RDF.rest, bn_union_new))
                    bn_union_lst.append(bn_union_new)
                else:
                    g.add((bn_union_lst[-1], RDF.rest, RDF.nil))
            g.add((URIRef(property_iri), RDFS.domain, bn))
        else:
            # single class as domain
            for d in cls.domain:
                g.add((URIRef(property_iri), RDFS.domain, URIRef(d)))
        # add range
        g.add((URIRef(property_iri), RDFS.range, URIRef(cls.reveal_property_range_iri())))
        return g

    @classmethod
    def reveal_possible_property_range(cls) -> Set[T]:
        """
        This is an abstract method that should be implemented by the subclasses.
        It should unpack the range of the property from as_range_of_object_property or as_range_of_data_property.

        Raises:
            NotImplementedError: This is an abstract method.

        Returns:
            Set[T]: The set of possible range of the property
        """
        raise NotImplementedError("This is an abstract method.")

    @classmethod
    def reveal_property_range_iri(cls) -> str:
        """
        This is an abstract method that should be implemented by the subclasses.
        It should return the IRI of the range of the property.

        Raises:
            NotImplementedError: This is an abstract method.

        Returns:
            str: The IRI of the range of the property
        """
        raise NotImplementedError("This is an abstract method.")

    @classmethod
    def retrieve_cardinality(cls) -> Tuple[int, int]:
        """
        This method is used to retrieve the cardinality of the property.

        Returns:
            Tuple[int, int]: The minimum and maximum cardinality of the property
        """
        cardinality = cls.model_fields['range'].metadata[0]
        return cardinality.min_length, cardinality.max_length

    def create_cache(self) -> BaseProperty:
        """
        This is an abstract method that should be implemented by the subclasses.
        It is used to create a cache for the property.

        Raises:
            NotImplementedError: This is an abstract method.
        """
        return NotImplementedError("This is an abstract method.")

    def _exclude_keys_for_compare_(self, *keys_to_exclude):
        list_keys_to_exclude = list(keys_to_exclude) if not isinstance(
            keys_to_exclude, list) else keys_to_exclude
        list_keys_to_exclude.append('instance_iri')
        list_keys_to_exclude.append('rdfs_comment')
        return set(tuple(list_keys_to_exclude))


class BaseClass(BaseModel, validate_assignment=True):
    """
    Base class for all the Python classes that are used to define the classes in ontology.

    Attributes:
        is_defined_by_ontology (Ontology): The ontology that defines the class
        object_lookup (Dict[str, BaseClass]): A dictionary that maps the IRI of the object to the object
        rdfs_comment (str): The comment of the instance
        instance_iri (str): The IRI of the instance

    Example:
    class MyClass(BaseOntology):
        myObjectProperty: MyObjectProperty
        myDataProperty: MyDataProperty
    """

    # NOTE validate_assignment=True is to make sure the validation is triggered when range is updated

    # The initialisation and validator sequence:
    # (I) start to run BaseClass.__init__(__pydantic_self__, **data) with **data as the raw input arguments;
    # (II) run until super().__init__(**data), note data is updated within BaseClass before sending to super().init(**data);
    # (III) now within BaseModel __init__:
    #     (i) run root_validator (for those pre=True), in order of how the root_validators are listed in codes;
    #     (ii) in order of how the fields are listed in codes:
    #         (1) run validator (for those pre=True) in order of how the validators (for the same field) are listed in codes;
    #         (2) run validator (for those pre=False) in order of how the validators (for the same field) are listed in codes;
    #     (iii) (if we are instantiating a child class of BaseClass) load default values in the child class (if they are provided)
    #             and run root_validator (for those pre=False) in order of how the root_validators are listed in codes,
    #             e.g. clz='clz provided in the child class' will be added to 'values' of the input argument of root_validator;
    # (IV) end BaseModel __init__;
    # (V) end BaseClass __init__

    is_defined_by_ontology: ClassVar[BaseOntology] = None
    object_lookup: ClassVar[Dict[str, BaseClass]] = None
    rdfs_comment: str = Field(default=None)
    rdf_type: str = Field(default=None)
    instance_iri: str = Field(default=None)
    _timestamp_of_latest_cache: float = PrivateAttr(default_factory=time.time)
    # format of the cache for all properties: {property_name: property_object}
    _latest_cache: Dict[str, Any] = PrivateAttr(default_factory=dict)
    _exist_in_kg: bool = PrivateAttr(default=False)

    @classmethod
    def __pydantic_init_subclass__(cls, **kwargs):
        # set the domain of all object/data properties
        for f, field_info in cls.model_fields.items():
            if BaseProperty.is_inherited(field_info.annotation):
                field_info.annotation.add_to_domain(cls)

        # register the class to the ontology
        cls.is_defined_by_ontology.register_class(cls)

    def __init__(self, **data) -> None:
        """
        The constructor of the BaseClass.
        It processes the range of the properties so that allows to simplify the input for the user.
        i.e. the user can directly assign the object/data property with a list of objects as its range.
        e.g. object = BaseClass(myObjectProperty=[obj1, obj2, obj3])
        """
        for f, field_info in self.__class__.model_fields.items():
            if BaseProperty.is_inherited(field_info.annotation):
                if f in data:
                    possible_type = field_info.annotation.reveal_possible_property_range()
                    if isinstance(data[f], list):
                        if all(isinstance(i, possible_type) for i in data[f]):
                            data[f] = field_info.annotation(range=set(data[f]))
                    elif isinstance(data[f], set):
                        if all(isinstance(i, possible_type) for i in data[f]):
                            data[f] = field_info.annotation(range=data[f])
                    elif isinstance(data[f], possible_type):
                        data[f] = field_info.annotation(range=data[f])
                    # for all the other cases, we will let the pydantic to validate the input

        super().__init__(**data)

    def model_post_init(self, __context: Any) -> None:
        """
        The post init process of the BaseClass.
        It sets the rdf_type and instance_iri if they are not set.
        It also registers the object to the lookup dictionary of the class.

        Args:
            __context (Any): Any other context that is needed for the post init process

        Returns:
            None: It calls the super().model_post_init(__context) to finish the post init process
        """
        if not bool(self.rdf_type):
            self.rdf_type = self.__class__.get_rdf_type()
        if not bool(self.instance_iri):
            self.instance_iri = init_instance_iri(
                self.rdf_type, self.__class__.__name__)
        # set new instance to the global look up table, so that we can avoid creating the same instance multiple times
        self._register_object()
        return super().model_post_init(__context)

    def _register_object(self):
        """
        This function registers the object to the lookup dictionary of the class.
        It should not be called by the user.

        Raises:
            ValueError: The object with the same IRI has already been registered
        """
        if self.__class__.object_lookup is None:
            self.__class__.object_lookup = {}
        if self.instance_iri in self.__class__.object_lookup:
            raise ValueError(
                f"An object with the same IRI {self.instance_iri} has already been registered.")
        self.__class__.object_lookup[self.instance_iri] = self

    @classmethod
    def clear_object_lookup(cls):
        """
        This function clears the lookup dictionary of the class.
        """
        if cls.object_lookup is not None:
            iris = list(cls.object_lookup.keys())
            for i in iris:
                del cls.object_lookup[i]

    @classmethod
    def get_rdf_type(cls) -> str:
        """
        This function returns the rdf_type of the class.

        Returns:
            str: The rdf_type of the class (rdf:type in owl)
        """
        return construct_rdf_type(cls.is_defined_by_ontology.get_namespace_iri(), cls.__name__)

    @classmethod
    def pull_from_kg(cls, iris: List[str], sparql_client: PySparqlClient, recursive_depth: int = 0) -> List[BaseClass]:
        """
        This function pulls the objects from the KG based on the given IRIs.

        Args:
            iris (List[str]): The list of IRIs of the objects that one wants to pull from the KG
            sparql_client (PySparqlClient): The SPARQL client that is used to pull the data from the KG
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion

        Raises:
            ValueError: The rdf:type of the IRI provided does not match the calling class

        Returns:
            List[BaseClass]: A list of objects that are pulled from the KG
        """
        # behaviour of recursive_depth: 0 means no recursion, -1 means infinite recursion, n means n-level recursion
        flag_pull = abs(recursive_depth) > 0
        recursive_depth = max(recursive_depth - 1, 0) if recursive_depth > -1 else max(recursive_depth - 1, -1)
        # TODO what do we do with undefined properties in python class? - write a warning message or we can add them to extra_fields https://docs.pydantic.dev/latest/concepts/models/#extra-fields
        if isinstance(iris, str):
            iris = [iris]
        iris = set(iris)
        # return format: {iri: {predicate: {object}}}
        node_dct = sparql_client.get_outgoing_and_attributes(iris)
        instance_lst = []
        # TODO optimise the time complexity of the following code when the number of instances is large
        ops = cls.get_object_properties()
        dps = cls.get_data_properties()

        for iri, props in node_dct.items():
            if cls.get_rdf_type() not in props[RDF.type.toPython()]:
                raise ValueError(f"The instance {iri} is of type {props[RDF.type.toPython()]}, therefore it cannot be instantiated as {cls.get_rdf_type()} ({cls.__name__}).")
            inst = KnowledgeGraph.get_object_from_lookup(iri)
            # handle object properties (where the recursion happens)
            # TODO need to consider what to do when two instances pointing to each other
            object_properties_dict = {
                op_dct['field']: op_dct['type'](
                    range=set() if op_iri not in props else op_dct['type'].reveal_object_property_range(
                        ).pull_from_kg(props[op_iri], sparql_client, recursive_depth) if flag_pull else props[op_iri]
                ) for op_iri, op_dct in ops.items()
            }
            # here we handle data properties
            data_properties_dict = {
                dp_dct['field']: dp_dct['type'](
                    range=props[dp_iri] if dp_iri in props else set()
                ) for dp_iri, dp_dct in dps.items()
            }
            if inst is not None:
                # consider those objects that are connected in the KG and are connected in python
                # TODO below query can be combined with those connected in the KG to save amount of queries
                for op_iri, op_dct in ops.items():
                    if flag_pull:
                        op_dct['type'].reveal_object_property_range().pull_from_kg(
                            set(inst.get_object_property_range_iris(op_dct['field'])) - set(props.get(op_iri, [])),
                            sparql_client, recursive_depth)
                inst._latest_cache = {k: v.create_cache() for k, v in {**object_properties_dict, **data_properties_dict}.items()}
                inst._timestamp_of_latest_cache = time.time()
            else:
                inst = cls(
                    instance_iri=iri,
                    **object_properties_dict,
                    **data_properties_dict,
                )
                inst.create_cache()
                inst._timestamp_of_latest_cache = time.time()

            inst._exist_in_kg = True
            # update cache here
            instance_lst.append(inst)
        return instance_lst

    @classmethod
    def pull_all_instances_from_kg(cls, sparql_client: PySparqlClient, recursive_depth: int = 0) -> Set[BaseClass]:
        """
        This function pulls all instances of the calling class from the knowledge graph (triplestore).
        It calls the pull_from_kg function with the IRIs of all instances of the calling class.

        Args:
            sparql_client (PySparqlClient): The SPARQL client that is used to pull the data from the KG
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion

        Returns:
            Set[BaseClass]: A set of objects that are pulled from the KG
        """
        iris = sparql_client.get_all_instances_of_class(cls.get_rdf_type())
        return cls.pull_from_kg(iris, sparql_client, recursive_depth)

    @classmethod
    def get_object_and_data_properties(cls) -> Dict[str, Dict[str, Union[str, Type[BaseProperty]]]]:
        """
        This function returns the object and data properties of the calling class.
        This method calls the get_object_properties and get_data_properties functions and returns the combined dictionary.

        Returns:
            Dict[str, Dict[str, Union[str, Type[BaseProperty]]]]: A dictionary containing the object and data properties of the calling class
        """
        return {**cls.get_object_properties(), **cls.get_data_properties()}

    @classmethod
    def get_object_properties(cls) -> Dict[str, Dict[str, Union[str, Type[ObjectProperty]]]]:
        """
        This function returns the object properties of the calling class.

        Returns:
            Dict[str, Union[str, Type[ObjectProperty]]]]: A dictionary containing the object properties of the calling class
                in the format of {predicate_iri: {'field': field_name, 'type': field_clz}}
                e.g. {'https://twa.com/myObjectProperty': {'field': 'myObjectProperty', 'type': MyObjectProperty}}
        """
        return {
            field_info.annotation.get_predicate_iri(): {
                'field': f, 'type': field_info.annotation
            } for f, field_info in cls.model_fields.items() if ObjectProperty.is_inherited(field_info.annotation)
        }

    @classmethod
    def get_data_properties(cls) -> Dict[str, Dict[str, Union[str, Type[DataProperty]]]]:
        """
        This function returns the data properties of the calling class.

        Returns:
            Dict[str, Dict[str, Union[str, Type[DataProperty]]]]: A dictionary containing the data properties of the calling class
                in the format of {predicate_iri: {'field': field_name, 'type': field_clz}}
                e.g. {'https://twa.com/myDataProperty': {'field': 'myDataProperty', 'type': MyDataProperty}}
        """
        return {
            field_info.annotation.get_predicate_iri(): {
                'field': f, 'type': field_info.annotation
            } for f, field_info in cls.model_fields.items() if DataProperty.is_inherited(field_info.annotation)
        }

    @classmethod
    def export_to_owl(cls, g: Graph) -> Graph:
        """
        This function exports the triples of the calling class to an RDF graph in OWL format.
        It operates at the TBox level.

        Args:
            g (Graph): The rdflib.Graph object to which the property will be added

        Returns:
            Graph: The rdflib.Graph object with the added triples
        """
        cls_iri = cls.get_rdf_type()
        g.add((URIRef(cls_iri), RDF.type, OWL.Class))
        g.add((URIRef(cls_iri), RDFS.isDefinedBy, URIRef(cls.is_defined_by_ontology.get_namespace_iri())))
        # add super classes
        idx = cls.__mro__.index(BaseClass)
        for i in range(1, idx):
            g.add((URIRef(cls_iri), RDFS.subClassOf, URIRef(cls.__mro__[i].get_rdf_type())))
        # add cardinality for object and data properties
        for prop_iri, prop_dct in cls.get_object_and_data_properties().items():
            prop = prop_dct['type']
            min_car, max_car = prop.retrieve_cardinality()
            if any([bool(min_car), bool(max_car)]):
                restriction = BNode()
                if bool(min_car):
                    if min_car == max_car:
                        g.add((restriction, OWL.qualifiedCardinality, Literal(min_car, datatype=XSD.nonNegativeInteger)))
                    else:
                        g.add((restriction, OWL.minQualifiedCardinality, Literal(min_car, datatype=XSD.nonNegativeInteger)))
                if bool(max_car):
                    g.add((restriction, OWL.maxQualifiedCardinality, Literal(max_car, datatype=XSD.nonNegativeInteger)))
                g.add((restriction, RDF.type, OWL.Restriction))
                g.add((restriction, OWL.onClass, URIRef(prop.reveal_property_range_iri())))
                g.add((restriction, OWL.onProperty, URIRef(prop_iri)))
                g.add((URIRef(cls_iri), RDFS.subClassOf, restriction))
        return g

    def create_cache(self):
        """ This function creates a cache of the instance of the calling class. """
        self._latest_cache = {f: getattr(self, f).create_cache()
                              if BaseProperty.is_inherited(field_info.annotation) else getattr(self, f)
                              for f, field_info in self.model_fields.items()}

    def get_object_property_range_iris(self, field_name: str) -> List[str]:
        """
        This function returns the IRIs of the range of the object property.

        Args:
            field_name (str): The name of the field, e.g. 'myObjectProperty'

        Returns:
            List[str]: A list of IRIs of the range of the object property
        """
        return [o.instance_iri if isinstance(o, BaseClass) else o for o in getattr(self, field_name).range]

    def delete_in_kg(self, sparql_client: PySparqlClient):
        # TODO implement this method
        raise NotImplementedError

    def push_to_kg(self, sparql_client: PySparqlClient, recursive_depth: int = 0) -> Tuple[Graph, Graph]:
        """
        This function pushes the triples of the calling object to the knowledge graph (triplestore).

        Args:
            sparql_client (PySparqlClient): The SPARQL client object to be used to push the triples
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion

        Returns:
            Tuple[Graph, Graph]: A tuple of two rdflib.Graph objects containing the triples to be removed and added
        """
        # type of changes: remove old triples, add new triples
        g_to_remove = Graph()
        g_to_add = Graph()
        g_to_remove, g_to_add = self.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)
        # TODO [future] what happens when KG changed during processing in the python side? race conditions...
        sparql_client.delete_and_insert_graphs(g_to_remove, g_to_add)
        return g_to_remove, g_to_add

    def collect_diff_to_graph(self, g_to_remove: Graph, g_to_add: Graph, recursive_depth: int = 0) -> Tuple[Graph, Graph]:
        """
        This function collects the differences between the latest cache and the current instance of the calling object.

        Args:
            g_to_remove (Graph): The rdflib.Graph object to which the triples to be removed will be added
            g_to_add (Graph): The rdflib.Graph object to which the triples to be added will be added
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion

        Returns:
            Tuple[Graph, Graph]: A tuple of two rdflib.Graph objects containing the triples to be removed and added
        """
        for f, field_info in self.model_fields.items():
            if BaseProperty.is_inherited(field_info.annotation):
                p_cache = self._latest_cache.get(f, field_info.annotation())
                p_now = getattr(self, f)
                p_now.collect_range_diff_to_graph(self.instance_iri, p_cache, g_to_remove, g_to_add, recursive_depth)
            elif f == 'rdf_type' and not self._exist_in_kg and not bool(self._latest_cache.get(f)):
                g_to_add.add((URIRef(self.instance_iri), RDF.type, URIRef(self.rdf_type)))
                # assume that the instance is in KG once the triples are added
                # TODO [future] or need to a better way to represent this?
                self._exist_in_kg = True
            elif f == 'rdfs_comment':
                if self._latest_cache.get(f) != self.rdfs_comment:
                    if self._latest_cache.get(f) is not None:
                        g_to_remove.add((URIRef(self.instance_iri), RDFS.comment, Literal(self._latest_cache.get(f))))
                    if self.rdfs_comment is not None:
                        g_to_add.add((URIRef(self.instance_iri), RDFS.comment, Literal(self.rdfs_comment)))
        return g_to_remove, g_to_add

    def _exclude_keys_for_compare_(self, *keys_to_exclude):
        list_keys_to_exclude = list(keys_to_exclude) if not isinstance(
            keys_to_exclude, list) else keys_to_exclude
        list_keys_to_exclude.append('instance_iri')
        list_keys_to_exclude.append('rdfs_comment')
        return set(tuple(list_keys_to_exclude))

    def __eq__(self, other: Any) -> bool:
        return self.__hash__() == other.__hash__()

    def __hash__(self):
        # using instance_iri for hash so that iri and object itself are treated the same in set operations
        return self.instance_iri.__hash__()
        # TODO [future] do we want to provide the method to compare if the content of two instances are the same?
        # a use case would be to compare if the chemicals in the two bottles are the same concentration
        # return self._make_hash_sha256_(self.dict(exclude=self._exclude_keys_for_compare_()))

    def _make_hash_sha256_(self, o):
        # adapted from https://stackoverflow.com/a/42151923
        hasher = hashlib.sha256()
        hasher.update(repr(self._make_hashable_(o)).encode())
        return base64.b64encode(hasher.digest()).decode()

    def _make_hashable_(self, o):
        # adapted from https://stackoverflow.com/a/42151923

        if isinstance(o, (tuple, list)):
            # see https://stackoverflow.com/questions/5884066/hashing-a-dictionary/42151923#comment101432942_42151923
            # NOTE here we sort the list as we assume the order of the range for object/data properties should not matter
            return tuple(sorted((self._make_hashable_(e) for e in o)))

        if isinstance(o, dict):
            # TODO [future] below is a shortcut for the implementation, the specific _exclude_keys_for_compare_ of nested classes are not called
            # but for OntoCAPE_SinglePhase this is sufficient for the comparison (as 'instance_iri' and 'namespace_for_init' are excluded by default)
            # to do it properly, we might need recursion that calls all _exclude_keys_for_compare_ while iterate the nested classes
            for key in self._exclude_keys_for_compare_():
                if key in o:
                    o.pop(key)
            return tuple(sorted((k, self._make_hashable_(v)) for k, v in o.items()))

        if isinstance(o, (set, frozenset)):
            return tuple(sorted(self._make_hashable_(e) for e in o))

        return o


class ObjectProperty(BaseProperty):
    """
    Base class for object properties.
    It inherits the BaseProperty class.
    """

    @classmethod
    def __pydantic_init_subclass__(cls, **kwargs):
        cls.is_defined_by_ontology.register_object_property(cls)

    def _collect_diff(
        self,
        o: Any,
        g_to_remove: Graph,
        g_to_add: Graph,
        flag_collect: bool,
        recursive_depth: int = 0
    ):
        if flag_collect:
            if isinstance(o, BaseClass):
                o_iri = o.instance_iri
                g_to_remove, g_to_add = o.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)
            elif isinstance(o, str):
                o_iri = o
                o_py = KnowledgeGraph.get_object_from_lookup(o)
                # only collect the diff if the object exists in the memory, otherwise it's not necessary
                if o_py is not None:
                    g_to_remove, g_to_add = o_py.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)
            else:
                raise TypeError(f"Type of {o} is not supported for range of {self}.")
        else:
            o_iri = o.instance_iri if isinstance(o, BaseClass) else o
        return g_to_remove, g_to_add, o_iri

    def collect_range_diff_to_graph(
        self,
        subject: str,
        cache: ObjectProperty,
        g_to_remove: Graph,
        g_to_add: Graph,
        recursive_depth: int = 0
    ) -> Tuple[Graph, Graph]:
        """
        This function collects the differences between the latest cache and the current instance of the calling object.

        Args:
            subject (str): The subject of the property when adding/removing triples
            cache (ObjectProperty): The cache of the property to compare with
            g_to_remove (Graph): The rdflib.Graph object to which the triples to be removed will be added
            g_to_add (Graph): The rdflib.Graph object to which the triples to be added will be added
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion

        Returns:
            Tuple[Graph, Graph]: A tuple of two rdflib.Graph objects containing the triples to be removed and added
        """
        # behaviour of recursive_depth: 0 means no recursion, -1 means infinite recursion, n means n-level recursion
        flag_collect = abs(recursive_depth) > 0
        recursive_depth = max(recursive_depth - 1, 0) if recursive_depth > -1 else max(recursive_depth - 1, -1)

        # TODO optimise the below codes
        # compare the range and its cache to find out what to remove and what to add
        diff_to_remove = cache.range - self.range
        diff_to_add = self.range - cache.range

        # iterate the differences and add them to the graph
        for o in diff_to_add:
            g_to_remove, g_to_add, o_iri = self._collect_diff(o, g_to_remove, g_to_add, flag_collect, recursive_depth)
            g_to_add.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o_iri)))

        for o in diff_to_remove:
            g_to_remove, g_to_add, o_iri = self._collect_diff(o, g_to_remove, g_to_add, flag_collect, recursive_depth)
            g_to_remove.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o_iri)))

        # besides the differences between the range and its cache
        # also need to consider the intersection of the range and its cache when recursive
        for o in self.range.intersection(cache.range):
            g_to_remove, g_to_add, o_iri = self._collect_diff(o, g_to_remove, g_to_add, flag_collect, recursive_depth)

        return g_to_remove, g_to_add

    @classmethod
    def export_to_owl(cls, g: Graph) -> Graph:
        """
        This function exports the triples of the object property to an OWL ontology.
        It calls the super class function with the flag 'is_object_property' set to True.

        Args:
            g (Graph): The rdflib.Graph object to which the triples will be added

        Returns:
            Graph: The rdflib.Graph object with the added triples
        """
        return super().export_to_owl(g, True)

    @classmethod
    def reveal_object_property_range(cls) -> T:
        """
        This function reveals the Pydantic class of the range of the object property.

        Returns:
            T: The Pydantic class of the range of the object property
        """
        return cls.model_fields['range'].annotation.__args__[0].__args__[0]

    @classmethod
    def reveal_possible_property_range(cls) -> Set[T]:
        """
        This function reveals the possible range of the object property.

        Returns:
            Set[T]: The set of possible range of the property
        """
        return cls.model_fields['range'].annotation.__args__[0].__args__

    @classmethod
    def reveal_property_range_iri(cls) -> str:
        """
        This function reveals the IRI of the range of the object property.

        Returns:
            str: IRI of the range of the object property
        """
        return cls.reveal_object_property_range().get_rdf_type()

    def create_cache(self) -> ObjectProperty:
        """
        This function creates a cache of the object property.

        Returns:
            ObjectProperty: The cache of the object property
        """
        return self.__class__(range=set([
            o.instance_iri if isinstance(o, BaseClass) else o for o in self.range
        ]))

class DataProperty(BaseProperty):
    """
    Base class for data properties.
    It inherits the BaseProperty class.
    """

    @classmethod
    def __pydantic_init_subclass__(cls, **kwargs):
        cls.is_defined_by_ontology.register_data_property(cls)

    def collect_range_diff_to_graph(
        self,
        subject: str,
        cache: DataProperty,
        g_to_remove: Graph,
        g_to_add: Graph,
        recursive_depth: int = 0,
    ) -> Tuple[Graph, Graph]:
        """
        This function collects the differences between the latest cache and the current instance of the calling object.

        Args:
            subject (str): The subject of the property when adding/removing triples
            cache (DataProperty): The cache of the property to compare with
            g_to_remove (Graph): The rdflib.Graph object to which the triples to be removed will be added
            g_to_add (Graph): The rdflib.Graph object to which the triples will be added
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion
                > this parameter is not used in this function, but it is kept for compatibility with the method in the parent class BaseProperty

        Returns:
            Tuple[Graph, Graph]: A tuple of two rdflib.Graph objects containing the triples to be removed and added
        """
        # compare the range and its cache to find out what to remove and what to add
        diff_to_remove = cache.range - self.range
        for d in diff_to_remove:
            self.add_property_to_graph(subject, d, g_to_remove)

        diff_to_add = self.range - cache.range
        # iterate the differences and add them to the graph
        for d in diff_to_add:
            self.add_property_to_graph(subject, d, g_to_add)

        return g_to_remove, g_to_add

    def add_property_to_graph(self, subject: str, object: Any, g: Graph) -> Graph:
        """
        This function adds a data property to the graph.

        Args:
            subject (str): The subject of the triple
            object (Any): The object of the triple
            g (Graph): The rdflib.Graph object to which the triple will be added

        Raises:
            TypeError: The type of the object is not supported by rdflib as a data property

        Returns:
            Graph: The rdflib.Graph object with the added triple
        """
        try:
            g.add((URIRef(subject), URIRef(self.predicate_iri), Literal(object)))
        except Exception as e:
            raise TypeError(
                f"Type of {object} ({type(object)}) is not supported by rdflib as a data property for {self.predicate_iri}.", e)
        return g

    @classmethod
    def export_to_owl(cls, g: Graph) -> Graph:
        """
        This function exports the triples of the data property to an OWL ontology.
        It calls the super class function with the flag 'is_object_property' set to False.

        Args:
            g (Graph): The rdflib.Graph object to which the triples will be added

        Returns:
            Graph: The rdflib.Graph object with the added triples
        """
        return super().export_to_owl(g, False)

    @classmethod
    def reveal_data_property_range(cls) -> T:
        """
        This function reveals the range of the data property.

        Returns:
            T: The range of the data property
        """
        return cls.model_fields['range'].annotation.__args__[0]

    @classmethod
    def reveal_possible_property_range(cls) -> Set[T]:
        """
        This function reveals the possible range of the object property.

        Returns:
            Set[T]: The set of possible range of the property
        """
        return cls.model_fields['range'].annotation.__args__

    @classmethod
    def reveal_property_range_iri(cls) -> str:
        """
        This function reveals the IRI of the range of the data property.

        Returns:
            str: IRI of the range of the data property
        """
        return _castPythonToXSD(cls.reveal_data_property_range())

    def create_cache(self) -> DataProperty:
        """
        This function creates a cache of the data property.

        Returns:
            DataProperty: The cache of the data property
        """
        return self.__class__(range=set(copy.deepcopy(self.range)))
