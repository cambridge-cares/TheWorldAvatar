from __future__ import annotations

from typing import _UnionGenericAlias
from typing import Any, Dict, List, Set, Tuple, Union, Generic, TypeVar, ClassVar, Type, Optional, ForwardRef
from typing_extensions import Annotated, get_args, get_origin
from annotated_types import Len

from pydantic import BaseModel, Field, PrivateAttr
from pydantic import AfterValidator, GetCoreSchemaHandler, ValidationInfo
from pydantic_core import PydanticUndefined, CoreSchema, core_schema
import rdflib
from rdflib import Graph, URIRef, Literal, BNode
from rdflib.namespace import RDF, RDFS, OWL, XSD, DC

from datetime import datetime
import warnings
import hashlib
import base64
import copy
import time

from twa.data_model.utils import construct_namespace_iri, construct_rdf_type, init_instance_iri
from twa.data_model.iris import TWA_BASE_URL, OWL_BASE_URL
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
    property_lookup: ClassVar[Dict[str, _BaseProperty]] = None
    iri_loading_in_progress: ClassVar[Set[str]] = None

    @classmethod
    def graph(cls) -> Graph:
        g = Graph()
        for iri, o in cls.construct_object_lookup().items():
            g += o.graph()
        return g

    @classmethod
    def all_triples_of_nodes(cls, iris) -> Graph:
        # ensure iris is a list
        if isinstance(iris, str):
            iris = [iris]

        # convert strings to URIRef if necessary
        iris = [URIRef(iri) if isinstance(iri, str) else iri for iri in iris]

        source_g = cls.graph()
        result_g = Graph()

        # add triples to result_graph
        for iri in iris:
            for triple in source_g.triples((iri, None, None)):
                result_g.add(triple)
            for triple in source_g.triples((None, None, iri)):
                result_g.add(triple)
        return result_g

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
    def add_iri_to_loading(cls, iri: str):
        """
        This method temporarily stores the IRI of the object that is loading into Python.
        This is to prevent circular graph patterns causing infinite recursion when pulling from the knowledge graph.

        Args:
            iri (str): The IRI of the object that is been loading into Python memory
        """
        if cls.iri_loading_in_progress is None:
            cls.iri_loading_in_progress = set()
        cls.iri_loading_in_progress.add(iri)

    @classmethod
    def is_iri_been_loading(cls, iri: str) -> bool:
        """
        This method detects whether a given IRI is been loading into Python by other process.

        Args:
            iri (str): The IRI of the object that is of interest

        Returns:
            bool: Boolean value whether the given IRI is been loading
        """
        if cls.iri_loading_in_progress is None:
            cls.iri_loading_in_progress = set()
        return iri in cls.iri_loading_in_progress

    @classmethod
    def remove_iri_from_loading(cls, iri: str):
        """
        This method removes the IRI of the object that is loaded into Python.

        Args:
            iri (str): The IRI of the object that is loaded into Python memory
        """
        if cls.iri_loading_in_progress is None:
            cls.iri_loading_in_progress = set()
        else:
            cls.iri_loading_in_progress.discard(iri)

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
    def register_class(cls, ontology_class: BaseClass):
        """
        This method registers a BaseClass (the Pydantic class itself) to the knowledge graph in Python memory.

        Args:
            ontolgy_class (BaseClass): The class to be registered
        """
        if cls.class_lookup is None:
            cls.class_lookup = {}
        # TODO bring below back
        # if ontology_class.rdf_type in cls.class_lookup:
        #     raise ValueError(f'Class with rdf_type {ontology_class.rdf_type} already exists in the knowledge graph: {cls.class_lookup[ontology_class.rdf_type]}.')
        cls.class_lookup[ontology_class.rdf_type] = ontology_class

    @classmethod
    def register_property(cls, prop: _BaseProperty):
        """
        This method registers a BaseProperty (the Pydantic class itself) to the knowledge graph in Python memory.

        Args:
            prop (BaseProperty): The property to be registered
        """
        if cls.property_lookup is None:
            cls.property_lookup = {}
        # TODO bring below back
        # if prop.predicate_iri in cls.property_lookup:
        #     raise ValueError(f'Property with predicate IRI {prop.predicate_iri} already exists in the knowledge graph: {cls.property_lookup[prop.predicate_iri]}.')
        cls.property_lookup[prop.predicate_iri] = prop

    @classmethod
    def construct_property_domain_range_lookup(cls) -> Dict[str, Dict[str, str]]:
        property_domain_range_lookup = {}
        for onto_cls in cls.class_lookup.values():
            try:
                onto_cls.model_rebuild()
            except Exception as e:
                raise Exception(f'Constructing property_domain_range_lookup failed, class {onto_cls} is not fully built: {onto_cls.model_fields}') from e
            for p_iri, info in onto_cls.get_object_and_data_properties().items():
                if p_iri not in property_domain_range_lookup:
                    property_domain_range_lookup[p_iri] = {'rdfs_domain': set(), 'rdfs_range': set()}
                property_domain_range_lookup[p_iri]['rdfs_domain'].add(onto_cls.rdf_type)
                property_domain_range_lookup[p_iri]['rdfs_range'].add(
                    get_args(info['type'])[0].rdf_type if issubclass(info['type'], ObjectProperty) else _castPythonToXSD(get_args(info['type'])[0])
                )
        return property_domain_range_lookup

# def as_range(t: T, min_cardinality: int = 0, max_cardinality: int = None) -> Set:
#     """
#     This function is used to wrap the data type as the range of an object/data property in the ontology.

#     Args:
#         t (T): The type of the class to be the range
#         min_cardinality (int): The minimum cardinality of the object/data property
#         max_cardinality (int): The maximum cardinality of the object/data property

#     Returns:
#         The object property with the specified range in the form of a set, with `str` as an alternative type
#     """
#     if min_cardinality < 0 or max_cardinality is not None and max_cardinality < 0:
#         raise ValueError('min_cardinality and max_cardinality must be greater than or equal to 0')
#     if issubclass(t, BaseClass):
#         return Annotated[Set[Union[t, str]], Len(min_cardinality, max_cardinality)]
#     else:
#         return Annotated[Set[t], Len(min_cardinality, max_cardinality)]


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
    This class is used to represent an ontology which consists of a list of BaseClass and ObjectProperty/DatatypeProperty.

    Attributes:
        base_url: The base URL to be used to construct the namespace IRI, the default value is 'https://www.theworldavatar.com/kg/'
        namespace: The namespace of the ontology, e.g. 'ontolab'
        class_lookup: A dictionary of BaseClass classes with their rdf:type as keys
        object_property_lookup: A dictionary of ObjectProperty classes with their predicate IRI as keys
        data_property_lookup: A dictionary of DatatypeProperty classes with their predicate IRI as keys
        rdfs_comment: The comment of the ontology
        owl_versionInfo: The version of the ontology
    """
    base_url: ClassVar[str] = TWA_BASE_URL
    namespace: ClassVar[str] = None
    class_lookup: ClassVar[Dict[str, BaseClass]] = None
    object_property_lookup: ClassVar[Dict[str, ObjectProperty]] = None
    data_property_lookup: ClassVar[Dict[str, DatatypeProperty]] = None
    rdfs_comment: ClassVar[str] = None
    owl_versionInfo: ClassVar[str] = None
    # TODO remove below
    # class_lookup_name_as_key: ClassVar[Dict[str, BaseClass]] = None
    # object_property_lookup_name_as_key: ClassVar[Dict[str, ObjectProperty]] = None
    # data_property_lookup_name_as_key: ClassVar[Dict[str, DatatypeProperty]] = None
    # forward_refs_key_predicate: ClassVar[Dict[str, Set[Type[BaseClass]]]] = None
    # forward_refs_key_range: ClassVar[Dict[str, Set[str]]] = None

    @classmethod
    def get_namespace_iri(cls) -> str:
        """
        This method is used to retrieve the namespace IRI of the ontology.
        It is a concatenation of its base URL and the class name in lower case (if class variable `namespace` is not provided).
        """
        # if cls.namespace is None:
        #     cls.namespace = cls.__name__.lower()
        return construct_namespace_iri(cls.base_url, cls.namespace)

    # TODO remove below
    # @classmethod
    # def postpone_rdfs_domain_range(cls, forward_ref_property: str, cls_as_domain: Type[BaseClass]):
    #     """
    #     This method is used to record the object/data properties that are forward referenced,
    #     whose registration as domain is therefore postponed.

    #     Args:
    #         forward_ref_property (str): The name of the forward referenced object/data property
    #         cls_as_domain (Type[BaseClass]): The class to be added as the domain of the object/data property
    #     """
    #     if cls.forward_refs_key_predicate is None:
    #         cls.forward_refs_key_predicate = {}
    #     f = forward_ref_property.strip()
    #     # Handle Optional and other variations
    #     if f.startswith('Optional[') and f.endswith(']'):
    #         f = f[len('Optional['):-1]
    #     cls_as_range = None
    #     if f.endswith(']'):
    #         cls_as_range = f[f.index('[')+1:f.index(']')]
    #         f = f[:f.index('[')]
    #     if f not in cls.forward_refs_key_predicate:
    #         cls.forward_refs_key_predicate[f] = {'rdfs_domain': set(), 'rdfs_range': set()}
    #     cls.forward_refs_key_predicate[f]['rdfs_domain'].add(cls_as_domain)
    #     if cls_as_range is not None:
    #         if cls.forward_refs_key_range is None:
    #             cls.forward_refs_key_range = {}
    #         if cls_as_range not in cls.forward_refs_key_range:
    #             cls.forward_refs_key_range[cls_as_range] = set()
    #         cls.forward_refs_key_range[cls_as_range].add(f)
    #         cls.forward_refs_key_predicate[f]['rdfs_range'].add(cls_as_range)

    # TODO remove below
    # @classmethod
    # def retrieve_postponed_property_given_range(cls, range_name):
    #     if cls.forward_refs_key_range is None:
    #         cls.forward_refs_key_predicate = {}
    #         return set()
    #     return cls.forward_refs_key_range.pop(range_name, set())

    # TODO remove below
    # @classmethod
    # def retrieve_postponed_property_domain(cls, property_name):
    #     """
    #     This method is used to retrieve the classes whose registration as domain of the object/data property
    #     is postponed.

    #     Args:
    #         property_name (str): The name of the forward referenced object/data property
    #     """
    #     if cls.forward_refs_key_predicate is None:
    #         return set()
    #     if property_name in cls.forward_refs_key_predicate:
    #         return cls.forward_refs_key_predicate[property_name].pop('rdfs_domain', set())
    #     return set()

    # TODO remove below
    # @classmethod
    # def retrieve_postponed_property_range(cls, property_name):
    #     if cls.forward_refs_key_predicate is None:
    #         return set()
    #     if property_name in cls.forward_refs_key_predicate:
    #         return cls.forward_refs_key_predicate[property_name].pop('rdfs_range', set())
    #     return set()

    @classmethod
    def register_class(cls, ontolgy_class: BaseClass):
        """
        This method registers a BaseClass (the Pydantic class itself) to the BaseOntology class.
        It also registers the BaseClass to the KnowledgeGraph class.

        Args:
            ontolgy_class (BaseClass): The BaseClass class to be registered
        """
        # register with rdf:type as key
        if cls.class_lookup is None:
            cls.class_lookup = {}
        # TODO bring below back
        # if ontolgy_class.rdf_type in cls.class_lookup:
        #     raise ValueError(f'Class with rdf_type {ontolgy_class.rdf_type} already exists in {cls}: {cls.class_lookup[ontolgy_class.rdf_type]}.')
        cls.class_lookup[ontolgy_class.rdf_type] = ontolgy_class

        # also register with knowledge graph
        KnowledgeGraph.register_class(ontolgy_class)


        # TODO remove below
        # # register with name as key
        # if cls.class_lookup_name_as_key is None:
        #     cls.class_lookup_name_as_key = {}
        # # if ontolgy_class.__name__ in cls.class_lookup_name_as_key:
        # #     raise ValueError(f'Class with name {ontolgy_class.__name__} already exists in {cls}: {cls.class_lookup_name_as_key[ontolgy_class.rdf_type]}.')
        # cls.class_lookup_name_as_key[ontolgy_class.__name__] = ontolgy_class


    @classmethod
    def register_object_property(cls, prop: ObjectProperty):
        """
        This method registers an ObjectProperty (the Pydantic class itself) to the BaseOntology class.
        It also registers the ObjectProperty to the KnowledgeGraph class.

        Args:
            prop (ObjectProperty): The ObjectProperty class to be registered
        """
        # register with iri as key
        if cls.object_property_lookup is None:
            cls.object_property_lookup = {}
        # TODO bring below back
        # if prop.predicate_iri in cls.object_property_lookup:
        #     raise ValueError(f'Object property with predicate IRI {prop.predicate_iri} already exists in {cls}: {cls.object_property_lookup[prop.predicate_iri]}.')
        cls.object_property_lookup[prop.predicate_iri] = prop

        # also register with knowledge graph
        KnowledgeGraph.register_property(prop)


        # TODO remove below
        # # register with name as key
        # if cls.object_property_lookup_name_as_key is None:
        #     cls.object_property_lookup_name_as_key = {}
        # # TODO bring below back
        # # if prop.__name__ in cls.object_property_lookup_name_as_key:
        # #     raise ValueError(f'Object property with name {prop.__name__} already exists in {cls}: {cls.object_property_lookup_name_as_key[prop.predicate_iri]}.')
        # cls.object_property_lookup_name_as_key[prop.__name__] = prop

    @classmethod
    def register_data_property(cls, prop: DatatypeProperty):
        """
        This method registers a DatatypeProperty (the Pydantic class itself) to the BaseOntology class.
        It also registers the DatatypeProperty to the KnowledgeGraph class.

        Args:
            prop (DatatypeProperty): The DatatypeProperty class to be registered
        """
        # register with iri as key
        if cls.data_property_lookup is None:
            cls.data_property_lookup = {}
        # TODO bring below back
        # if prop.predicate_iri in cls.data_property_lookup:
        #     raise ValueError(f'Data property with predicate IRI {prop.predicate_iri} already exists in {cls}: {cls.data_property_lookup[prop.predicate_iri]}.')
        cls.data_property_lookup[prop.predicate_iri] = prop

        # also register with knowledge graph
        KnowledgeGraph.register_property(prop)


        # TODO remove below
        # # register with name as key
        # if cls.data_property_lookup_name_as_key is None:
        #     cls.data_property_lookup_name_as_key = {}
        # # TODO bring below back
        # # if prop.__name__ in cls.data_property_lookup_name_as_key:
        # #     raise ValueError(f'Data property with name {prop.__name__} already exists in {cls}: {cls.data_property_lookup_name_as_key[prop.predicate_iri]}.')
        # # cls.data_property_lookup_name_as_key[prop.__name__] = prop

    @classmethod
    def export_to_graph(cls, g: Graph = None) -> Graph:
        """
        This method is used to export the ontology to a rdflib.Graph object.
        It operates at the TBox level, i.e. it only exports the classes and properties of the ontology.

        Args:
            g (Graph): The rdflib.Graph object to which the ontology will be exported
        """
        if g is None:
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
        # handle all object and data properties
        property_domain_range_lookup = KnowledgeGraph.construct_property_domain_range_lookup()
        if bool(cls.object_property_lookup):
            for prop in cls.object_property_lookup.values():
                g = prop.export_to_owl(
                    g,
                    property_domain_range_lookup[prop.predicate_iri]['rdfs_domain'],
                    property_domain_range_lookup[prop.predicate_iri]['rdfs_range'],
                )
        # handle all data properties
        if bool(cls.data_property_lookup):
            for prop in cls.data_property_lookup.values():
                g = prop.export_to_owl(
                    g,
                    property_domain_range_lookup[prop.predicate_iri]['rdfs_domain'],
                    property_domain_range_lookup[prop.predicate_iri]['rdfs_range'],
                )

        return g

    @classmethod
    def export_to_triple_store(cls, sparql_client: PySparqlClient):
        """
        This method is used to export the ontology to a triplestore.
        It operates at the TBox level, i.e. it only exports the classes and properties of the ontology.

        Args:
            sparql_client (PySparqlClient): The PySparqlClient object that connects to the triplestore
        """
        g = cls.export_to_graph()

        # upload to triplestore
        sparql_client.upload_graph(g)

    @classmethod
    def export_to_owl(cls, file_path: str, format: str = 'ttl'):
        """
        This method is used to export the ontology to an ontology file.
        It operates at the TBox level, i.e. it only exports the classes and properties of the ontology.

        Args:
            file_path (str): The path of the ontology file to be exported to
            format (str): The format of the ontology file, the default value is 'ttl'
        """
        g = cls.export_to_graph()

        # serialize
        g.serialize(destination=file_path, format=format)


class Owl(BaseOntology):
    # This is to enable TransitiveProperty so that it can be registered
    # It is not intended to be used as a standalone ontology
    base_url: ClassVar[str] = OWL_BASE_URL

class _BaseProperty(set, Generic[T]):
    rdfs_isDefinedBy: ClassVar[Type[BaseOntology]] = None
    predicate_iri: ClassVar[str] = None
    owl_minQualifiedCardinality: ClassVar[int] = 0
    owl_maxQualifiedCardinality: ClassVar[int] = None
    # TODO remove below
    # rdfs_domain: ClassVar[Set[str]] = None
    # rdfs_range: ClassVar[Set[str]] = None

# class BaseProperty(BaseModel, validate_assignment=True):
    # NOTE validate_assignment=True is to make sure the validation is triggered when range is updated
    """
    Base class that is inherited by ObjectProperty and DatatypeProperty.

    Attributes:
        rdfs_isDefinedBy: The ontology that defines the property
        predicate_iri: The predicate IRI of the property
        rdfs_domain: The domain of the property
        rdfs_range: The range of the property
    """

#     rdfs_isDefinedBy: ClassVar[BaseOntology] = None
#     rdfs_domain: ClassVar[Set] = None
#     # setting default_factory to set is safe here, i.e. it won't be shared between instances
#     # see https://docs.pydantic.dev/latest/concepts/models/#fields-with-non-hashable-default-values
#     rdfs_range: Set = Field(default_factory=set)

#     # TODO [future] vanilla set operations don't trigger the validation as of pydantic 2.6.1
#     # it also seems this will not be supported in the near future
#     # see https://github.com/pydantic/pydantic/issues/496
#     # for a workaround, see https://github.com/pydantic/pydantic/issues/8575
#     # and https://gist.github.com/geospackle/8f317fc19469b1e216edee3cc0f1c898

    # def __init__(self, **data) -> None:
    #     """
    #     The constructor of the BaseProperty class.
    #     It parses the rdfs_range attribute to make sure it's always a set.
    #     """
    #     # below code is to make sure rdfs_range is always a set even if it's a single value
    #     if 'rdfs_range' in data:
    #         if not isinstance(data['rdfs_range'], set):
    #             if not isinstance(data['rdfs_range'], list):
    #                 data['rdfs_range'] = [data['rdfs_range']]
    #             data['rdfs_range'] = set(data['rdfs_range'])
    #     else:
    #         data['rdfs_range'] = set()
    #     super().__init__(**data)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def __hash__(self):
        return hash((frozenset(self), self.predicate_iri))

    # def __hash__(self) -> int:
    #     return hash(tuple([self.predicate_iri] + sorted([o.__hash__() for o in self.rdfs_range])))

    # @property
    # def predicate_iri(self):
    #     return self.__class__.predicate_iri

    # @classmethod
    # def get_predicate_iri(cls) -> str:
    #     """ Get the predicate IRI of the property. """
    #     return construct_rdf_type(
    #         cls.rdfs_isDefinedBy.get_namespace_iri(), cls.__name__[:1].lower() + cls.__name__[1:])

    # @classmethod
    # def add_to_rdfs_domain(cls, domain: Type[BaseClass]):
    #     """
    #     Add an IRI to the set of property's domain.

    #     Args:
    #         domain (Type[BaseClass]): The class to be added as the rdfs_domain
    #     """
    #     if cls.rdfs_domain is None:
    #         cls.rdfs_domain = set()
    #     cls.rdfs_domain.add(domain.rdf_type)

    # @classmethod
    # def add_to_rdfs_range(cls, range: Type[T]):
    #     """
    #     Add an IRI to the set of property's range.

    #     Args:
    #         range (Type[T]): The class to be added as the rdfs_range
    #     """
    #     if cls.rdfs_range is None:
    #         cls.rdfs_range = set()
    #     try:
    #         if issubclass(range, BaseClass):
    #             cls.rdfs_range.add(range.rdf_type)
    #         else:
    #             cls.rdfs_range.add(_castPythonToXSD(range))
    #     except TypeError:
    #         raise TypeError(f'Invalid range type: {range}')
    #         # cls.rdfs_range.add(_castPythonToXSD(range))

    @classmethod
    def _is_inherited(cls, prop: Any) -> bool:
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

    @classmethod
    def add_to_graph(cls, g: Graph, s: str, o: Any) -> Graph:
        # try:
        #     g.add((URIRef(s), URIRef(cls.predicate_iri), Literal(o)))
        # except Exception as e:
        #     raise TypeError(f'Type of {o} ({type(o)}) is not supported by rdflib as a data property for {cls.predicate_iri}.', e)
        # return g
        raise NotImplementedError('This is an abstract method.')

    # def reassign_range(self, new_value):
    #     """ This function reassigns range of the object/data properties to new values. """
    #     setattr(self, 'range', new_value)

    # def collect_range_diff_to_graph(
    #     self,
    #     subject: str,
    #     cache: _BaseProperty,
    #     g_to_remove: Graph,
    #     g_to_add: Graph,
    #     recursive_depth: int = 0,
    #     traversed_iris: set = None,
    # ):
    #     """
    #     This is an abstract method that should be implemented by the subclasses.
    #     It is used to collect the difference between the range of the property and the cache.
    #     The recursion stops when the IRI is traversed already, the logic to determine this is at the BaseClass side.

    #     Args:
    #         subject (str): The subject of the property
    #         cache (BaseProperty): The cache of the property to compare with
    #         g_to_remove (Graph): The rdflib.Graph object to which the triples to be removed will be added
    #         g_to_add (Graph): The rdflib.Graph object to which the triples to be added will be added
    #         recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion
    #         traversed_iris (set): A set of IRIs that were already traversed in recursion

    #     Raises:
    #         NotImplementedError: This is an abstract method.
    #     """
    #     raise NotImplementedError("This is an abstract method.")

    @classmethod
    def export_to_owl(cls, g: Graph, rdfs_domain: set, rdfs_range: set, is_object_property: bool = True) -> Graph:
        """
        This method is used to export the triples of the property to an OWL file.
        It operates at the TBox level.

        Args:
            g (Graph): The rdflib.Graph object to which the property will be added
            is_object_property (bool): Whether the property is an object property or a data property

        Returns:
            Graph: The rdflib.Graph object with the added triples
        """
        property_iri = cls.predicate_iri
        g.add((URIRef(property_iri), RDFS.isDefinedBy, URIRef(cls.rdfs_isDefinedBy.get_namespace_iri())))
        # add rdf:type and super properties
        if is_object_property:
            g.add((URIRef(property_iri), RDF.type, OWL.ObjectProperty))
            idx = cls.__mro__.index(ObjectProperty)
        else:
            g.add((URIRef(property_iri), RDF.type, OWL.DatatypeProperty))
            idx = cls.__mro__.index(DatatypeProperty)
        for i in range(1, idx):
            g.add((URIRef(property_iri), RDFS.subPropertyOf, URIRef(cls.__mro__[i].predicate_iri)))
        # add domain
        if len(rdfs_domain) == 0:
            # it is possible that a property is defined without specifying its domain, so we only print a warning
            warnings.warn(f'Warning: property {cls} has no domain to be added, i.e. it is not used by any classes!')
        elif len(rdfs_domain) > 1:
            # union of class as domain
            bn = BNode()
            bn_union = BNode()
            g.add((bn, RDF.type, OWL.Class))
            g.add((bn, OWL.unionOf, bn_union))
            bn_union_lst = [bn_union]
            for d in rdfs_domain:
                g.add((bn_union_lst[-1], RDF.first, URIRef(d)))
                if len(bn_union_lst) < len(rdfs_domain):
                    bn_union_new = BNode()
                    g.add((bn_union_lst[-1], RDF.rest, bn_union_new))
                    bn_union_lst.append(bn_union_new)
                else:
                    g.add((bn_union_lst[-1], RDF.rest, RDF.nil))
            g.add((URIRef(property_iri), RDFS.domain, bn))
        else:
            # single class as domain
            for d in rdfs_domain:
                g.add((URIRef(property_iri), RDFS.domain, URIRef(d)))
        # add range
        if len(rdfs_range) == 0:
            # it is possible that a property is defined without specifying its range, so we only print a warning
            warnings.warn(f'Warning: property {cls} has no range to be added, i.e. it is not used by any classes!')
        elif len(rdfs_range) == 1:
            for r in rdfs_range:
                g.add((URIRef(property_iri), RDFS.range, URIRef(r)))
        else:
            raise NotImplementedError(f'Union of range is not supported yet! Property: {cls}. rdfs_domain: {rdfs_domain}. rdfs_range: {rdfs_range}')
        return g

    # @classmethod
    # def reveal_possible_property_range(cls) -> Set[T]:
    #     """
    #     This is an abstract method that should be implemented by the subclasses.
    #     It should unpack the range of the property from use_as_range.

    #     Raises:
    #         NotImplementedError: This is an abstract method.

    #     Returns:
    #         Set[T]: The set of possible range of the property
    #     """
    #     raise NotImplementedError("This is an abstract method.")

    # @classmethod
    # def reveal_property_range_iri(cls) -> str:
    #     """
    #     This is an abstract method that should be implemented by the subclasses.
    #     It should return the IRI of the range of the property.

    #     Raises:
    #         NotImplementedError: This is an abstract method.

    #     Returns:
    #         str: The IRI of the range of the property
    #     """
    #     raise NotImplementedError("This is an abstract method.")

    @classmethod
    def retrieve_cardinality(cls) -> Tuple[int, int]:
        """
        This method is used to retrieve the cardinality of the property.

        Returns:
            Tuple[int, int]: The minimum and maximum cardinality of the property
        """
        return cls.owl_minQualifiedCardinality, cls.owl_maxQualifiedCardinality

    # def create_cache(self, recursive_depth: int = 0, traversed_iris: set = None) -> _BaseProperty:
    #     """
    #     This is an abstract method that should be implemented by the subclasses.
    #     It is used to create a cache for the property.
    #     The recursion stops when the IRI is traversed already, the logic to determine this is at the BaseClass side.

    #     Args:
    #         recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion
    #         traversed_iris (set): A set of IRIs that were already traversed in recursion

    #     Raises:
    #         NotImplementedError: This is an abstract method.
    #     """
    #     return NotImplementedError("This is an abstract method.")

    def _graph(self, subject: str, g: Graph, is_object_property: bool = True):
        if is_object_property:
            for o in self.range:
                g.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o.instance_iri if isinstance(o, BaseClass) else o)))
        else:
            for o in self.range:
                g.add((URIRef(subject), URIRef(self.predicate_iri), Literal(o)))
        return g

    # TODO remove the below
    @classmethod
    def my_method(cls):
        print(f'My method called for {cls.__name__}')

    @classmethod
    def create_from_base(
        cls,
        class_name: str,
        ontology: Type[BaseOntology],
        min_cardinality: Optional[int] = 0,
        max_cardinality: Optional[int] = None,
        base_property: Type[_BaseProperty] = None,
    ) -> Type[_BaseProperty]:
        # NOTE we inherit cardinality from the base_property if not specified
        base = cls if base_property is None else base_property
        return type(class_name, (base,), {
            'rdfs_isDefinedBy': ontology,
            'owl_minQualifiedCardinality': min_cardinality if bool(min_cardinality) else base.owl_minQualifiedCardinality,
            'owl_maxQualifiedCardinality': max_cardinality if bool(max_cardinality) else base.owl_maxQualifiedCardinality,
        })

    @classmethod
    def _validate_before(cls, value: Any, info: ValidationInfo) -> Any:
        """
        Developer can overwrite this method to add custom validation.

        Args:
            value (Any): The value to be assigned to the field
            info (ValidationInfo): Additional information can be used for validation,
                e.g. info.field_name is the name of the field being validated

        Returns:
            Any: The validated value
        """
        # print('==================================================')
        # print('before')
        # print(value)
        # print(type(value))
        # print(len(value))
        # print(info)
        # # print(cls.model_fields[info.field_name])
        # print('end')
        # print('==================================================')
        return value

    # @classmethod
    # def _validate(cls, value, validator_info):
    #     # this runs for each field
    #     print(value)
    #     print(validator_info)
    #     if not isinstance(value, set):
    #         if not isinstance(value, list):
    #             value = [value]
    #         value = cls(value)
    #     # if  is not None and len(value) < cls.owl_minQualifiedCardinality:
    #     #     raise ValueError(f'Set must contain at least {cls.owl_minQualifiedCardinality} items, got {len(value)}: {value}.')
    #     # if  is not None and len(value) > cls.owl_maxQualifiedCardinality:
    #     #     raise ValueError(f'Set must contain at most {cls.owl_maxQualifiedCardinality} items, got {len(value)}: {value}.')
    #     return value

    @classmethod
    def __get_pydantic_core_schema__(
        cls, source_type: Any, handler: GetCoreSchemaHandler
    ) -> CoreSchema:
        # https://docs.pydantic.dev/latest/concepts/types/#handling-custom-generic-classes
        # print('************************************************************')
        try:
            tp = get_args(source_type)[0]
        except Exception as e:
            raise ValueError(f"""Error: {e}. Type used as the field for object/data property: {source_type}.
                Did you forget to include the range in the class definition?
                E.g. `{source_type.__name__.lower()}: {source_type.__name__}[str]` for data property.
                or `{source_type.__name__.lower()}: {source_type.__name__}[MyClass]` for object property.""")
        # print(get_origin(source_type))
        # print('ah haha', source_type)
        # print(get_args(source_type))
        # print(tp)
        if issubclass(tp, BaseClass):
            # the set can contain either actual objects of BaseClass or string IRIs
            tp_schema = core_schema.union_schema(choices=[handler.generate_schema(tp), core_schema.str_schema()])
        else:
            # the means datatype properties, therefore the set can only contain the data type
            tp_schema = handler.generate_schema(tp)

        return core_schema.chain_schema(
            [
                core_schema.with_info_before_validator_function(
                    cls._validate_before,
                    core_schema.set_schema(
                        items_schema=tp_schema,
                        # add validation for cardinality
                        min_length=cls.owl_minQualifiedCardinality,
                        max_length=cls.owl_maxQualifiedCardinality,
                    ),
                    field_name=handler.field_name,
                ),
            ]
        )


class BaseClass(BaseModel, validate_assignment=True):
    """
    Base class for all the Python classes that are used to define the classes in ontology.

    Attributes:
        rdfs_isDefinedBy (Ontology): The ontology that defines the class
        object_lookup (Dict[str, BaseClass]): A dictionary that maps the IRI of the object to the object
        rdfs_comment (str): The comment of the instance
        rdfs_label (str): The label of the instance
        instance_iri (str): The IRI of the instance

    Example:
    class MyClass(BaseOntology):
        myObjectProperty: MyObjectProperty
        myDatatypeProperty: MyDatatypeProperty
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

    rdfs_isDefinedBy: ClassVar[BaseOntology] = None
    """ > NOTE for all subclasses, one can just use `rdfs_isDefinedBy = MyOntology`,
        see [this discussion in Pydantic](https://github.com/pydantic/pydantic/issues/2061)"""
    rdf_type: ClassVar[str] = OWL_BASE_URL + 'Class'
    """ > NOTE rdf_type is the automatically generated IRI of the class which can also be accessed at the instance level. """
    object_lookup: ClassVar[Dict[str, BaseClass]] = None
    rdfs_comment: Optional[str] = Field(default=None)
    rdfs_label: Optional[str] = Field(default=None)
    instance_iri: str = Field(default=None)
    # format of the cache for all properties: {property_name: property_object}
    _latest_cache: Dict[str, Any] = PrivateAttr(default_factory=dict)
    _exist_in_kg: bool = PrivateAttr(default=False)

    @classmethod
    def __pydantic_init_subclass__(cls, **kwargs):
        # ensure that the cls already has field rdfs_isDefinedBy
        if cls.rdfs_isDefinedBy is None:
            raise AttributeError(f"Did you forget to specify `rdfs_isDefinedBy` for your class {cls}?")

        # set the rdf_type
        cls.rdf_type = construct_rdf_type(cls.rdfs_isDefinedBy.get_namespace_iri(), cls.__name__)

        # TODO remove below
        # # set the domain of all object/data properties
        # for f, field_info in cls.model_fields.items():
        #     tp = get_args(field_info.annotation)[0] if type(field_info.annotation) == _UnionGenericAlias else field_info.annotation
        #     if _BaseProperty._is_inherited(tp):
        #         tp.add_to_rdfs_domain(cls)
        #         tp.add_to_rdfs_range(get_args(tp)[0])
        #     elif isinstance(tp, ForwardRef):
        #         # possible situations:
        #         # - ForwardRef('MyObjectProperty[MyClass]')
        #         # - ForwardRef('Optional[MyObjectProperty[MyClass]]')
        #         # if the field is ForwardRef, then postpone the adding of domain
        #         cls.rdfs_isDefinedBy.postpone_rdfs_domain_range(tp.__forward_arg__, cls)

        # TODO remove below
        # # check if there's any forward referenced object properties with this class as range
        # set_properties = cls.rdfs_isDefinedBy.retrieve_postponed_property_given_range(cls.__name__)
        # for p in set_properties:
        #     if p in cls.rdfs_isDefinedBy.object_property_lookup_name_as_key:
        #         prop = cls.rdfs_isDefinedBy.object_property_lookup_name_as_key[p]
        #         # add the current class as range of the property also remove it from the dict
        #         prop.add_to_rdfs_range(cls)
        #         cls.rdfs_isDefinedBy.forward_refs_key_predicate[p]['rdfs_range'].remove(cls.__name__)
        #         # add the classes as domain of the property
        #         to_add_as_domain = cls.rdfs_isDefinedBy.retrieve_postponed_property_domain(prop.__name__)
        #         for c in to_add_as_domain:
        #             prop.add_to_rdfs_domain(c)
        #     else:
        #         if cls.__name__ not in cls.rdfs_isDefinedBy.forward_refs_key_predicate:
        #             cls.rdfs_isDefinedBy.forward_refs_key_range[cls.__name__] = set()
        #         cls.rdfs_isDefinedBy.forward_refs_key_range[cls.__name__].add(p)

        # register the class to the ontology
        cls.rdfs_isDefinedBy.register_class(cls)

    # NOTE TODO the __init__ is commented out for now, maybe bring it back later?
    # def __init__(self, **data) -> None:
    #     """
    #     The constructor of the BaseClass.
    #     It processes the range of the properties so that allows to simplify the input for the user.
    #     i.e. the user can directly assign the object/data property with a list of objects as its range.
    #     e.g. object = BaseClass(myObjectProperty=[obj1, obj2, obj3])
    #     """
    #     for f, field_info in self.__class__.model_fields.items():
    #         if _BaseProperty._is_inherited(field_info.annotation):
    #             if f in data:
    #                 possible_type = get_args(field_info.annotation)
    #                 if isinstance(data[f], list):
    #                     if all(isinstance(i, possible_type) for i in data[f]):
    #                         data[f] = field_info.annotation(set(data[f]))
    #                 elif isinstance(data[f], set):
    #                     if all(isinstance(i, possible_type) for i in data[f]):
    #                         data[f] = field_info.annotation(data[f])
    #                 elif isinstance(data[f], possible_type):
    #                     data[f] = field_info.annotation(data[f])
    #                 # for all the other cases, we will let the pydantic to validate the input
    #             else:
    #                 # if the object/data property is not in the input
    #                 if field_info.default is PydanticUndefined and field_info.default_factory is None:
    #                     # also if the default value is undefined and there is no default_factory
    #                     # we will set it to its default initialisation
    #                     # this doesn't affect the validation process
    #                     # as the actual validation will be done by checking the cardinality of the property
    #                     # the default initialisation of the range is an empty set
    #                     data[f] = field_info.annotation(set())
    #         elif isinstance(field_info.annotation, ForwardRef):
    #             # this means the object/data property is not resolved yet
    #             # pydantic will re-build the model in super().__init__(**data)
    #             # therefore we need to get it ready in format of dictionary {'range': ...} to avoid validation error
    #             if f in data:
    #                 if not _BaseProperty._is_inherited(type(data[f])):
    #                     data[f] = {data[f]}
    #             else:
    #                 data[f] = set()

    #     super().__init__(**data)

    # def __getattribute__(self, name) -> Any:
    #     _fields = super().__getattribute__('model_fields')
    #     if name in _fields:
    #         if _BaseProperty._is_inherited(_fields[name].annotation):
    #             return super().__getattribute__(name).range
    #     return super().__getattribute__(name)

    # def __setattr__(self, name: str, value: Any) -> None:
    #     _fields = super().__getattribute__('model_fields')
    #     if name in _fields:
    #         if _BaseProperty._is_inherited(_fields[name].annotation):
    #             if not isinstance(value, set):
    #                 if not isinstance(value, list):
    #                     value = [value]
    #                 value = set(value)
    #             super().__getattribute__(name).range = value
    #             return
    #     super().__setattr__(name, value)

    # def get_original_property_field(self, name: str) -> Any:
    #     return object.__getattribute__(self, name)

    def model_post_init(self, __context: Any) -> None:
        """
        The post init process of the BaseClass.
        It sets the instance_iri if it is not set.
        It also registers the object to the lookup dictionary of the class.

        Args:
            __context (Any): Any other context that is needed for the post init process

        Returns:
            None: It calls the super().model_post_init(__context) to finish the post init process
        """
        if not bool(self.instance_iri):
            self.instance_iri = init_instance_iri(
                self.__class__.rdfs_isDefinedBy.get_namespace_iri(),
                self.__class__.__name__
            )
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
    def retrieve_subclass(cls, iri: str) -> Type[BaseClass]:
        """
        This function retrieves the subclass of the current class based on the IRI.
        If the IRI is the same as the rdf:type of the current class, it will return the current class itself.

        Args:
            iri (str): The IRI of the subclass

        Returns:
            Type[BaseClass]: The subclass of the BaseClass
        """
        if iri == cls.rdf_type:
            return cls
        return cls.construct_subclass_dictionary()[iri]

    @classmethod
    def construct_subclass_dictionary(cls) -> Dict[str, Type[BaseClass]]:
        """
        This function constructs a dictionary that maps the rdf:type to the subclass of the BaseClass.

        Returns:
            Dict[str, Type[BaseClass]]: The dictionary that maps the rdf:type to the subclass of the BaseClass
        """
        subclass_dict = {}
        for clz in cls.__subclasses__():
            subclass_dict[clz.rdf_type] = clz
            # recursively add the subclass of the subclass
            subclass_dict.update(clz.construct_subclass_dictionary())
        return subclass_dict

    @classmethod
    def push_all_instances_to_kg(cls, sparql_client: PySparqlClient, recursive_depth: int = 0):
        """
        This function pushes all the instances of the class to the knowledge graph.

        Args:
            sparql_client (PySparqlClient): The SPARQL client that is used to push the data to the KG
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion
        """
        g_to_remove = Graph()
        g_to_add = Graph()
        cls.pull_from_kg(cls.object_lookup.keys(), sparql_client, recursive_depth)
        for obj in cls.object_lookup.values():
            g_to_remove, g_to_add = obj.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)
        sparql_client.delete_and_insert_graphs(g_to_remove, g_to_add)

    @classmethod
    def clear_object_lookup(cls):
        """
        This function clears the lookup dictionary of the class.
        """
        if cls.object_lookup is not None:
            iris = list(cls.object_lookup.keys())
            for i in iris:
                del cls.object_lookup[i]

    # @classmethod
    # def get_rdf_type(cls) -> str:
    #     """
    #     This function returns the rdf_type of the class.

    #     Returns:
    #         str: The rdf_type of the class (rdf:type in owl)
    #     """
    #     if cls == BaseClass:
    #         return OWL_BASE_URL + 'Class'
    #     return construct_rdf_type(cls.rdfs_isDefinedBy.get_namespace_iri(), cls.__name__)

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
        if isinstance(iris, str):
            iris = [iris]
        iris = set(iris)
        # if the iris are not provided, then just return empty list
        if not bool(iris):
            return []
        # prepare the list to be returned
        instance_lst = []

        # check if any of the iris are loading
        i_loading = set()
        for i in iris:
            if KnowledgeGraph.is_iri_been_loading(i):
                # for those that are loading, use string here and remove it from query
                instance_lst.append(i)
                i_loading.add(i)
            else:
                # for those that are not loading, indicate they are to be loaded now
                KnowledgeGraph.add_iri_to_loading(i)
        iris = iris - i_loading

        # behaviour of recursive_depth: 0 means no recursion, -1 means infinite recursion, n means n-level recursion
        flag_pull = abs(recursive_depth) > 0
        recursive_depth = max(recursive_depth - 1, 0) if recursive_depth > -1 else max(recursive_depth - 1, -1)
        # TODO what do we do with undefined properties in python class? - write a warning message or we can add them to extra_fields https://docs.pydantic.dev/latest/concepts/models/#extra-fields
        # return format: {iri: {predicate: {object}}}
        node_dct = sparql_client.get_outgoing_and_attributes(iris)
        for iri, props in node_dct.items():
            # TODO optimise the time complexity of the following code when the number of instances is large
            # check if the rdf:type of the instance matches the calling class or any of its subclasses
            target_clz_rdf_type = list(props[RDF.type.toPython()])[0]
            if target_clz_rdf_type != cls.rdf_type and target_clz_rdf_type not in cls.construct_subclass_dictionary().keys():
                raise ValueError(
                    f"""The instance {iri} is of type {props[RDF.type.toPython()]},
                    it doesn't match the rdf:type of class {cls.__name__} ({cls.rdf_type}),
                    nor any of its subclasses ({cls.construct_subclass_dictionary()}),
                    therefore it cannot be instantiated.""")
            inst = KnowledgeGraph.get_object_from_lookup(iri)
            # obtain the target class in case it is a subclass
            target_clz = cls.retrieve_subclass(target_clz_rdf_type)
            # rebuild the model in case there're any ForwardRef that were not resolved previously
            target_clz.model_rebuild()

            # instead of calling cls.get_object_properties() and cls.get_data_properties()
            # calling methods of target_clz ensures that all properties are correctly inherited
            ops = target_clz.get_object_properties()
            dps = target_clz.get_data_properties()
            # handle object properties (where the recursion happens)
            # TODO [I think this is solved?] need to consider what to do when two instances pointing to each other, or if there's circular nodes
            # here object_properties_dict is a fetch of the remote KG
            object_properties_dict = {}
            for op_iri, op_dct in ops.items():
                _set = set()
                if op_iri in props:
                    if flag_pull:
                        c_tp: BaseClass = get_args(op_dct['type'])[0]
                        _set = c_tp.pull_from_kg(props[op_iri], sparql_client, recursive_depth)
                    else:
                        _set = set(props[op_iri])
                object_properties_dict[op_dct['field']] = _set
            # here we handle data properties (data_properties_dict is a fetch of the remote KG)
            data_properties_dict = {dp_dct['field']: set(props[dp_iri]) if dp_iri in props else set() for dp_iri, dp_dct in dps.items()}
            # handle rdfs:label and rdfs:comment (also fetch of the remote KG)
            rdfs_properties_dict = {}
            if RDFS.label.toPython() in props:
                if len(props[RDFS.label.toPython()]) > 1:
                    raise ValueError(f"The instance {iri} has multiple rdfs:label {props[RDFS.label.toPython()]}.")
                rdfs_properties_dict['rdfs_label'] = list(props[RDFS.label.toPython()])[0]
            if RDFS.comment.toPython() in props:
                if len(props[RDFS.comment.toPython()]) > 1:
                    raise ValueError(f"The instance {iri} has multiple rdfs:comment {props[RDFS.comment.toPython()]}.")
                rdfs_properties_dict['rdfs_comment'] = list(props[RDFS.comment.toPython()])[0]
            # instantiate the object
            if inst is not None:
                for op_iri, op_dct in ops.items():
                    if flag_pull:
                        # below lines pull those object properties that are NOT connected in the remote KG,
                        # but are connected in the local python memory
                        # e.g. object `a` has a field `to_b` that points to object `b`
                        # but triple <a> <to_b> <b> does not exist in the KG
                        # this code then ensures the cache of object `b` is accurate
                        # TODO [future] below query can be combined with those connected in the KG to save amount of queries
                        c_tp: BaseClass = get_args(op_dct['type'])[0]
                        c_tp.pull_from_kg(
                            set([o.instance_iri if isinstance(o, BaseClass) else o for o in getattr(inst, op_dct['field'])]) - set(props.get(op_iri, [])),
                            sparql_client, recursive_depth)
                # now collect all featched values
                fetched = {
                    k: set([o.instance_iri if isinstance(o, BaseClass) else o for o in v])
                    for k, v in object_properties_dict.items()
                } # object properties
                fetched.update({k: set(copy.deepcopy(v)) for k, v in data_properties_dict.items()}) # data properties
                fetched.update(rdfs_properties_dict) # rdfs properties
                # compare it with cached values and local values for all object/data/rdfs properties
                # if the object is already in the lookup, then update the object for those fields that are not modified in the python
                inst.update_according_to_fetch(fetched, flag_pull)
            else:
                # if the object is not in the lookup, create a new object
                inst = target_clz(
                    instance_iri=iri,
                    **rdfs_properties_dict,
                    **object_properties_dict,
                    **data_properties_dict,
                )
                inst.create_cache()

            inst._exist_in_kg = True
            # update cache here
            instance_lst.append(inst)
            # remote inst from the loading status
            KnowledgeGraph.remove_iri_from_loading(inst.instance_iri)
        return instance_lst

    @classmethod
    def pull_all_instances_from_kg(cls, sparql_client: PySparqlClient, recursive_depth: int = 0) -> Set[BaseClass]:
        """
        This function pulls all instances of the calling class from the knowledge graph (triplestore).
        It calls the pull_from_kg function with the IRIs of all instances of the calling class.
        By default, it pulls the instances with no recursion.

        Args:
            sparql_client (PySparqlClient): The SPARQL client that is used to pull the data from the KG
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion

        Returns:
            Set[BaseClass]: A set of objects that are pulled from the KG
        """
        iris = sparql_client.get_all_instances_of_class(cls.rdf_type)
        return cls.pull_from_kg(iris, sparql_client, recursive_depth)

    @classmethod
    def get_object_and_data_properties(cls) -> Dict[str, Dict[str, Union[str, Type[_BaseProperty]]]]:
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
        dct_op = {}
        for f, field_info in cls.model_fields.items():
            op = get_args(field_info.annotation)[0] if type(field_info.annotation) == _UnionGenericAlias else field_info.annotation
            if ObjectProperty._is_inherited(op):
                dct_op[op.predicate_iri] = {'field': f, 'type': op}
        return dct_op

    @classmethod
    def get_data_properties(cls) -> Dict[str, Dict[str, Union[str, Type[DatatypeProperty]]]]:
        """
        This function returns the data properties of the calling class.

        Returns:
            Dict[str, Dict[str, Union[str, Type[DatatypeProperty]]]]: A dictionary containing the data properties of the calling class
                in the format of {predicate_iri: {'field': field_name, 'type': field_clz}}
                e.g. {'https://twa.com/myDatatypeProperty': {'field': 'myDatatypeProperty', 'type': MyDatatypeProperty}}
        """
        dct_dp = {}
        for f, field_info in cls.model_fields.items():
            dp = get_args(field_info.annotation)[0] if type(field_info.annotation) == _UnionGenericAlias else field_info.annotation
            if DatatypeProperty._is_inherited(dp):
                dct_dp[dp.predicate_iri] = {'field': f, 'type': dp}
        return dct_dp

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
        # rebuild model to resovle any ForwardRef
        cls.model_rebuild()
        cls_iri = cls.rdf_type
        g.add((URIRef(cls_iri), RDF.type, OWL.Class))
        g.add((URIRef(cls_iri), RDFS.isDefinedBy, URIRef(cls.rdfs_isDefinedBy.get_namespace_iri())))
        # add super classes
        idx = cls.__mro__.index(BaseClass)
        for i in range(1, idx):
            g.add((URIRef(cls_iri), RDFS.subClassOf, URIRef(cls.__mro__[i].rdf_type)))
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
                if issubclass(prop, ObjectProperty):
                    g.add((restriction, OWL.onClass, URIRef(get_args(prop)[0].rdf_type)))
                else:
                    g.add((restriction, OWL.onClass, URIRef(_castPythonToXSD(get_args(prop)[0]))))
                g.add((restriction, OWL.onProperty, URIRef(prop_iri)))
                g.add((URIRef(cls_iri), RDFS.subClassOf, restriction))
        return g

    def create_cache(self, recursive_depth: int = 0, traversed_iris: set = None):
        """
        This function creates a cache of the instance of the calling class.
        The recursion stops when the IRI is traversed already.

        Args:
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion
            traversed_iris (set): A set of IRIs that were already traversed in recursion
        """
        # note here we create deepcopy for all fields so there won't be issue caused by referencing the same memory address
        # firstly, create cache for those properties that were connected in previous cache but might not be presented in the current local values
        if traversed_iris is None:
            traversed_iris = set()
        if self.instance_iri in traversed_iris:
            return
        traversed_iris.add(self.instance_iri)
        for f, cached in self._latest_cache.items():
            # TODO revisit this part
            f_tp = get_args(self.model_fields[f].annotation)[0] if type(self.model_fields[f].annotation) == _UnionGenericAlias else self.model_fields[f].annotation
            if ObjectProperty._is_inherited(f_tp):
                disconnected_object_properties = cached - getattr(self, f)
                for o in disconnected_object_properties:
                    obj = KnowledgeGraph.get_object_from_lookup(o)
                    if obj is not None:
                        obj.create_cache(recursive_depth, traversed_iris)
        # secondly (and finally), create cache for all currently connected properties
        recursive_depth = max(recursive_depth - 1, 0) if recursive_depth > -1 else max(recursive_depth - 1, -1)
        for f, field_info in self.model_fields.items():
            tp = get_args(field_info.annotation)[0] if type(field_info.annotation) == _UnionGenericAlias else field_info.annotation
            if DatatypeProperty._is_inherited(tp):
                self._latest_cache[f] = copy.deepcopy(getattr(self, f))
            elif ObjectProperty._is_inherited(tp):
                _set_for_comparison = set()
                for o in getattr(self, f):
                    if isinstance(o, BaseClass):
                        # this function will be useful when pushing a brand new (nested) object to knowledge graph
                        # so that the cache of those objects appeared at deeper recursive_depth are also updated
                        o.create_cache(recursive_depth, traversed_iris)
                        _set_for_comparison.add(o.instance_iri)
                    elif isinstance(o, str):
                        obj = KnowledgeGraph.get_object_from_lookup(o)
                        if obj is not None:
                            obj.create_cache(recursive_depth, traversed_iris)
                        _set_for_comparison.add(o)
                    else:
                        raise Exception(f"Unsupported datatype {type(o)} for range of object property {self}")
                # add the as the set that will actually be used for comparison when pulling/pushing to cache
                self._latest_cache[f] = copy.deepcopy(_set_for_comparison)
            else:
                self._latest_cache[f] = copy.deepcopy(getattr(self, f))

        # TODO remove below
        # self._latest_cache = {f: getattr(self, f).create_cache(recursive_depth, traversed_iris)
        #                       if _BaseProperty._is_inherited(field_info.annotation) else copy.deepcopy(getattr(self, f))
        #                       for f, field_info in self.model_fields.items()}

    def revert_local_changes(self):
        """ This function reverts the local changes made to the python object to cached values. """
        for f, field_info in self.model_fields.items():
            if _BaseProperty._is_inherited(field_info.annotation):
                setattr(self, f, copy.deepcopy(self._latest_cache.get(f, field_info.annotation(set()))))
            else:
                setattr(self, f, copy.deepcopy(self._latest_cache.get(f, None)))

    def update_according_to_fetch(self, fetched: dict, flag_connect_object: bool):
        """
        This function compares the fetched values with the cached values and local values.
        It updates the cache and local values depend on the comparison results.
        NOTE that this function should not be called by users.

        Args:
            fetched (dict): The dictionary containing the fetched values
            flag_connect_object (bool): The boolean flag to indicate whether to use python objects
                in memory or string IRIs when reconnecting the range of object properties
        """
        for p_iri, p_dct in self.__class__.get_object_and_data_properties().items():
            fetched_value = fetched.get(p_dct['field']) if p_dct['field'] in fetched else set()
            cached_value = self._latest_cache.get(p_dct['field']) if p_dct['field'] in self._latest_cache else set()
            local_value = getattr(self, p_dct['field'])
            # below code compare the three values, the expected behaviour elaborated:
            # if fetched == cached --> no remote changes, update cache, no need to worry about local changes
            # if fetched != cached --> remote changed, now should check if local has changed:
            #     if local == cached --> no local changes, can update both cache and local values with fetched value
            #     if local != cached --> there are local changed, now should check if the local changes are the same as remote (unlikely tho)
            #         if local != fetched --> raise exception
            #         if local == fetched --> (which is really unlikely) update cache only
            # in practice, the above logic can be simplified:
            if fetched_value != cached_value:
                if local_value == cached_value:
                    # no local changes, therefore update both cached (delayed later) and local values to the fetched value
                    setattr(self, p_dct['field'], copy.deepcopy(fetched_value))
                else:
                    # there are both local and remote changes, now compare these two
                    if local_value != fetched_value:
                        raise Exception(f"""The remote changes in knowledge graph conflicts with local changes
                            for {self.instance_iri} {p_iri}:
                            Objects appear in the remote but not in the local: {fetched_value}
                            Triples appear in the local but not the remote: {local_value}""")
            # the cache can be updated regardless as long as there are no exceptions
            self._latest_cache[p_dct['field']] = copy.deepcopy(fetched_value)

            # when pulling the same objects again but with different recursive_depth
            # below ensures python objects in memory / the IRIs are used correctly for range of object properties
            if ObjectProperty._is_inherited(p_dct['type']):
                _local_value_set = getattr(self, p_dct['field'])
                if bool(_local_value_set):
                    if flag_connect_object and isinstance(next(iter(_local_value_set)), str):
                        setattr(self, p_dct['field'], set([KnowledgeGraph.get_object_from_lookup(o) for o in _local_value_set]))
                    if not flag_connect_object and isinstance(next(iter(_local_value_set)), BaseClass):
                        setattr(self, p_dct['field'], set([o.instance_iri for o in _local_value_set]))

        # compare rdfs_comment and rdfs_label
        for r in ['rdfs_comment', 'rdfs_label']:
            fetched_value = fetched.get(r, None)
            cached_value = self._latest_cache.get(r, None)
            local_value = getattr(self, r)
            # apply the same logic as above
            if fetched_value != cached_value:
                if local_value == cached_value:
                    setattr(self, r, copy.deepcopy(fetched_value))
                else:
                    if local_value != fetched_value:
                        raise Exception(f"""The remote changes of {r} in knowledge graph conflicts with local changes.
                            Remote: {fetched_value}.\nLocal : {local_value}""")
            self._latest_cache[r] = copy.deepcopy(fetched_value)

    def get_object_property_by_iri(self, iri: str) -> ObjectProperty:
        """
        This function returns the object property by the IRI of the property.

        Args:
            iri (str): IRI of the object property

        Returns:
            ObjectProperty: The object property
        """
        dct = self.__class__.get_object_properties()
        field_name = dct.get(iri, {}).get('field', None)
        if field_name is not None:
            return getattr(self, field_name)
        else:
            return None

    # def get_object_property_range_iris(self, field_name: str) -> List[str]:
    #     """
    #     This function returns the IRIs of the range of the object property.

    #     Args:
    #         field_name (str): The name of the field, e.g. 'myObjectProperty'

    #     Returns:
    #         List[str]: A list of IRIs of the range of the object property
    #     """
    #     return [o.instance_iri if isinstance(o, BaseClass) else o for o in getattr(self, field_name)]

    def delete_in_kg(self, sparql_client: PySparqlClient):
        # TODO implement this method
        raise NotImplementedError

    def push_to_kg(
        self,
        sparql_client: PySparqlClient,
        recursive_depth: int = 0,
        pull_first: bool = False,
        maximum_retry: int = 0,
    ) -> Tuple[Graph, Graph]:
        """
        This function pushes the triples of the calling object to the knowledge graph (triplestore).

        Args:
            sparql_client (PySparqlClient): The SPARQL client object to be used to push the triples
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion
            pull_first (bool): Whether to pull the latest triples from the KG before pushing the triples
            maximum_retry (int): The number of retries if any exception was raised during SPARQL update

        Returns:
            Tuple[Graph, Graph]: A tuple of two rdflib.Graph objects containing the triples to be removed and added
        """
        # TODO [future] what happens when KG changed during processing in the python side? race conditions...
        # NOTE when push, the objects in memory are loaded to collect diff and only stops when it's string (i.e. no object cached)
        # this supports the situation where recursive_depth specified here is greater than the value used to pull the object

        # pull the latest triples from the KG if needed
        if pull_first:
            self.__class__.pull_from_kg(self.instance_iri, sparql_client, recursive_depth)
        # type of changes: remove old triples, add new triples
        g_to_remove = Graph()
        g_to_add = Graph()
        g_to_remove, g_to_add = self.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)

        # retry push if any exception is raised
        retry_delay = 2
        for attempt in range(0, maximum_retry +1):
            try:
                sparql_client.delete_and_insert_graphs(g_to_remove, g_to_add)
                # if no exception was thrown, update cache
                self.create_cache(recursive_depth)
                return g_to_remove, g_to_add
            except Exception as e:
                if attempt < maximum_retry:
                    time.sleep(retry_delay)
                else:
                    raise e

    def collect_diff_to_graph(self, g_to_remove: Graph, g_to_add: Graph, recursive_depth: int = 0, traversed_iris: set = None) -> Tuple[Graph, Graph]:
        """
        This function collects the differences between the latest cache and the current instance of the calling object.
        The recursion stops when the IRI is traversed already.

        Args:
            g_to_remove (Graph): The rdflib.Graph object to which the triples to be removed will be added
            g_to_add (Graph): The rdflib.Graph object to which the triples to be added will be added
            recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion
            traversed_iris (set): A set of IRIs that were already traversed in recursion

        Returns:
            Tuple[Graph, Graph]: A tuple of two rdflib.Graph objects containing the triples to be removed and added
        """
        if traversed_iris is None:
            traversed_iris = set()
        if self.instance_iri in traversed_iris:
            return g_to_remove, g_to_add
        traversed_iris.add(self.instance_iri)
        for f, field_info in self.model_fields.items():
            # TODO Optional[]
            tp: ObjectProperty | DatatypeProperty = get_args(field_info.annotation)[0] if type(field_info.annotation) == _UnionGenericAlias else field_info.annotation
            if _BaseProperty._is_inherited(tp):
                # # TODO optimise the below codes
                # behaviour of recursive_depth: 0 means no recursion, -1 means infinite recursion, n means n-level recursion
                # NOTE this is only revelant for object properties
                flag_collect = abs(recursive_depth) > 0
                recursive_depth = max(recursive_depth - 1, 0) if recursive_depth > -1 else max(recursive_depth - 1, -1)

                # TODO [revise this statement] returning None as p_cache supports the default_factory that initialise the object with empty input values
                # for those properties with minimum cardinality 1, otherwise it will fail the pydantic validation
                p_cache = self._latest_cache.get(f, set())
                p_now = getattr(self, f)
                # # TODO remove below
                # try:
                #     p_now.collect_range_diff_to_graph(self.instance_iri, p_cache, g_to_remove, g_to_add, recursive_depth, traversed_iris)
                # except Exception as e:
                #     raise Exception(f'f: {f}. p_cache: {p_cache}. p_now: {p_now}') from e

                # # [What-is-this???] NOTE this supports the default_factory that initialise the object with empty input values for those properties with minimum cardinality 1
                # cached_range = cache.range if cache is not None else set()
                # # compare the range and its cache to find out what to remove and what to add
                # # NOTE this supports the default_factory that initialise the object with empty input values for those properties with minimum cardinality 1
                # cached_range = cache.range if cache is not None else set()
                # diff_to_remove = cached_range - self.range
                # diff_to_add = self.range - cached_range

                # compare the range and its cache to find out what to remove and what to add
                # remove the objects that are in cache but not in local values
                diff_to_remove = p_cache - p_now
                for d in diff_to_remove:
                    g_to_remove = tp.add_to_graph(g_to_remove, self.instance_iri, d)

                # add the objects that are in local values but not in cache
                diff_to_add = p_now - p_cache
                for d in diff_to_add:
                    g_to_add = tp.add_to_graph(g_to_add, self.instance_iri, d)
                    # if flag_collect and issubclass(tp, ObjectProperty):
                    #     d_py = d if isinstance(d, BaseClass) else KnowledgeGraph.get_object_from_lookup(d)
                    #     if d_py is not None:
                    #         g_to_remove, g_to_add = d_py.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth, traversed_iris)

                # besides the differences between the local values and cache
                # also need to consider the intersection of the local values and cache when recursive for object property
                _all = set.union(p_now, p_cache)
                if flag_collect and issubclass(tp, ObjectProperty):
                    for d in _all:
                        d_py = d if isinstance(d, BaseClass) else KnowledgeGraph.get_object_from_lookup(d)
                        # only collect the diff if the object exists in the memory, otherwise it's not necessary
                        if d_py is not None:
                            g_to_remove, g_to_add = d_py.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth, traversed_iris)

            elif f == 'rdfs_comment':
                if self._latest_cache.get(f) != self.rdfs_comment:
                    if self._latest_cache.get(f) is not None:
                        g_to_remove.add((URIRef(self.instance_iri), RDFS.comment, Literal(self._latest_cache.get(f))))
                    if self.rdfs_comment is not None:
                        g_to_add.add((URIRef(self.instance_iri), RDFS.comment, Literal(self.rdfs_comment)))

            elif f == 'rdfs_label':
                if self._latest_cache.get(f) != self.rdfs_label:
                    if self._latest_cache.get(f) is not None:
                        g_to_remove.add((URIRef(self.instance_iri), RDFS.label, Literal(self._latest_cache.get(f))))
                    if self.rdfs_label is not None:
                        g_to_add.add((URIRef(self.instance_iri), RDFS.label, Literal(self.rdfs_label)))

        if not self._exist_in_kg:
            g_to_add.add((URIRef(self.instance_iri), RDF.type, URIRef(self.rdf_type)))
            # assume that the instance is in KG once the triples are added
            # TODO [future] or need to a better way to represent this?
            self._exist_in_kg = True

        return g_to_remove, g_to_add

    def graph(self, g: Graph = None) -> Graph:
        """
        This method adds all the outgoing triples of the calling object.

        Args:
            g (Graph, optional): The rdflib.Graph object to which the triples should be added

        Returns:
            Graph: The rdflib.Graph object containing the triples added
        """
        if g is None:
            g = Graph()
        print(self.model_fields)
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.rdf_type)))
        for f, field_info in self.model_fields.items():
            tp = get_args(field_info.annotation)[0] if type(field_info.annotation) == _UnionGenericAlias else field_info.annotation
            if ObjectProperty._is_inherited(tp):
                tp: ObjectProperty
                prop = getattr(self, f)
                for o in prop:
                    g.add((URIRef(self.instance_iri), URIRef(tp.predicate_iri), URIRef(o.instance_iri if isinstance(o, BaseClass) else o)))
            elif DatatypeProperty._is_inherited(tp):
                tp: DatatypeProperty
                prop = getattr(self, f)
                for o in prop:
                    g.add((URIRef(self.instance_iri), URIRef(tp.predicate_iri), Literal(o)))
            elif f == 'rdfs_comment' and self.rdfs_comment is not None:
                    g.add((URIRef(self.instance_iri), RDFS.comment, Literal(self.rdfs_comment)))
            elif f == 'rdfs_label' and self.rdfs_label is not None:
                    g.add((URIRef(self.instance_iri), RDFS.label, Literal(self.rdfs_label)))
        return g

    def triples(self):
        """
        This method generates the turtle representation for all outgoing triples of the calling object.

        Returns:
            str: The outgoing triples in turtle format
        """
        return self.graph().serialize(format='ttl')

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


class ObjectProperty(_BaseProperty):
    """
    Base class for object properties.
    It inherits the BaseProperty class.
    """

    @classmethod
    def __init_subclass__(cls, **kwargs):
    # def __pydantic_init_subclass__(cls, **kwargs):
        # ensure that the cls already has field rdfs_isDefinedBy
        if cls.rdfs_isDefinedBy is None:
            raise AttributeError(f"Did you forget to specify `rdfs_isDefinedBy` for your object property {cls}?")

        # set the predicate_iri
        cls.predicate_iri = f'{cls.rdfs_isDefinedBy.base_url}/{cls.__name__}'

        # TODO remove below
        # # add the postponed domains
        # to_add_as_domain = cls.rdfs_isDefinedBy.retrieve_postponed_property_domain(cls.__name__)
        # print(cls.__name__)
        # for c in to_add_as_domain:
        #     cls.add_to_rdfs_domain(c)
        # to_add_as_range = cls.rdfs_isDefinedBy.retrieve_postponed_property_range(cls.__name__)
        # for r in to_add_as_range:
        #     if r in cls.rdfs_isDefinedBy.class_lookup_name_as_key:
        #         cls.add_to_rdfs_range(cls.rdfs_isDefinedBy.class_lookup_name_as_key[r])
        #     else:
        #         if 'rdfs_range' not in cls.rdfs_isDefinedBy.forward_refs_key_predicate[cls.__name__]:
        #             cls.rdfs_isDefinedBy.forward_refs_key_predicate[cls.__name__]['rdfs_range'] = set()
        #         cls.rdfs_isDefinedBy.forward_refs_key_predicate[cls.__name__]['rdfs_range'].add(r)

        # register the class to the ontology
        cls.rdfs_isDefinedBy.register_object_property(cls)

    # def _collect_diff(
    #     self,
    #     o: Any,
    #     g_to_remove: Graph,
    #     g_to_add: Graph,
    #     flag_collect: bool,
    #     recursive_depth: int = 0,
    #     traversed_iris: set = None,
    # ):
    #     if flag_collect:
    #         if isinstance(o, BaseClass):
    #             o_iri = o.instance_iri
    #             g_to_remove, g_to_add = o.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth, traversed_iris)
    #         elif isinstance(o, str):
    #             o_iri = o
    #             o_py = KnowledgeGraph.get_object_from_lookup(o)
    #             # only collect the diff if the object exists in the memory, otherwise it's not necessary
    #             if o_py is not None:
    #                 g_to_remove, g_to_add = o_py.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth, traversed_iris)
    #         else:
    #             raise TypeError(f"Type of {o} is not supported for range of {self}.")
    #     else:
    #         o_iri = o.instance_iri if isinstance(o, BaseClass) else o
    #     return g_to_remove, g_to_add, o_iri

    # def collect_range_diff_to_graph(
    #     self,
    #     subject: str,
    #     cache: ObjectProperty,
    #     g_to_remove: Graph,
    #     g_to_add: Graph,
    #     recursive_depth: int = 0,
    #     traversed_iris: set = None,
    # ) -> Tuple[Graph, Graph]:
    #     """
    #     This function collects the differences between the latest cache and the current instance of the calling object.
    #     The recursion stops when the IRI is traversed already, the logic to determine this is at the BaseClass side.

    #     Args:
    #         subject (str): The subject of the property when adding/removing triples
    #         cache (ObjectProperty): The cache of the property to compare with
    #         g_to_remove (Graph): The rdflib.Graph object to which the triples to be removed will be added
    #         g_to_add (Graph): The rdflib.Graph object to which the triples to be added will be added
    #         recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion
    #         traversed_iris (set): A set of IRIs that were already traversed in recursion

    #     Returns:
    #         Tuple[Graph, Graph]: A tuple of two rdflib.Graph objects containing the triples to be removed and added
    #     """
    #     # behaviour of recursive_depth: 0 means no recursion, -1 means infinite recursion, n means n-level recursion
    #     flag_collect = abs(recursive_depth) > 0
    #     recursive_depth = max(recursive_depth - 1, 0) if recursive_depth > -1 else max(recursive_depth - 1, -1)

    #     # TODO optimise the below codes
    #     # compare the range and its cache to find out what to remove and what to add
    #     # NOTE this supports the default_factory that initialise the object with empty input values for those properties with minimum cardinality 1
    #     cached_range = cache.range if cache is not None else set()
    #     diff_to_remove = cached_range - self.range
    #     diff_to_add = self.range - cached_range

    #     # iterate the differences and add them to the graph
    #     for o in diff_to_add:
    #         g_to_remove, g_to_add, o_iri = self._collect_diff(o, g_to_remove, g_to_add, flag_collect, recursive_depth, traversed_iris)
    #         g_to_add.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o_iri)))

    #     for o in diff_to_remove:
    #         g_to_remove, g_to_add, o_iri = self._collect_diff(o, g_to_remove, g_to_add, flag_collect, recursive_depth, traversed_iris)
    #         g_to_remove.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o_iri)))

    #     # besides the differences between the range and its cache
    #     # also need to consider the intersection of the range and its cache when recursive
    #     for o in self.range.intersection(cached_range):
    #         g_to_remove, g_to_add, o_iri = self._collect_diff(o, g_to_remove, g_to_add, flag_collect, recursive_depth, traversed_iris)

    #     return g_to_remove, g_to_add

    @classmethod
    def export_to_owl(cls, g: Graph, rdfs_domain: set, rdfs_range: set) -> Graph:
        """
        This function exports the triples of the object property to an OWL ontology.
        It calls the super class function with the flag 'is_object_property' set to True.

        Args:
            g (Graph): The rdflib.Graph object to which the triples will be added

        Returns:
            Graph: The rdflib.Graph object with the added triples
        """
        return super().export_to_owl(g, rdfs_domain, rdfs_range, True)

    # @classmethod
    # def reveal_object_property_range(cls) -> T:
    #     """
    #     This function reveals the Pydantic class of the range of the object property.

    #     Returns:
    #         T: The Pydantic class of the range of the object property
    #     """
    #     return cls.model_fields['range'].annotation.__args__[0].__args__[0]

    # @classmethod
    # def reveal_possible_property_range(cls) -> Set[T]:
    #     """
    #     This function reveals the possible range of the object property.

    #     Returns:
    #         Set[T]: The set of possible range of the property
    #     """
    #     return cls.model_fields['range'].annotation.__args__[0].__args__

    # @classmethod
    # def reveal_property_range_iri(cls) -> str:
    #     """
    #     This function reveals the IRI of the range of the object property.

    #     Returns:
    #         str: IRI of the range of the object property
    #     """
    #     return cls.reveal_object_property_range().rdf_type

    # def create_cache(self, recursive_depth: int = 0, traversed_iris: set = None) -> ObjectProperty:
    #     """
    #     This function creates a cache of the object property.
    #     The recursion stops when the IRI is traversed already, the logic to determine this is at the BaseClass side.

    #     Args:
    #         recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion
    #         traversed_iris (set): A set of IRIs that were already traversed in recursion

    #     Returns:
    #         ObjectProperty: The cache of the object property
    #     """
    #     recursive_depth = max(recursive_depth - 1, 0) if recursive_depth > -1 else max(recursive_depth - 1, -1)
    #     for o in self.range:
    #         if isinstance(o, BaseClass):
    #             # this function will be useful when pushing a brand new (nested) object to knowledge graph
    #             # so that the cache of those objects appeared at deeper recursive_depth are also updated
    #             o.create_cache(recursive_depth, traversed_iris)
    #         elif isinstance(o, str):
    #             obj = KnowledgeGraph.get_object_from_lookup(o)
    #             if obj is not None:
    #                 obj.create_cache(recursive_depth, traversed_iris)
    #         else:
    #             raise Exception(f"Unsupported datatype {type(o)} for range of object property {self}")
    #     # return the cache that will actually be used for comparison when pulling/pushing
    #     return self.__class__(set([
    #         o.instance_iri if isinstance(o, BaseClass) else o for o in self.range
    #     ]))

    @classmethod
    def add_to_graph(cls, g: Graph, s: str, o: Any) -> Graph:
        if isinstance(o, BaseClass):
            o_iri = o.instance_iri
        elif isinstance(o, str):
            o_iri = o
        g.add((URIRef(s), URIRef(cls.predicate_iri), URIRef(o_iri)))
        return g


class TransitiveProperty(ObjectProperty):
    """
    Base class for transitive object properties.
    It inherits the ObjectProperty class.
    """
    rdfs_isDefinedBy = Owl

    def obtain_transitive_objects(self, transitive_objects: set=None):
        if transitive_objects is None:
            transitive_objects = set()
        for o in self.range:
            if isinstance(o, str):
                o = KnowledgeGraph.get_object_from_lookup(o)
            prop = o.get_object_property_by_iri(self.predicate_iri)
            transitive_objects.add(o)
            if prop is not None:
                transitive_objects = prop.obtain_transitive_objects(transitive_objects)
        return transitive_objects


class DatatypeProperty(_BaseProperty):
    """
    Base class for data properties.
    It inherits the BaseProperty class.
    """

    @classmethod
    def __init_subclass__(cls, **kwargs):
    # def __pydantic_init_subclass__(cls, **kwargs):
        # ensure that the cls already has field rdfs_isDefinedBy
        if cls.rdfs_isDefinedBy is None:
            raise AttributeError(f"Did you forget to specify `rdfs_isDefinedBy` for your data property {cls}?")

        # ensure that the field predicate_iri is set
        if cls.predicate_iri is None:
            cls.predicate_iri = f'{cls.rdfs_isDefinedBy.base_url}/{cls.__name__}'

        # TODO remove below
        # # add the postponed domains
        # to_add_as_domain = cls.rdfs_isDefinedBy.retrieve_postponed_property_domain(cls.__name__)
        # for c in to_add_as_domain:
        #     cls.add_to_rdfs_domain(c)

        # register the class to the ontology
        cls.rdfs_isDefinedBy.register_data_property(cls)

    # def collect_range_diff_to_graph(
    #     self,
    #     subject: str,
    #     cache: DatatypeProperty,
    #     g_to_remove: Graph,
    #     g_to_add: Graph,
    #     recursive_depth: int = 0,
    #     traversed_iris: set = None,
    # ) -> Tuple[Graph, Graph]:
    #     """
    #     This function collects the differences between the latest cache and the current instance of the calling object.
    #     The recursion stops when the IRI is traversed already, the logic to determine this is at the BaseClass side.

    #     Args:
    #         subject (str): The subject of the property when adding/removing triples
    #         cache (DatatypeProperty): The cache of the property to compare with
    #         g_to_remove (Graph): The rdflib.Graph object to which the triples to be removed will be added
    #         g_to_add (Graph): The rdflib.Graph object to which the triples will be added
    #         recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion
    #             > this parameter is not used in this function, but it is kept for compatibility with the method in the parent class BaseProperty
    #         traversed_iris (set): A set of IRIs that were already traversed in recursion

    #     Returns:
    #         Tuple[Graph, Graph]: A tuple of two rdflib.Graph objects containing the triples to be removed and added
    #     """
    #     # create an empty set for the cache.range if the cache is None
    #     # NOTE this supports the default_factory that initialise the object with empty input values for those properties with minimum cardinality 1
    #     cached_range = cache.range if cache is not None else set()

    #     # compare the range and its cache to find out what to remove and what to add
    #     diff_to_remove = cached_range - self.range
    #     for d in diff_to_remove:
    #         self.add_property_to_graph(subject, d, g_to_remove)

    #     diff_to_add = self.range - cached_range
    #     # iterate the differences and add them to the graph
    #     for d in diff_to_add:
    #         self.add_property_to_graph(subject, d, g_to_add)

    #     return g_to_remove, g_to_add

    # def add_property_to_graph(self, subject: str, object: Any, g: Graph) -> Graph:
    #     """
    #     This function adds a data property to the graph.

    #     Args:
    #         subject (str): The subject of the triple
    #         object (Any): The object of the triple
    #         g (Graph): The rdflib.Graph object to which the triple will be added

    #     Raises:
    #         TypeError: The type of the object is not supported by rdflib as a data property

    #     Returns:
    #         Graph: The rdflib.Graph object with the added triple
    #     """
    #     try:
    #         g.add((URIRef(subject), URIRef(self.predicate_iri), Literal(object)))
    #     except Exception as e:
    #         raise TypeError(
    #             f"Type of {object} ({type(object)}) is not supported by rdflib as a data property for {self.predicate_iri}.", e)
    #     return g

    @classmethod
    def export_to_owl(cls, g: Graph, rdfs_domain: set, rdfs_range: set) -> Graph:
        """
        This function exports the triples of the data property to an OWL ontology.
        It calls the super class function with the flag 'is_object_property' set to False.

        Args:
            g (Graph): The rdflib.Graph object to which the triples will be added

        Returns:
            Graph: The rdflib.Graph object with the added triples
        """
        return super().export_to_owl(g, rdfs_domain, rdfs_range, False)

    # @classmethod
    # def reveal_data_property_range(cls) -> T:
    #     """
    #     This function reveals the range of the data property.

    #     Returns:
    #         T: The range of the data property
    #     """
    #     print('hahs')
    #     return cls.model_fields['range'].annotation.__args__[0]

    # @classmethod
    # def reveal_possible_property_range(cls) -> Set[T]:
    #     """
    #     This function reveals the possible range of the object property.

    #     Returns:
    #         Set[T]: The set of possible range of the property
    #     """
    #     return cls.model_fields['range'].annotation.__args__

    # @classmethod
    # def reveal_property_range_iri(cls) -> str:
    #     """
    #     This function reveals the IRI of the range of the data property.

    #     Returns:
    #         str: IRI of the range of the data property
    #     """
    #     return _castPythonToXSD(cls.reveal_data_property_range())

    # def create_cache(self, recursive_depth: int = 0, traversed_iris: set = None) -> DatatypeProperty:
    #     """
    #     This function creates a cache of the data property.
    #     The recursion stops when the IRI is traversed already, the logic to determine this is at the BaseClass side.

    #     Args:
    #         recursive_depth (int): The depth of the recursion, 0 means no recursion, -1 means infinite recursion, n means n-level recursion
    #         traversed_iris (set): A set of IRIs that were already traversed in recursion

    #     Returns:
    #         DatatypeProperty: The cache of the data property
    #     """
    #     return self.__class__(set(copy.deepcopy(self.range)))

    @classmethod
    def add_to_graph(cls, g: Graph, s: str, o: Any) -> Graph:
        try:
            g.add((URIRef(s), URIRef(cls.predicate_iri), Literal(o)))
        except Exception as e:
            raise TypeError(f'Type of {o} ({type(o)}) is not supported by rdflib as a data property for {cls.predicate_iri}.', e)
        return g
