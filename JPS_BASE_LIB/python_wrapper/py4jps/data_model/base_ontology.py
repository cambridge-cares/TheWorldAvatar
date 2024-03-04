from __future__ import annotations

from typing import Any, List, Union, TypeVar
from typing_extensions import Annotated
from annotated_types import Len

from pydantic import BaseModel, Field, model_validator
from rdflib import Graph, URIRef, Literal
from rdflib.namespace import RDF, RDFS

import hashlib
import base64

from py4jps.data_model.utils import construct_rdf_type, init_instance_iri
from py4jps.data_model.iris import TWA_BASE_PREFIX
from py4jps.kg_operations import PySparqlClient


T = TypeVar('T')


def as_range_of_object_property(t: T, min_cardinality: int = 0, max_cardinality: int = None) -> List[Union[T, str]]:
    return Annotated[List[Union[t, str]], Len(min_cardinality, max_cardinality)]


def as_range_of_data_property(t: T) -> List[T]:
    # NOTE the cardinality for data property is to have at most one value
    # TODO add support for multiple values
    return Annotated[List[t], Len(0, 1)]


def reveal_object_property_range(t: List[Union[T, str]]) -> T:
    return t.__args__[0].__args__[0]


class BaseProperty(BaseModel):
    base_prefix: str = Field(default=TWA_BASE_PREFIX, frozen=True)
    namespace: str = Field(default=None, frozen=True)
    predicate_iri: str = Field(default=None)
    range: Any

    def __init__(self, **data) -> None:
        # TODO validate range is either str or specific type
        # below code is to make sure range is always a list
        if 'range' in data and not isinstance(data['range'], list):
            data['range'] = [data['range']]
        super().__init__(**data)

    @model_validator(mode='after')
    def set_predicate_iri(self):
        if not bool(self.predicate_iri):
            self.predicate_iri = self.__class__.get_predicate_iri()
        return self

    @classmethod
    def get_predicate_iri(cls) -> str:
        return construct_rdf_type(cls.model_fields['base_prefix'].default, cls.model_fields['namespace'].default, cls.__name__)

    def add_property_to_graph(self, subject: str, g: Graph):
        raise NotImplementedError("This is an abstract method.")

    def _exclude_keys_for_compare_(self, *keys_to_exclude):
        list_keys_to_exclude = list(keys_to_exclude) if not isinstance(
            keys_to_exclude, list) else keys_to_exclude
        list_keys_to_exclude.append('instance_iri')
        list_keys_to_exclude.append('rdfs_comment')
        list_keys_to_exclude.append('base_prefix')
        list_keys_to_exclude.append('namespace')
        return set(tuple(list_keys_to_exclude))


class BaseOntology(BaseModel):
    # TODO think about how to make it easier to instantiate an instance by hand, especially the object properties (or should it be fully automated?)
    # two directions:
    # 1. firstly, from instance to triples
    #    - [done] store all object properties as pydantic objects
    #    - [done] store all data properties as pydantic objects
    #    - TODO updating existing instance in kg, i.e., consider cache
    # 2. from kg triples to instance
    #    - [done] pull single instance (node) from kg given iri
    #    - [done] arbitary recursive depth when pull triples from kg?
    #    - TODO pull all instances of a class from kg
    """The initialisation and validator sequence:
        (I) start to run BaseOntology.__init__(__pydantic_self__, **data) with **data as the raw input arguments;
        (II) run until super().__init__(**data), note data is updated within BaseOntology before sending to super().init(**data);
        (III) now within BaseModel __init__:
            (i) run root_validator (for those pre=True), in order of how the root_validators are listed in codes;
            (ii) in order of how the fields are listed in codes:
                (1) run validator (for those pre=True) in order of how the validators (for the same field) are listed in codes;
                (2) run validator (for those pre=False) in order of how the validators (for the same field) are listed in codes;
            (iii) (if we are instantiating a child class of BaseOntology) load default values in the child class (if they are provided)
                  and run root_validator (for those pre=False) in order of how the root_validators are listed in codes,
                  e.g. clz='clz provided in the child class' will be added to 'values' of the input argument of root_validator;
        (IV) end BaseModel __init__;
        (V) end BaseOntology __init__

    Example:
    class MyClass(BaseOntology):
        myObjectProperty: MyObjectProperty
        myDataProperty: MyDataProperty
    """
    base_prefix: str = Field(default=TWA_BASE_PREFIX, frozen=True)
    namespace: str = Field(default=None, frozen=True)
    rdfs_comment: str = Field(default=None)
    rdf_type: str = Field(default=None)
    instance_iri: str = Field(default=None)

    @model_validator(mode='after')
    def set_rdf_type(self):
        if not bool(self.rdf_type):
            self.rdf_type = self.__class__.get_rdf_type()
        if not bool(self.instance_iri):
            self.instance_iri = init_instance_iri(
                self.base_prefix, self.__class__.__name__)
        return self

    @classmethod
    def get_rdf_type(cls) -> str:
        return construct_rdf_type(cls.model_fields['base_prefix'].default, cls.model_fields['namespace'].default, cls.__name__)

    def push_to_kg(self, sparql_client: PySparqlClient):
        """This method is for pushing new instances to the KG, or updating existing instance."""
        g = Graph()
        self.create_triples_for_kg(g)
        sparql_client.upload_graph(g)

    @classmethod
    def pull_from_kg(cls, iris: List[str], sparql_client: PySparqlClient, recursive_depth: int = 0) -> List[BaseOntology]:
        # behaviour of recursive_depth: 0 means no recursion, -1 means infinite recursion, n means n-level recursion
        pull_flag = abs(recursive_depth) > 0
        recursive_depth = max(recursive_depth - 1, 0) if recursive_depth > -1 else max(recursive_depth - 1, -1)
        # TODO what do we do with undefined properties in python class? - write a warning message or we can add them to extra_fields https://docs.pydantic.dev/latest/concepts/models/#extra-fields
        if isinstance(iris, str):
            iris = [iris]
        # return format: {iri: {predicate: [object]}}
        node_dct = sparql_client.get_outgoing_and_attributes(iris)
        instance_lst = []
        # TODO optimise the time complexity of the following code when the number of instances is large
        ops = cls.get_object_properties()
        dps = cls.get_data_properties()
        for iri, props in node_dct.items():
            instance_lst.append(
                cls(
                    instance_iri=iri,
                    # Handle object properties (where the recursion happens)
                    # TODO need to consider what to do when two instances pointing to each other
                    **{
                        op_dct['field']: op_dct['type'](
                            range=reveal_object_property_range(
                                op_dct['type'].model_fields['range'].annotation
                            ).pull_from_kg(props[op_iri], sparql_client, recursive_depth) if pull_flag else props[op_iri]
                        ) if op_iri in props else None for op_iri, op_dct in ops.items()
                    },
                    # Here we handle data properties
                    **{
                        dp_dct['field']: dp_dct['type'](
                            range=props[dp_iri]
                        ) if dp_iri in props else None for dp_iri, dp_dct in dps.items()
                    }
                )
            )
        # TODO add check for rdf_type
        return instance_lst

    @classmethod
    def get_object_properties(cls):
        return {
            field_info.annotation.get_predicate_iri(): {
                'field': f, 'type': field_info.annotation
            } for f, field_info in cls.model_fields.items() if issubclass(field_info.annotation, ObjectProperty)
        }

    @classmethod
    def get_data_properties(cls):
        return {
            field_info.annotation.get_predicate_iri(): {
                'field': f, 'type': field_info.annotation
            } for f, field_info in cls.model_fields.items() if issubclass(field_info.annotation, DataProperty)
        }

    def create_triples_for_kg(self, g):
        """This method is for creating triples as rdflib.Graph() for the knowledge graph.
        By default, it will create triples for the instance itself.
        One can overwrite this method and provide additional triples should they wish.
        In which case, please remember to call the super.create_triples_for_kg(g),
        unless one wants to provide completely different triples."""
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.rdf_type)))
        if self.rdfs_comment:
            g.add((URIRef(self.instance_iri), RDFS.comment,
                  Literal(self.rdfs_comment)))
        for f, prop in iter(self):
            if f not in ['base_prefix', 'namespace', 'rdfs_comment', 'instance_iri', 'rdf_type'] and bool(prop):
                if isinstance(prop, BaseProperty) and bool(prop.range):
                    g = prop.add_property_to_graph(self.instance_iri, g)
                else:
                    raise TypeError(
                        f"Type of {prop} is not supported for field {f} when creating KG triples for instance {self.dict()}.")
        return g

    def _exclude_keys_for_compare_(self, *keys_to_exclude):
        list_keys_to_exclude = list(keys_to_exclude) if not isinstance(
            keys_to_exclude, list) else keys_to_exclude
        list_keys_to_exclude.append('instance_iri')
        list_keys_to_exclude.append('rdfs_comment')
        list_keys_to_exclude.append('base_prefix')
        list_keys_to_exclude.append('namespace')
        return set(tuple(list_keys_to_exclude))

    def __eq__(self, other: Any) -> bool:
        return self.__hash__() == other.__hash__()

    def __hash__(self):
        return self._make_hash_sha256_(self.dict(exclude=self._exclude_keys_for_compare_()))

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
            return tuple(sorted((k, self._make_hashable_(v)) for k, v in o.items()))

        if isinstance(o, (set, frozenset)):
            return tuple(sorted(self._make_hashable_(e) for e in o))

        return o


class ObjectProperty(BaseProperty):

    def add_property_to_graph(self, subject: str, g: Graph):
        # NOTE the range can be a list of BaseOntology or str
        # The code will only enter the below if range is not None
        op = self.range if isinstance(self.range, list) else [self.range]
        for o in op:
            if isinstance(o, BaseOntology):
                g.add((URIRef(subject), URIRef(
                    self.predicate_iri), URIRef(o.instance_iri)))
                # given that the KG only store triple once, we don't need to check if the triples already exist for object properties
                g = o.create_triples_for_kg(g)
            elif isinstance(o, str):
                g.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o)))
            else:
                raise TypeError(f"Type of {o} is not supported.")
        return g


class DataProperty(BaseProperty):

    def add_property_to_graph(self, subject: str, g: Graph):
        # TODO implement behaviour to overwrite the existing data property when pushing to KG
        dp = self.range if isinstance(self.range, list) else [self.range]
        for d in dp:
            try:
                g.add((URIRef(subject), URIRef(self.predicate_iri), Literal(d)))
            except Exception as e:
                raise TypeError(
                    f"Type of {d} ({type(d)}) is not supported by rdflib as a data property for {self.predicate_iri}.", e)
        return g
