from __future__ import annotations

from typing import Any, Dict, List, Set, Tuple, Union, TypeVar
from typing_extensions import Annotated
from annotated_types import Len

from pydantic import BaseModel, Field, PrivateAttr, model_validator
from rdflib import Graph, URIRef, Literal
from rdflib.namespace import RDF, RDFS

import hashlib
import base64
import copy
import time

from py4jps.data_model.utils import construct_rdf_type, init_instance_iri
from py4jps.data_model.iris import TWA_BASE_PREFIX
from py4jps.kg_operations import PySparqlClient


T = TypeVar('T')


GLOBAL_OBJECT_LOOKUP = {}


def get_ontology_object_from_lookup(iri: str) -> Union[BaseOntology, None]:
    global GLOBAL_OBJECT_LOOKUP
    return GLOBAL_OBJECT_LOOKUP.get(iri, None)


def set_new_ontology_object(iri: str, obj: BaseOntology):
    global GLOBAL_OBJECT_LOOKUP
    # NOTE this is to make sure the object is not overwritten
    if iri not in GLOBAL_OBJECT_LOOKUP:
        GLOBAL_OBJECT_LOOKUP[iri] = obj


def clear_ontology_object_lookup():
    global GLOBAL_OBJECT_LOOKUP
    GLOBAL_OBJECT_LOOKUP = {}


def create_cache(attr_value):
    if isinstance(attr_value, ObjectProperty):
        return attr_value.__class__(range=set([
            o.instance_iri if isinstance(o, BaseOntology) else o for o in attr_value.range
        ]))
    elif isinstance(attr_value, DataProperty):
        return attr_value.__class__(range=set(copy.deepcopy(attr_value.range)))
    else:
        return attr_value


def as_range_of_object_property(t: T, min_cardinality: int = 0, max_cardinality: int = None) -> Set[Union[T, str]]:
    return Annotated[Set[Union[t, str]], Len(min_cardinality, max_cardinality)]


def as_range_of_data_property(t: T, min_cardinality: int = 0, max_cardinality: int = None) -> Set[T]:
    return Annotated[Set[t], Len(min_cardinality, max_cardinality)]


def reveal_object_property_range(t: Set[Union[T, str]]) -> T:
    return t.__args__[0].__args__[0]


class BaseProperty(BaseModel, validate_assignment=True):
    # NOTE validate_assignment=True is to make sure the validation is triggered when range is updated

    base_prefix: str = Field(default=TWA_BASE_PREFIX, frozen=True)
    namespace: str = Field(default=None, frozen=True)
    predicate_iri: str = Field(default=None)
    # setting default_factory to set is safe here, i.e. it won't be shared between instances
    # see https://docs.pydantic.dev/latest/concepts/models/#fields-with-non-hashable-default-values
    range: Set = Field(default_factory=set)

    # TODO [future] vanilla set operations don't trigger the validation as of pydantic 2.6.1
    # it also seems this will not be supported in the near future
    # see https://github.com/pydantic/pydantic/issues/496
    # for a workaround, see https://github.com/pydantic/pydantic/issues/8575
    # and https://gist.github.com/geospackle/8f317fc19469b1e216edee3cc0f1c898

    def __init__(self, **data) -> None:
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
        if not bool(self.predicate_iri):
            self.predicate_iri = self.__class__.get_predicate_iri()
        return self

    @classmethod
    def get_predicate_iri(cls) -> str:
        return construct_rdf_type(cls.model_fields['base_prefix'].default, cls.model_fields['namespace'].default, cls.__name__)

    def collect_range_diff_to_graph(self, subject: str, g: Graph, to_add_range: bool = False, to_remove_range: bool = False):
        raise NotImplementedError("This is an abstract method.")

    def _exclude_keys_for_compare_(self, *keys_to_exclude):
        list_keys_to_exclude = list(keys_to_exclude) if not isinstance(
            keys_to_exclude, list) else keys_to_exclude
        list_keys_to_exclude.append('instance_iri')
        list_keys_to_exclude.append('rdfs_comment')
        list_keys_to_exclude.append('base_prefix')
        list_keys_to_exclude.append('namespace')
        return set(tuple(list_keys_to_exclude))


class BaseOntology(BaseModel, validate_assignment=True):
    """
    Example:
    class MyClass(BaseOntology):
        myObjectProperty: MyObjectProperty
        myDataProperty: MyDataProperty
    """

    # TODO [future] think about how to make it easier to instantiate an instance by hand, especially the object properties (or should it be fully automated?)

    # NOTE validate_assignment=True is to make sure the validation is triggered when range is updated

    # The initialisation and validator sequence:
    # (I) start to run BaseOntology.__init__(__pydantic_self__, **data) with **data as the raw input arguments;
    # (II) run until super().__init__(**data), note data is updated within BaseOntology before sending to super().init(**data);
    # (III) now within BaseModel __init__:
    #     (i) run root_validator (for those pre=True), in order of how the root_validators are listed in codes;
    #     (ii) in order of how the fields are listed in codes:
    #         (1) run validator (for those pre=True) in order of how the validators (for the same field) are listed in codes;
    #         (2) run validator (for those pre=False) in order of how the validators (for the same field) are listed in codes;
    #     (iii) (if we are instantiating a child class of BaseOntology) load default values in the child class (if they are provided)
    #             and run root_validator (for those pre=False) in order of how the root_validators are listed in codes,
    #             e.g. clz='clz provided in the child class' will be added to 'values' of the input argument of root_validator;
    # (IV) end BaseModel __init__;
    # (V) end BaseOntology __init__

    base_prefix: str = Field(default=TWA_BASE_PREFIX, frozen=True)
    namespace: str = Field(default=None, frozen=True)
    rdfs_comment: str = Field(default=None)
    rdf_type: str = Field(default=None)
    instance_iri: str = Field(default=None)
    _timestamp_of_latest_cache: float = PrivateAttr(default_factory=time.time)
    # format of the cache for all properties: {property_name: property_object}
    _latest_cache: Dict[str, Any] = PrivateAttr(default_factory=dict)
    _exist_in_kg: bool = PrivateAttr(default=False)

    @model_validator(mode='after')
    def set_rdf_type(self):
        if not bool(self.rdf_type):
            self.rdf_type = self.__class__.get_rdf_type()
        if not bool(self.instance_iri):
            self.instance_iri = init_instance_iri(
                self.base_prefix, self.__class__.__name__)
        set_new_ontology_object(self.instance_iri, self)
        return self

    @classmethod
    def get_rdf_type(cls) -> str:
        return construct_rdf_type(cls.model_fields['base_prefix'].default, cls.model_fields['namespace'].default, cls.__name__)

    @classmethod
    def pull_from_kg(cls, iris: List[str], sparql_client: PySparqlClient, recursive_depth: int = 0) -> List[BaseOntology]:
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
            inst = get_ontology_object_from_lookup(iri)
            # handle object properties (where the recursion happens)
            # TODO need to consider what to do when two instances pointing to each other
            object_properties_dict = {
                op_dct['field']: op_dct['type'](
                    range=set() if op_iri not in props else reveal_object_property_range(
                        op_dct['type'].model_fields['range'].annotation
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
                        reveal_object_property_range(
                            op_dct['type'].model_fields['range'].annotation
                        ).pull_from_kg(
                            set(inst.get_object_property_range_iris(op_dct['field'])) - set(props.get(op_iri, [])),
                            sparql_client, recursive_depth)
                inst._latest_cache = {k: create_cache(v) for k, v in {**object_properties_dict, **data_properties_dict}.items()}
                inst._timestamp_of_latest_cache = time.time()
            else:
                inst = cls(
                    instance_iri=iri,
                    **object_properties_dict,
                    **data_properties_dict,
                )
                inst._latest_cache = {f: create_cache(getattr(inst, f)) for f in inst.model_fields}
                inst._timestamp_of_latest_cache = time.time()

            inst._exist_in_kg = True
            # update cache here
            instance_lst.append(inst)
            # set new instance to the global look up table, so that we can avoid creating the same instance multiple times
            # also, existing objects will be skipped as the modification should already be done to the objects themselves
            set_new_ontology_object(iri, inst)
        # TODO add check for rdf_type
        return instance_lst

    @classmethod
    def pull_all_instances_from_kg(cls, sparql_client: PySparqlClient, recursive_depth: int = 0) -> Set[BaseOntology]:
        iris = sparql_client.get_all_instances_of_class(cls.get_rdf_type())
        return cls.pull_from_kg(iris, sparql_client, recursive_depth)

    @classmethod
    def get_object_properties(cls):
        # return {predicate_iri: {'field': field_name, 'type': field_clz}}
        # e.g. {'https://twa.com/myObjectProperty': {'field': 'myObjectProperty', 'type': MyObjectProperty}}
        return {
            field_info.annotation.get_predicate_iri(): {
                'field': f, 'type': field_info.annotation
            } for f, field_info in cls.model_fields.items() if issubclass(field_info.annotation, ObjectProperty)
        }

    @classmethod
    def get_data_properties(cls):
        # return {predicate_iri: {'field': field_name, 'type': field_clz}}
        # e.g. {'https://twa.com/myDataProperty': {'field': 'myDataProperty', 'type': MyDataProperty}}
        return {
            field_info.annotation.get_predicate_iri(): {
                'field': f, 'type': field_info.annotation
            } for f, field_info in cls.model_fields.items() if issubclass(field_info.annotation, DataProperty)
        }

    def get_object_property_range_iris(self, field_name: str):
        return [o.instance_iri if isinstance(o, BaseOntology) else o for o in getattr(self, field_name).range]

    def delete_in_kg(self, sparql_client: PySparqlClient):
        # TODO implement this method
        raise NotImplementedError

    def push_to_kg(self, sparql_client: PySparqlClient, recursive_depth: int = 0):
        # type of changes: remove old triples, add new triples
        g_to_remove = Graph()
        g_to_add = Graph()
        g_to_remove, g_to_add = self.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)
        # TODO [future] what happens when KG changed during processing in the python side? race conditions...
        sparql_client.delete_and_insert_graphs(g_to_remove, g_to_add)
        return g_to_remove, g_to_add

    def collect_diff_to_graph(self, g_to_remove: Graph, g_to_add: Graph, recursive_depth: int = 0) -> Tuple[Graph, Graph]:
        for f, field_info in self.model_fields.items():
            if issubclass(field_info.annotation, BaseProperty):
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
        list_keys_to_exclude.append('base_prefix')
        list_keys_to_exclude.append('namespace')
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

    def collect_range_diff_to_graph(
        self,
        subject: str,
        cache: ObjectProperty,
        g_to_remove: Graph,
        g_to_add: Graph,
        recursive_depth: int = 0
    ) -> Tuple[Graph, Graph]:
        # behaviour of recursive_depth: 0 means no recursion, -1 means infinite recursion, n means n-level recursion
        flag_collect = abs(recursive_depth) > 0
        recursive_depth = max(recursive_depth - 1, 0) if recursive_depth > -1 else max(recursive_depth - 1, -1)

        # TODO optimise the below codes
        # compare the range and its cache to find out what to remove and what to add
        diff_to_remove = cache.range - self.range
        diff_to_add = self.range - cache.range

        # iterate the differences and add them to the graph
        for o in diff_to_add:
            if isinstance(o, BaseOntology):
                g_to_add.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o.instance_iri)))
                if flag_collect:
                    g_to_remove, g_to_add = o.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)
            elif isinstance(o, str):
                g_to_add.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o)))
                if flag_collect:
                    o_py = get_ontology_object_from_lookup(o)
                    # only collect the diff if the object exists in the memory, otherwise it's not necessary
                    if o_py is not None:
                        g_to_remove, g_to_add = o_py.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)
            else:
                raise TypeError(f"Type of {o} is not supported for range of {self}.")

        for o in diff_to_remove:
            if isinstance(o, BaseOntology):
                g_to_remove.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o.instance_iri)))
                if flag_collect:
                    g_to_remove, g_to_add = o.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)
            elif isinstance(o, str):
                g_to_remove.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o)))
                if flag_collect:
                    o_py = get_ontology_object_from_lookup(o)
                    # only collect the diff if the object exists in the memory, otherwise it's not necessary
                    if o_py is not None:
                        g_to_remove, g_to_add = o_py.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)
            else:
                raise TypeError(f"Type of {o} is not supported for range of {self}.")

        # besides the differences between the range and its cache
        # also need to consider the intersection of the range and its cache when recursive
        if flag_collect:
            for o in self.range.intersection(cache.range):
                if isinstance(o, BaseOntology):
                    g_to_remove, g_to_add = o.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)
                elif isinstance(o, str):
                    o_py = get_ontology_object_from_lookup(o)
                    # only collect the diff if the object exists in the memory, otherwise it's not necessary
                    if o_py is not None:
                        g_to_remove, g_to_add = o_py.collect_diff_to_graph(g_to_remove, g_to_add, recursive_depth)
                else:
                    raise TypeError(f"Type of {o} is not supported for range of {self}.")

        return g_to_remove, g_to_add


class DataProperty(BaseProperty):

    def collect_range_diff_to_graph(
        self,
        subject: str,
        cache: DataProperty,
        g_to_remove: Graph,
        g_to_add: Graph,
        recursive_depth: int = 0,
    ) -> Tuple[Graph, Graph]:
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
        try:
            g.add((URIRef(subject), URIRef(self.predicate_iri), Literal(object)))
        except Exception as e:
            raise TypeError(
                f"Type of {object} ({type(object)}) is not supported by rdflib as a data property for {self.predicate_iri}.", e)
        return g
