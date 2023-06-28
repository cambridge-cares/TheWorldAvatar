from chemistry_and_robots.data_model.base_ontology import *
import pytest
from typing import List
from typing import Dict
import uuid

SIMPLETESTCLASS_CLZ = 'http://test_hash_eq/SimpleTestClass'
TESTCLASS_CLZ = 'http://test_hash_eq/TestClass'
NAMESPACE_FOR_INIT = 'http://test_hash_eq/namespace_placeholder/'
STR_TO_INCLUDE = 'This string is a placeholder for str_attr_to_include'
ANOTHER_STR_TO_INCLUDE = 'This is another string str_attr_to_include, which is different from the STR_TO_INCLUDE'
INT_TO_INCLUDE = 5
ANOTHER_INT_TO_INCLUDE = 10

ALL_INCLUDE = True
NOT_ALL_INCLUDE = False

class SimpleTestClass_AllInclude(BaseOntology):
    clz: str = SIMPLETESTCLASS_CLZ
    str_attr_to_include: str
    int_attr_to_include: int
    str_attr_to_exclude_if_not_all_include: str
    int_attr_to_exclude_if_not_all_include: int

class SimpleTestClass_NotAllInclude(SimpleTestClass_AllInclude):

    def _exclude_keys_for_compare_(self, *keys_to_exclude) -> Dict[str, Any]:
        return super()._exclude_keys_for_compare_('str_attr_to_exclude_if_not_all_include', 'int_attr_to_exclude_if_not_all_include', *keys_to_exclude)

class NestedTestClass_AllInclude(SimpleTestClass_AllInclude):
    clz: str = TESTCLASS_CLZ
    list_attr_to_include: List[SimpleTestClass_AllInclude] = None
    list_attr_to_exclude_if_not_all_include: List[SimpleTestClass_AllInclude] = None

class NestedTestClass_NotAllInclude(SimpleTestClass_NotAllInclude):
    clz: str = TESTCLASS_CLZ
    list_attr_to_include: List[SimpleTestClass_AllInclude] = None
    list_attr_to_exclude_if_not_all_include: List[SimpleTestClass_AllInclude] = None

    # NOTE here keys 'str_attr_to_exclude_if_not_all_include' and 'int_attr_to_exclude_if_not_all_include' are already excluded
    def _exclude_keys_for_compare_(self, *keys_to_exclude) -> Dict[str, Any]:
        return super()._exclude_keys_for_compare_('list_attr_to_exclude_if_not_all_include', *keys_to_exclude)

@pytest.mark.parametrize(
    "all_include,str_to_include_1,int_to_include_1,str_to_include_2,int_to_include_2,target_result",
    [
        (ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (NOT_ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (NOT_ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (NOT_ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (NOT_ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
    ],
)
def test_simple_test_class(all_include, str_to_include_1, int_to_include_1, str_to_include_2, int_to_include_2, target_result):
    simple_instance_1 = generate_simple_test_class_instance(all_include, str_to_include_1, int_to_include_1)
    simple_instance_2 = generate_simple_test_class_instance(all_include, str_to_include_2, int_to_include_2)
    assert (simple_instance_1 == simple_instance_2) == target_result

@pytest.mark.parametrize(
    "mode_,mode_simple,str_to_include_1,int_to_include_1,str_to_include_2,int_to_include_2,target_result",
    [
        (ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (ALL_INCLUDE, NOT_ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (ALL_INCLUDE, NOT_ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (ALL_INCLUDE, ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (ALL_INCLUDE, ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),

        (NOT_ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, NOT_ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, NOT_ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (NOT_ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (NOT_ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (NOT_ALL_INCLUDE, NOT_ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (NOT_ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, ALL_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, True),
        (NOT_ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (NOT_ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
        (NOT_ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, STR_TO_INCLUDE, INT_TO_INCLUDE, False),
        (NOT_ALL_INCLUDE, ALL_INCLUDE, STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, ANOTHER_STR_TO_INCLUDE, ANOTHER_INT_TO_INCLUDE, False),
    ],
)
def test_test_class(mode_, mode_simple, str_to_include_1, int_to_include_1, str_to_include_2, int_to_include_2, target_result):
    instance_1 = generate_test_class_instance(mode_, mode_simple, str_to_include_1, int_to_include_1)
    instance_2 = generate_test_class_instance(mode_, mode_simple, str_to_include_2, int_to_include_2)
    assert (instance_1 == instance_2) == target_result

def generate_simple_test_class_instance(all_include: bool, str_to_include: str, int_to_include: int):
    if all_include:
        return SimpleTestClass_AllInclude(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=NAMESPACE_FOR_INIT,
            str_attr_to_include=str_to_include,
            int_attr_to_include=int_to_include,
            str_attr_to_exclude_if_not_all_include=str_to_exclude(),
            int_attr_to_exclude_if_not_all_include=int_to_exclude()
        )
    else:
        return SimpleTestClass_NotAllInclude(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=NAMESPACE_FOR_INIT,
            str_attr_to_include=str_to_include,
            int_attr_to_include=int_to_include,
            str_attr_to_exclude_if_not_all_include=str_to_exclude(),
            int_attr_to_exclude_if_not_all_include=int_to_exclude()
        )

def generate_test_class_instance(all_include: bool, all_include_simple: bool, str_to_include: str, int_to_include: int):
    list_attr = [generate_simple_test_class_instance(all_include_simple, str_to_include, int_to_include)]
    if all_include:
        return NestedTestClass_AllInclude(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=NAMESPACE_FOR_INIT,
            str_attr_to_include=str_to_include,
            int_attr_to_include=int_to_include,
            str_attr_to_exclude_if_not_all_include=str_to_exclude(),
            int_attr_to_exclude_if_not_all_include=int_to_exclude(),
            list_attr_to_include=list_attr if all_include else None,
            list_attr_to_exclude_if_not_all_include=list_attr if not all_include else None
        )
    else:
        return NestedTestClass_NotAllInclude(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=NAMESPACE_FOR_INIT,
            str_attr_to_include=str_to_include,
            int_attr_to_include=int_to_include,
            str_attr_to_exclude_if_not_all_include=str_to_exclude(),
            int_attr_to_exclude_if_not_all_include=int_to_exclude(),
            list_attr_to_include=list_attr if all_include else None,
            list_attr_to_exclude_if_not_all_include=list_attr if not all_include else None
        )

def str_to_exclude():
    """This method generates a uuid string that will be different each time calling it."""
    return str(uuid.uuid4())

def int_to_exclude():
    """This method generates a uuid4 string as a 128-bit integer that will be different each time calling it."""
    return uuid.uuid4().int
