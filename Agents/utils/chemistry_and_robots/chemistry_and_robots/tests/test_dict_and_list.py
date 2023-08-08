import pytest

from chemistry_and_robots.kg_operations import dict_and_list as dal

@pytest.mark.parametrize(
    "list_of_dict,key,value,expected_list_of_dict",
    [
        ([], "key", "value", []),
        ([{"key": "value", "key2": "value"}, {"key": "value_", "key2": "value_"}], "key", "value", [{"key": "value", "key2": "value"}]),
        ([{"key": "value", "key2": "value_"}, {"key": "value_", "key2": "value_"}], "key", "value", [{"key": "value", "key2": "value_"}]),
        ([{"key": "value", "key2": "value"}, {"key": "value_", "key2": "value_"}], "key", "value_", [{"key": "value_", "key2": "value_"}]),
        ([{"key": "value", "key2": "value"}, {"key": "value", "key2": "value_"}], "key", "value", [{"key": "value", "key2": "value"}, {"key": "value", "key2": "value_"}]),
    ],
)
def test_get_sublist_in_list_of_dict_matching_key_value(list_of_dict, key, value, expected_list_of_dict):
    assert dal.get_sublist_in_list_of_dict_matching_key_value(list_of_dict, key, value) == expected_list_of_dict

@pytest.mark.parametrize(
    "list_of_dict,key,expected_list_of_value",
    [
        ([], "key", []),
        ([{"key": "value", "key2": "value"}, {"key": "value_", "key2": "value_"}], "key", ["value", "value_"]),
    ],
)
def test_get_value_from_list_of_dict(list_of_dict, key, expected_list_of_value):
    assert sorted(dal.get_value_from_list_of_dict(list_of_dict, key)) == sorted(expected_list_of_value)

@pytest.mark.parametrize(
    "list_of_dict,key,expected_list_of_value",
    [
        ([], "key", []),
        ([{"key": "value", "key2": "value"}, {"key": "value_", "key2": "value_"}], "key", ["value", "value_"]),
        ([{"key": "value", "key2": "value"}, {"key": "value", "key2": "value_"}], "key", ["value"]),
    ],
)
def test_get_unique_values_in_list_of_dict(list_of_dict, key, expected_list_of_value):
    assert sorted(dal.get_unique_values_in_list_of_dict(list_of_dict, key)) == sorted(expected_list_of_value)

@pytest.mark.parametrize(
    "list_of_dict,wanted_keys,expected_list_of_dict",
    [
        ([], [], []),
        ([{"key": "value", "key2": "value"}], ["key"], [{"key": "value"}]),
        ([{"key": "value", "key2": "value"}], ["key2"], [{"key2": "value"}]),
        ([{"key": "value", "key2": "value"}], ["key", "key2"], [{"key": "value", "key2": "value"}]),
        ([{"key": "value", "key2": "value"}, {"key": "value_", "key2": "value_"}], ["key"], [{"key": "value"}, {"key": "value_"}]),
    ],
)
def test_keep_wanted_keys_from_list_of_dict(list_of_dict, wanted_keys, expected_list_of_dict):
    assert dal.keep_wanted_keys_from_list_of_dict(list_of_dict, wanted_keys) == expected_list_of_dict

@pytest.mark.parametrize(
    "list_of_dict,unwanted_keys,expected_list_of_dict",
    [
        ([], [], []),
        ([{"key": "value", "key2": "value"}], ["key"], [{"key2": "value"}]),
        ([{"key": "value", "key2": "value"}], ["key2"], [{"key": "value"}]),
        ([{"key": "value", "key2": "value"}], ["key", "key2"], []),
        ([{"key": "value", "key2": "value"}, {"key": "value_", "key2": "value_"}], ["key"], [{"key2": "value"}, {"key2": "value_"}]),
    ],
)
def test_remove_unwanted_keys_from_list_of_dict(list_of_dict, unwanted_keys, expected_list_of_dict):
    assert dal.remove_unwanted_keys_from_list_of_dict(list_of_dict, unwanted_keys) == expected_list_of_dict

@pytest.mark.parametrize(
    "list_of_dict,expected_list_of_dict",
    [
        ([], []),
        ([{"key": "value"}, {"key": "value"}], [{"key": "value"}]),
    ],
)
def test_remove_duplicate_dict_from_list_of_dict(list_of_dict, expected_list_of_dict):
    assert dal.remove_duplicate_dict_from_list_of_dict(list_of_dict) == expected_list_of_dict

@pytest.mark.parametrize(
    "list_of_dict,key,expected_result",
    [
        ([], "key", False),
        ([{"key": "value"}], "key", True),
        ([{"key": "value"}], "key2", False),
        ([{"key": "value"}, {"key2": "value2"}], "key", True),
        ([{"key": "value"}, {"key2": "value2"}], "key3", False),
    ],
)
def test_check_if_key_in_list_of_dict(list_of_dict, key, expected_result):
    assert dal.check_if_key_in_list_of_dict(list_of_dict, key) == expected_result

@pytest.mark.parametrize(
    "list_a,list_b,expected_result",
    [
        ([], [], True),
        (None, None, True),
        (None, [], False),
        ([], None, False),
        ([1, 2, 3], [1, 2, 3], True),
        ([1, 2, 3], [1, 2, 3, 4], False),
        ([1, 2, 3], [3, 2, 1], True),
        (["1", "2", "3"], ["1", "2", "3"], True),
        (["1", "2", "3"], ["3", "2", "1"], True),
    ],
)
def test_check_if_two_lists_equal(list_a, list_b, expected_result):
    assert dal.check_if_two_lists_equal(list_a, list_b) == expected_result
