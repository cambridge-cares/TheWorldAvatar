"""
# Author: qhouyee #

A test suite for the agent.kgutils.querybuilder submodule.
"""

# Third party import
import pytest

# Self import
from agent.kgutils.querybuilder import QueryBuilder
from agent.exceptions import QueryBuilderError


def test_init_querybuilder():
    """
    Tests that the QueryBuilder is properly initialised
    """
    builder = QueryBuilder(0)
    assert builder.prefix == ""
    assert builder.select_string == "SELECT "
    assert builder.where_string == "WHERE {"


def test_init_querybuilder_fail():
    """
    Tests that the right exceptions are raised when QueryBuilder fails to initialise
    """
    # Raises error with wrong input
    with pytest.raises(QueryBuilderError) as exc_info:
        QueryBuilder(1)
    # Check error message
    assert exc_info.match(
        r"^Invalid query_type parameter! Valid parameter includes 0.$")


def test_add_prefix():
    """
    Tests the add_prefix method of QueryBuilder
    """
    # Set up test cases
    base_namespace = "http://www.example.org/base"
    test_namespace = "http://www.test.org/test/namespace"
    prefix = "test"
    # Expected result
    expected = "PREFIX base:<" + base_namespace + ">\n"
    expected += "PREFIX " + prefix + ":<" + test_namespace + ">\n"
    # Initialise class
    builder = QueryBuilder(0)
    # Execute methods
    builder.add_prefix(base_namespace)
    builder.add_prefix(test_namespace, prefix)
    # Test assertion
    assert builder.prefix == expected


def test_add_prefix_fail():
    """
    Tests that the add_prefix method of QueryBuilder fails with the right exceptions
    """
    # Set up test cases
    base_namespace = "http://www.example.org/base"
    test_namespace = "http://www.test.org/test/namespace"
    prefix = "test"
    # Initialise class
    builder = QueryBuilder(0)
    builder.add_prefix(base_namespace)
    # Raises error with the same prefix
    with pytest.raises(QueryBuilderError) as exc_info:
        builder.add_prefix(test_namespace)
    # Check error message
    assert exc_info.match("base has already been added. Please input a new prefix.")

    # Raises error with the same namespace
    with pytest.raises(QueryBuilderError) as exc_info:
        builder.add_prefix(base_namespace, prefix)
    # Check error message
    assert exc_info.match(base_namespace+" exists. Please input a new namespace.")


def test_add_select_var():
    """
    Tests the add_select_var method of QueryBuilder
    """
    # Set up test variable
    test_var = "test"
    # Initialise class
    builder = QueryBuilder(0)
    # Execute methods
    builder.add_select_var(test_var)
    # Test assertion
    assert builder.select_string == "SELECT ?" + test_var + " "


def test_add_where_triple():
    """
    Tests the add_where_triple method of QueryBuilder
    """
    # Initialise test cases
    prefix = "base:"
    subject = "sub"
    predicate = "prop"
    object_node = "obj"
    # Initialise expected where clause
    expected = "WHERE {"
    # Initialise class
    builder = QueryBuilder(0)
    # Execute methods
    # No variable
    builder.add_where_triple(prefix + subject, prefix + predicate, prefix + object_node)
    expected += prefix + subject + " " + prefix + predicate + " " + prefix + object_node + ".\n"
    # Subject is variable
    builder.add_where_triple(subject, prefix + predicate, prefix + object_node, 1)
    expected += "?" + subject + " " + prefix + predicate + " " + prefix + object_node + ".\n"
    # Predicate is variable
    builder.add_where_triple(prefix + subject, predicate, prefix + object_node, 2)
    expected += prefix + subject + " ?" + predicate + " " + prefix + object_node + ".\n"
    # Object is variable
    builder.add_where_triple(prefix + subject, prefix + predicate, object_node, 3)
    expected += prefix + subject + " " + prefix + predicate + " ?"+ object_node + ".\n"
    # Subject and predicate are variables
    builder.add_where_triple(subject, predicate, prefix + object_node, 4)
    expected += "?" + subject + " ?" + predicate + " " + prefix + object_node + ".\n"
    # Subject and object are variables
    builder.add_where_triple(subject, prefix + predicate, object_node, 5)
    expected += "?" + subject + " " + prefix + predicate + " ?" + object_node + ".\n"
    # Predicate and object are variables
    builder.add_where_triple(prefix + subject, predicate, object_node, 6)
    expected += prefix + subject + " ?" + predicate + " ?" + object_node + ".\n"
    # All nodes are variables
    builder.add_where_triple(subject, predicate, object_node, 7)
    expected += "?" + subject + " ?" + predicate + " ?" + object_node + ".\n"
    # Test assertion
    assert builder.where_string == expected

def test_add_optional_triple():
    """
    Tests the add_optional_triple method of QueryBuilder
    """
    # Initialise test cases
    prefix = "base:"
    subject = "sub"
    predicate = "prop"
    object_node = "obj"
    # Initialise expected where clause
    expected = "WHERE {"
    # Initialise class
    builder = QueryBuilder(0)
    # Execute methods
    # No variable
    builder.add_optional_triple(prefix + subject, prefix + predicate, prefix + object_node)
    expected += "OPTIONAL {" + prefix + subject + " " + prefix + predicate + " " + prefix + object_node + ".}\n"
    # Subject is variable
    builder.add_optional_triple(subject, prefix + predicate, prefix + object_node, 1)
    expected += "OPTIONAL {?" + subject + " " + prefix + predicate + " " + prefix + object_node + ".}\n"
    # Predicate is variable
    builder.add_optional_triple(prefix + subject, predicate, prefix + object_node, 2)
    expected += "OPTIONAL {" + prefix + subject + " ?" + predicate + " " + prefix + object_node + ".}\n"
    # Object is variable
    builder.add_optional_triple(prefix + subject, prefix + predicate, object_node, 3)
    expected += "OPTIONAL {" + prefix + subject + " " + prefix + predicate + " ?"+ object_node + ".}\n"
    # Subject and predicate are variables
    builder.add_optional_triple(subject, predicate, prefix + object_node, 4)
    expected += "OPTIONAL {?" + subject + " ?" + predicate + " " + prefix + object_node + ".}\n"
    # Subject and object are variables
    builder.add_optional_triple(subject, prefix + predicate, object_node, 5)
    expected += "OPTIONAL {?" + subject + " " + prefix + predicate + " ?" + object_node + ".}\n"
    # Predicate and object are variables
    builder.add_optional_triple(prefix + subject, predicate, object_node, 6)
    expected += "OPTIONAL {" + prefix + subject + " ?" + predicate + " ?" + object_node + ".}\n"
    # All nodes are variables
    builder.add_optional_triple(subject, predicate, object_node, 7)
    expected += "OPTIONAL {?" + subject + " ?" + predicate + " ?" + object_node + ".}\n"
    # Test assertion
    assert builder.where_string == expected


def test_add_where_triple_fail():
    """
    Tests that the add_where_triple method of QueryBuilder fails with the right exceptions
    """
    # Initialise class
    builder = QueryBuilder(0)
    # Raises error when VALUES is inserted agian
    with pytest.raises(QueryBuilderError) as exc_info:
        builder.add_where_triple("sub", "pred","obj", 9)
    # Check error message
    assert exc_info.match("Invalid var_index! Only 0 - 7 is valid.")


def test_add_values_where():
    """
    Tests the add_values_where method of QueryBuilder
    """
    # Set up test variable
    test_var = "test"
    # Initialise class
    builder = QueryBuilder(0)
    # Execute methods
    builder.add_values_where(test_var, 0, 1, 2) # test for int
    # Test method
    assert builder.where_string == "WHERE {VALUES ?" + test_var + " {0 1 2 } \n"

    # Test for string variables
    builder = QueryBuilder(0)
    builder.add_values_where(test_var, "test", "result")
    # Test method
    assert builder.where_string == "WHERE {VALUES ?" + test_var + " {'test' 'result' } \n"

    # Test for classes
    builder = QueryBuilder(0)
    builder.add_values_where(test_var, "base:Subject", "base:Object")
    # Test method
    assert builder.where_string == "WHERE {VALUES ?" + test_var + " {base:Subject base:Object } \n"


def test_add_values_where_fail():
    """
    Tests that the add_values_where method of QueryBuilder fails with the right exceptions
    """
    # Initialise class
    builder = QueryBuilder(0)
    builder.add_values_where("test", 0, 1, 2)
    # Raises error when VALUES is inserted agian
    with pytest.raises(QueryBuilderError) as exc_info:
        builder.add_values_where("test", 0, 1, 2)
    # Check error message
    assert exc_info.match("VALUES clause has already been defined!")


def test_str_return():
    """
    Tests that the QueryBuilder object returns the right values
    """
    # Initialise test cases
    subject= "s"
    pred = "p"
    object_node = "o"
    namespace = "http://www.test.org"
    # Initialise and add placeholders
    builder = QueryBuilder()
    builder.add_select_var(subject, pred, object_node)
    builder.add_where_triple(subject, pred, object_node, 7)
    # Generate expected values
    expected = "SELECT ?" + subject + " ?" + pred + " ?" + object_node + " "
    expected += "WHERE {?" + subject + " ?" + pred + " ?" + object_node + ".\n}"
    # Test assertion
    assert str(builder) == expected

    # Test assertion for prefix
    builder.add_prefix(namespace)
    expected = "PREFIX base:<" + namespace + ">\n" + expected
    assert str(builder) == expected
