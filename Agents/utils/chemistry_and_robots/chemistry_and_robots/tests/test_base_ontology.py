from chemistry_and_robots.data_model.base_ontology import *
import pytest

@pytest.fixture()
def initialise_variables():
    instance_iri = INSTANCE_IRI_TO_BE_INITIALISED
    instance_iri_random = 'http://test_example/TestInstanceIRI'
    clz = 'http://test_example/TestClass'
    namespace_for_init = 'http://test_example/test_namespace/'
    yield instance_iri_random, instance_iri, clz, namespace_for_init

def test_create_base_ontology(initialise_variables):
    instance_iri_random, instance_iri, clz, namespace_for_init = initialise_variables

    test_instance = BaseOntology(instance_iri=instance_iri, clz=clz, namespace_for_init=namespace_for_init)
    assert test_instance.instance_iri.startswith(namespace_for_init)
    assert test_instance.clz == clz
    assert test_instance.namespace_for_init == namespace_for_init

def test_instance_iri_missing_exception(initialise_variables):
    instance_iri_random, instance_iri, clz, namespace_for_init = initialise_variables

    with pytest.raises(InstanceIRIInitialisationError) as e_info:
        test_instance = BaseOntology()
    assert str(e_info.value).startswith(InstanceIRIInitialisationError.instance_iri_missing)

def test_both_missing_missing_exception(initialise_variables):
    instance_iri_random, instance_iri, clz, namespace_for_init = initialise_variables

    with pytest.raises(InstanceIRIInitialisationError) as e_info:
        test_instance = BaseOntology(instance_iri=instance_iri)
    assert str(e_info.value).startswith(InstanceIRIInitialisationError.both_clz_and_namespace_missing)

    with pytest.raises(InstanceIRIInitialisationError) as e_info:
        test_instance = BaseOntology(instance_iri=instance_iri, namespace_for_init=None)
    assert str(e_info.value).startswith(InstanceIRIInitialisationError.both_clz_and_namespace_missing)

    with pytest.raises(InstanceIRIInitialisationError) as e_info:
        test_instance = BaseOntology(instance_iri=instance_iri, clz=None)
    assert str(e_info.value).startswith(InstanceIRIInitialisationError.both_clz_and_namespace_missing)

    with pytest.raises(InstanceIRIInitialisationError) as e_info:
        test_instance = BaseOntology(instance_iri=instance_iri, namespace_for_init=None, clz=None)
    assert str(e_info.value).startswith(InstanceIRIInitialisationError.both_clz_and_namespace_missing)

def test_clz_missing_exception(initialise_variables):
    instance_iri_random, instance_iri, clz, namespace_for_init = initialise_variables

    with pytest.raises(InstanceIRIInitialisationError) as e_info:
        test_instance = BaseOntology(instance_iri=instance_iri, namespace_for_init=namespace_for_init)
    assert str(e_info.value).startswith(InstanceIRIInitialisationError.clz_missing)

    with pytest.raises(InstanceIRIInitialisationError) as e_info:
        test_instance = BaseOntology(instance_iri=instance_iri, namespace_for_init=namespace_for_init, clz=None)
    assert str(e_info.value).startswith(InstanceIRIInitialisationError.clz_missing)

def test_namespace_for_init_missing_exception(initialise_variables):
    instance_iri_random, instance_iri, clz, namespace_for_init = initialise_variables

    with pytest.raises(InstanceIRIInitialisationError) as e_info:
        test_instance = BaseOntology(instance_iri=instance_iri, clz=clz)
    assert str(e_info.value).startswith(InstanceIRIInitialisationError.namespace_for_init_missing)

    with pytest.raises(InstanceIRIInitialisationError) as e_info:
        test_instance = BaseOntology(instance_iri=instance_iri, clz=clz, namespace_for_init=None)
    assert str(e_info.value).startswith(InstanceIRIInitialisationError.namespace_for_init_missing)

def test_namespace_for_init_should_not_be_provided_exception(initialise_variables):
    instance_iri_random, instance_iri, clz, namespace_for_init = initialise_variables

    with pytest.raises(InstanceIRIInitialisationError) as e_info:
        test_instance = BaseOntology(instance_iri=instance_iri_random, namespace_for_init=namespace_for_init)
    assert str(e_info.value).startswith(InstanceIRIInitialisationError.namespace_for_init_should_not_be_provided)

    with pytest.raises(InstanceIRIInitialisationError) as e_info:
        test_instance = BaseOntology(instance_iri=instance_iri_random, clz=clz, namespace_for_init=namespace_for_init)
    assert str(e_info.value).startswith(InstanceIRIInitialisationError.namespace_for_init_should_not_be_provided)
