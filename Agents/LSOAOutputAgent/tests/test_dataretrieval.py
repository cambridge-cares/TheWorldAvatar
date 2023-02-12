################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #    
# Date: 04 Apr 2022                            #
################################################

import pytest

from agent.dataretrieval.dataretrival import *
from agent.errorhandling.exceptions import InvalidInput

from tests.conftest import *

# Import module(s) under test from agent

@pytest.mark.parametrize('year, expectedMsg', \
[
(2020, 'Provided formate of year is not string')
])
def test_retrieve_year_format(year, expectedMsg):
    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        retrieve_elec_data_from_KG(year = year)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)

    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        retrieve_gas_data_from_KG(year = year)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)

    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        retrieve_fuel_poor_from_KG(year = year)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)

    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        retrieve_ONS_shape_from_KG(year = year)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)

    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        retrieve_temp_from_KG(year = year)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)

    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        output_query_template('Electricity', year)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)

@pytest.mark.parametrize('keyword, expectedMsg', \
[
('Fuel Poverty', 'Not a valid keyword provided. Please check the spelling/Capitals etc etc...')
])
def test_retrieve_keyword_format(keyword, expectedMsg):
    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        output_query_template(keyword, '2020')
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)

@pytest.mark.parametrize("keyword,expected_query", \
[
('Electricity', """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT DISTINCT ?s ?usage ?meter WHERE {?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography>;<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasConsumed> ?elec;
                    <http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#hasElecMeters> ?meteriri.
    ?meteriri <http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#hasConsumingElecMeters> ?meter;
              <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasStartUTC>  "2020-01-01 12:00:00"^^<http://www.w3.org/2001/XMLSchema#dateTime>.
    ?energy <http://www.ontology-of-units-of-measure.org/resource/om-2/hasPhenomenon> ?elec;
            <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> ?usageiri.
    ?usageiri <http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue> ?usage.
    ?elec <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasStartUTC>  "2020-01-01 12:00:00"^^<http://www.w3.org/2001/XMLSchema#dateTime>.
    }"""),
('Gas', """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT DISTINCT ?s ?usage ?meter ?nonmeter WHERE {?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography>;<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasUsed> ?gas;
                    <http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#hasGasMeters> ?metiri.
        ?metiri <http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#hasConsumingGasMeters> ?meter;
                <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasStartUTC>  "2020-01-01 12:00:00"^^<http://www.w3.org/2001/XMLSchema#dateTime>; 
                <http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#hasNonConsumingGasMeters> ?nonmeter.
        ?energy <http://www.ontology-of-units-of-measure.org/resource/om-2/hasPhenomenon> ?gas;
                 <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> ?usageiri.
        ?usageiri <http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue>  ?usage.
        ?gas      <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasStartUTC>  "2020-01-01 12:00:00"^^<http://www.w3.org/2001/XMLSchema#dateTime>.

        }"""),
('Fuel poverty', """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT DISTINCT ?s (xsd:float(?a)/xsd:float(?num) AS ?result) ?num WHERE {?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography>;  <http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl#hasHouseholds> ?housesiri.
     ?housesiri <http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl#fuelpoorhouseholds> ?a;
             <http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl#validFrom> "2020-01-01 12:00:00"^^<http://www.w3.org/2001/XMLSchema#dateTime>;
             <http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl#numberofhouseholds> ?num.
        }"""),
('Temperature', """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT DISTINCT ?s ?start ?var ?t WHERE {?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography>;    <http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#hasClimateMeasurement>  ?m.
    ?m <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasStartUTC> ?start;
        <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasEndUTC> ?end.
    ?m  <http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#hasClimateVariable> ?var.
    ?p <http://www.ontology-of-units-of-measure.org/resource/om-2/hasPhenomenon> ?m.
    ?p <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> ?t_iri.
    ?t_iri <http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue> ?t.

    FILTER (regex(str(?start), "2020-\\\d\\\d-01 12:00:00") && datatype(?start) = xsd:dateTime)
        }"""),
('ONS output area', """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT DISTINCT ?s ?geom WHERE {?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography>;   <http://www.opengis.net/ont/geosparql#hasGeometry> ?o.
    OPTIONAL{
            ?o <http://www.opengis.net/ont/geosparql#asWKT> ?geom}
    }"""),]
)
def test_query_string(keyword,expected_query):

    # Create a query
    query = output_query_template(keyword,'2020')
    assert query == expected_query
