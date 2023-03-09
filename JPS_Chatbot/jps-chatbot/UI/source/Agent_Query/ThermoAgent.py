import json
import os
from pprint import pprint
from urllib.error import HTTPError
from rapidfuzz import process, fuzz

if __name__ == "__main__":
    from AgentUtil.util.SPARQLWarehouse import ONTOCOMPCHEM_IRI_FROM_ONTOSPECIES_QUERY
    from AgentUtil.util.UniversalQuery import query_blazegraph, make_simple_http_request
    from AgentUtil.util.MarieLogger import MarieError, MarieIOLog, MarieMessage
    from AgentUtil.util.Lookup import find_nearest_match
    from AgentUtil.util.UnitConversion import convertPressure, convertTemperature
    from location import JPS_DICT_DIR, AGENT_QUERY_DIR
else:
    from .AgentUtil.util.SPARQLWarehouse import ONTOCOMPCHEM_IRI_FROM_ONTOSPECIES_QUERY
    from .AgentUtil.util.UniversalQuery import query_blazegraph, make_simple_http_request
    from .AgentUtil.util.MarieLogger import MarieError, MarieIOLog, MarieMessage
    from .AgentUtil.util.UnitConversion import convertPressure, convertTemperature
    from .AgentUtil.util.Lookup import find_nearest_match
    from .location import JPS_DICT_DIR, AGENT_QUERY_DIR

dictionary = {}
mappings = open(os.path.join(AGENT_QUERY_DIR, 'cc.txt')).readlines()[1:]
for m in mappings:
    old, new = m.split(',')
    dictionary[old.strip()] = new.strip()


def find_ontocompchem_IRI(ontospecies_iri):

    query = ONTOCOMPCHEM_IRI_FROM_ONTOSPECIES_QUERY % ontospecies_iri
    results = query_blazegraph(query, 'ontocompchem')
    ontocompchem_iri_list = []
    for binding in results["results"]["bindings"]:
        oc_iri = binding['ocIRI']['value']
        ontocompchem_iri_list.append(oc_iri)

    return ontocompchem_iri_list


def select_data(temperature, pressure, multi_point_result, single_point_result, _attribute):
    _original_attribute = _attribute
    # if there is a T, give single point
    if temperature is None:
        if pressure is None:
            # Both none, then give multiple points, varying T with default P
            selected_result = multi_point_result
        else:
            # T is none, P is given, give multi-point, you will have T graph with given P
            selected_result = multi_point_result
    else:
        if pressure is None:
            # T is given, P is none, give single point, there is no multi point with P changing
            selected_result = single_point_result
        else:
            # Both are fine
            selected_result = single_point_result

    if _attribute is None:
        return None
        # filtered_response = selected_result
        # filtered_response['attribute'] = 'all'
    else:
        _attribute = _attribute.capitalize()

    if _attribute in selected_result:
        filtered_response = selected_result[_attribute]  # value and unit
        filtered_response['temperature'] = selected_result['Temperature']
        filtered_response['pressure'] = selected_result['Pressure']
        filtered_response['attribute'] = _original_attribute
    else:
        filtered_response = selected_result
    return filtered_response


def filter_response(_result, _attribute, temperature=None, pressure=None):
    candidates = ['heat capacity', 'heat capacity at constant volume', 'heat capacity at constant pressure',
                  'internal energy', 'gibbs energy', 'entropy', 'enthalpy']

    _attribute = process.extractOne(_attribute.strip().lower(), candidates, scorer=fuzz.ratio)[0]
    _original_attribute = _attribute
    if 'result' in _result:
        single_point_result = _result['result']['Thermodynamic data for a single T, P point']
        multi_point_result = _result['result']['Thermodynamic data over a selected T range at a single P point']
    else:
        return None

    # heat capacity is an exception
    # the change of P won't affect single point heat capacity
    # same thing for multiple point heat capacity

    if _attribute == 'heat capacity':
        # do both
        _attribute = 'heat capacity at constant pressure'
        filtered_response_constant_p = select_data(temperature, pressure, multi_point_result, single_point_result,
                                                   _attribute)
        _attribute = 'heat capacity at constant volume'
        filtered_response_constant_v = select_data(temperature, pressure, multi_point_result, single_point_result,
                                                   _attribute)
        filtered_response = {"multiple_results": [filtered_response_constant_v, filtered_response_constant_p]}
    else:
        filtered_response = select_data(temperature, pressure, multi_point_result, single_point_result, _attribute)
    return filtered_response


def unitConversion(temperature, pressure):
    if temperature is not None:
        temperature = convertTemperature(temperature)
    if pressure is not None:
        pressure = convertPressure(pressure)
    return temperature, pressure


class ThermoAgent:
    def __init__(self):
        with open(os.path.join(JPS_DICT_DIR, 'ONTOSPECIES_URI_DICT')) as f:
            self.dict = json.loads(f.read())

        with open(os.path.join(JPS_DICT_DIR, 'ONTOSPECIES_KEYS')) as f:
            self.keys = json.loads(f.read())

    def find_IRI(self, _key):
        if _key is None:
            return None
        return self.dict[_key]

    def findOntoSpecies(self, species):
        species = species.replace("’s", "")
        if species == '-95 f':
            species = 'NH4OH'
        _key, _score = find_nearest_match(species, self.keys)
        _IRI = self.find_IRI(_key)
        return _IRI, _key, _score

    def callThermoAgent(self, species=None, attribute=None, temperature=None, pressure=None):
        if attribute is None:
            return None
        url = 'http://kg.cmclinnovations.com:81/stdc-agent/api/thermoagent/calculate'
        temperature, pressure = unitConversion(temperature, pressure)
        MarieMessage('temperature :{}, pressure: {}'.format(temperature, pressure))
        ontospecies_iri_list, key, score = self.findOntoSpecies(species)
        if ontospecies_iri_list is None:
            MarieError('No ontospecies iri found for this species {}'.format(species))
            return None

        if len(ontospecies_iri_list) == 0:
            MarieError('No ontospecies iri found for this species {}'.format(species))
        else:
            MarieMessage('ontospecies list {}'.format(ontospecies_iri_list))
        print('all_ontospecies_iri', ontospecies_iri_list)

        for ontospecies_iri in ontospecies_iri_list:
            print('ontospecies_iri', ontospecies_iri)
            if ontospecies_iri in dictionary:
                ontospecies_iri = dictionary[ontospecies_iri]
            else:
                print('not in the dictionary', ontospecies_iri)

            ontocompchem_iri_list = find_ontocompchem_IRI(ontospecies_iri)
            if len(ontocompchem_iri_list) == 0:
                MarieError('No ontocompchem iri found for this species {}'.format(species))
            else:
                MarieMessage('ontocompchem list {}'.format(ontocompchem_iri_list))

            for ontocompchem_iri in ontocompchem_iri_list:
                data = {'ontocompchem_IRI': ontocompchem_iri,
                        'ontospecies_IRI': ontospecies_iri,
                        'temperature': temperature,
                        'pressure': pressure}
                print('========= calling thermoagent with data ==========')
                pprint(data)
                MarieMessage(json.dumps(data, indent=4))
                raw_response = make_simple_http_request(url, data, None)
                if raw_response is None:
                    pass
                else:
                    return {'IRI': {'key': key, 'score': score}, 'data': data, 'result': filter_response(json.loads(raw_response), attribute, temperature=temperature,
                                           pressure=pressure)}
        return None


# findOntoSpecies(species)
if __name__ == '__main__':
    ta = ThermoAgent()
    # _species = 'inchi=1s/c2h6o/c1-2-3/h3h,2h2,1h3'
    _url = 'http://kg.cmclinnovations.com:81/stdc-agent/api/thermoagent/calculate'
    # t_list = [0, 300, 1232, 212, -323, 32555, 9999, None]
    # p_list = [0, 123219, 1210000, 1232334, -232343, None]
    # # attribute_list = ['heat capacity', 'gibbs energy', 'internal energy', 'enthalpy', 'entropy']
    # _species = 'c1cccc1'
    # _species = 'co2'
    # _species = 'benzene'
    # t = 'roomtemperature'
    # t = None
    # p = '100000 Pa'
    # attribute = 'entropy'
    # attribute = None
    # p = None
    # _species = 'geometry c=c=c'
    # attribute = 'HeatCapacity'

    _species = 'methane'
    t = '100 k'
    p = None
    attribute = 'Internal Energy'

    # {'species': 'c3h5n3o', 'temperature': 'temperature 294.62 degree celsius', 'attribute': 'enthalpy'}

    _species = 'c6h11o3'
    t = '300'
    p = None
    attribute = 'HeatCapacityAtConstPressure'

    response = ta.callThermoAgent(species=_species, temperature=t, pressure=p, attribute=attribute)
    print('response', response)
    # t_list = ['300 K', -232, None]
    # p_list = [100000, '100kpa', '-232343 Pa', None]
    # attribute_list = [None, 'heat capacity']
    #
    # tmp = {}
    # for t in t_list:
    #     for p in p_list:
    #         for attribute in attribute_list:
    #             key = 'T {} | P {} | A {}'.format(t, p, attribute)
    #             response = ta.callThermoAgent(species=_species, temperature=t, pressure=p,
    #                                           attribute=attribute)
    #             tmp[key] = response
    #
    # # ideally, species, attribute, qualifiers
    # with open('thermo_result', 'w') as f:
    #     f.write(json.dumps(tmp, indent=4))
