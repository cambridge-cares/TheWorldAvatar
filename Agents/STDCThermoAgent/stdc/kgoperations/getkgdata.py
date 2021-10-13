from stdc.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from stdc.kgoperations.querykg import querykg
from stdc.kgoperations.querytemplates import ontocompchem_data_query, \
                                             ontospecies_data_query
import stdc.unitconverter.unitconverter as unitconv

def get_ontocompchem_data(ocIRI, osIRI):
    query = ontocompchem_data_query(ocIRI, osIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['ontocompchem']
    data = querykg(sparqlEndPoint=sparqlendpoint, queryStr=query)
    if data:
        data = data[0]
        if data['frequencies']:
            data['frequencies'] = ','.join(data['frequencies'].split())
        if data['rot_constants']:
            data['rot_constants'] = ','.join(data['rot_constants'].split())
    return data

def get_ontospecies_data(osIRI):
    query = ontospecies_data_query(osIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['ontospecies']
    data = querykg(sparqlEndPoint=sparqlendpoint, queryStr=query)

    enthalpy_ref_data = 'n/a'
    if data:
        data = data[0]

        if 'enthalpy_ref' in data:
            enthalpy_ref = data['enthalpy_ref']
            enthalpy_ref_unit = data.pop('enthalpy_ref_unit','n/a')
            enthalpy_ref_prov = data.pop('enthalpy_ref_prov', 'n/a')
            enthalpy_ref_temp = data['enthalpy_ref_temp'] if 'enthalpy_ref_temp' in data else 'n/a'
            enthalpy_ref_temp_unit = data.pop('enthalpy_ref_temp_unit', '')
            enthalpy_ref_type = "Standard enthalpy of formation"

            if 'enthalpy_ref_temp' in data:
                enthalpy_ref_type = f"{enthalpy_ref_type} at {float(enthalpy_ref_temp):.2f} {enthalpy_ref_temp_unit}"
                enthalpy_ref_type = enthalpy_ref_type.strip()
            enthalpy_ref_data = \
            {
                "type": f"{enthalpy_ref_type}",
                "value": f"{float(enthalpy_ref):.2f}",
                "unit": enthalpy_ref_unit,
                "provenance": enthalpy_ref_prov
            }
        if enthalpy_ref_unit != 'n/a':
            href_conv = unitconv.convertEnergyMoleUnitsToSI(enthalpy_ref_unit)
            data['enthalpy_ref'] = str(float(data['enthalpy_ref']) * href_conv)

        if enthalpy_ref_temp_unit:
            tref_conv = unitconv.convertTemperatureUnitsToSI(enthalpy_ref_temp_unit)
            data['enthalpy_ref_temp'] = str(float(data['enthalpy_ref_temp']) * tref_conv[0] + tref_conv[1])
    return data, enthalpy_ref_data