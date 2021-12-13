import html
import logging
import re

import numpy as np
import pandas as pd
import rdflib
from rdflib import RDF, RDFS, OWL, SDO, Namespace, Literal, URIRef
from rdflib.namespace import XSD
from rdflib.term import _is_valid_uri
from tqdm import tqdm

import ontomatch.utils.util

# TODO-AE 211106 rename BASE to BASE_DUKES
BASE = Namespace('http://www.theworldavatar.com/kb/powsys/dukes/')
BASE_GPPD = Namespace('http://www.theworldavatar.com/kb/powsys/gppd/')
BASE_KWL = Namespace('http://www.theworldavatar.com/kb/powsys/kwl/')
BASE_MUN_GER = Namespace('http://www.theworldavatar.com/kb/municipalities/')
BASE_REST_FODOR = Namespace('http://www.theworldavatar.com/kb/restaurants/fodor/')
BASE_REST_ZAGAT = Namespace('http://www.theworldavatar.com/kb/restaurants/zagat/')
BASE_BIBL_DBLP = Namespace('http://www.theworldavatar.com/kb/bibliography/dblp/')
BASE_BIBL_ACM = Namespace('http://www.theworldavatar.com/kb/bibliography/acm/')
BASE_BIBL_SCHOLAR = Namespace('http://www.theworldavatar.com/kb/bibliography/scholar/')
BASE_PROD_AMAZON = Namespace('http://www.theworldavatar.com/kb/products/amazon/')
BASE_PROD_GOOGLE = Namespace('http://www.google.com/base/feeds/snippets/')
DBO = Namespace('http://dbpedia.org/ontology/')
DBR = Namespace('http://dbpedia.org/resource/')
GEO = Namespace('http://www.w3.org/2003/01/geo/wgs84_pos#')
POW = Namespace('http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#')
PSREAL = Namespace('http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#')
CPSYS = Namespace('http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#')
CPSYSV1 = Namespace('http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#')
CPSPACEEXT = Namespace('http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#')
CPUNIT = Namespace('http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#')
EIPREAL = Namespace('http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#')
CPTECSYS = Namespace('http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#')

def bind_prefixes(g):
    g.bind('rdf', RDF)
    g.bind('owl', OWL)
    g.bind('sdo', SDO)  # schema.org
    g.bind('base', BASE)
    g.bind('dbo', DBO)
    g.bind('dbr', DBR)
    g.bind('geo', GEO )
    g.bind('pow', POW)
    g.bind('psreal', PSREAL)
    g.bind('cpsys', CPSYS)
    g.bind('cpsysv1', CPSYSV1)
    g.bind('cpspaceext', CPSPACEEXT)
    g.bind('cpunit', CPUNIT)
    g.bind('eipreal', EIPREAL)
    g.bind('cptecsys', CPTECSYS)

def add(graph, subject, predicates, obj=None):

    prev_object = subject
    triples = []
    for i, current_pred in enumerate(predicates):
        if i < len(predicates) - 1 or obj is None:
            current_obj = rdflib.BNode()
        else:
            current_obj = obj
        t = (prev_object, current_pred, current_obj)
        graph.add(t)
        triples.append(t)
        prev_object = current_obj

    return triples, current_obj

def replace_special_characters(s):
    s = s.replace('/', ' ').replace('\\', ' ')
    return s

def normalize(s: str):
    if s and isinstance(s, str):
        symbols = [',', '-', '–', '\n', ' ', u'\xa0', "'"]
        for sym in symbols:
            s = s.replace(sym, '_')
        symbols = ['�', '&', '?', '+', '.', '"', '#', '(', ')', '\\', '/', '!', '%', ':']
        for sym in symbols:
            s = s.replace(sym, '')
        return s.strip()
    return s

def create_owner_dict(g, df):
    d = {}
    for _, row in df.iterrows():
        name = v(row, 'owner')
        if name:
            s = BASE[name.replace(' ', '_')]
            if _is_valid_uri(s):
                g.add((s, RDF.type, CPSYSV1['Organization']))
                g.add((s, CPSYSV1['hasName'], Literal(name)))
                d[name] = s
            else:
                print('skipped owner {} because no valid URI'.format(s))
    return d

def get_owner(g, dictionary, name):
    if not name:
        return None
    if name in dictionary:
        return dictionary[name]

    s = BASE[name.replace(' ', '_')]
    #TODO-AE skipping owners
    if _is_valid_uri(s):
        g.add((s, RDF.type, CPSYSV1['Organization']))
        g.add((s, CPSYSV1['hasName'], Literal(name)))
        dictionary[name] = s
        return s

    print('skipped owner {} because no valid URI'.format(s))

def create_plant_from_dictionary(g, d, version, country_short, use_schema=False):

    plant_name_norm = d.get('plantnamenorm')
    if plant_name_norm:
        name = replace_special_characters(plant_name_norm)
        if country_short:
            name += '_' + country_short
    elif d['plantname']:
        name = replace_special_characters(d['plantname'])
        if country_short:
            name += '_' + country_short
    else:
        name = country_short
    if 'plant_id' in d:
        name = str(d['plant_id']) + '_' + name

    s = BASE[name]
    g.add((s, RDF.type, OWL.NamedIndividual))
    g.add((s, RDF.type, PSREAL[d['type']]))

    # some rows e.g. in KWL have an empty plant name
    label = d['plantname']
    if label:
        label = d['plantname'].replace('_', ' ')
        g.add((s, RDFS.label, Literal(label, lang="en")))


    if not use_schema:
        o = d['country']
        if o:
            g.add((s, CPSYS['hasAddress'], o))
    else:
        o = d['country']
        if o:
            g.add((s, DBO['country'], o))

        x = rdflib.BNode()
        g.add((s, SDO['address'], x))
        g.add((x, SDO['addressCountry'], Literal(country_short)))
        o = d['location']
        if o:
            g.add((x, SDO['addressLocality'], Literal(o)))
        o = d['zip_code']
        if o:
            g.add((x, SDO['postalCode'], Literal(o, datatype=XSD.integer)))
        o = d['street']
        if o:
            g.add((x, SDO['streetAddress'], Literal(o)))
        o = d['region']
        if o:
            g.add((x, SDO['addressRegion'], Literal(o)))


    longitude = d.get('long')
    latitude = d.get('lat')
    if longitude and latitude:
        x = rdflib.BNode()
        g.add((x, RDF.type, CPSPACEEXT['ProjectedCoordinateSystem']))
        g.add((s, CPSPACEEXT['hasGISCoordinateSystem'], x))
        add(g, x, [ CPSPACEEXT['hasProjectedCoordinate_x'], CPSYS['hasValue'], CPSYS['numericalValue'] ], Literal(longitude))
        add(g, x, [ CPSPACEEXT['hasProjectedCoordinate_y'], CPSYS['hasValue'], CPSYS['numericalValue'] ], Literal(latitude))

    o = d['year_built']
    if o:
        x = rdflib.BNode()
        g.add((s, POW['hasYearOfBuilt'], x))
        add(g, x, [ CPSYS['hasValue'], CPSYS['numericalValue'] ], Literal(int(o), datatype=XSD.integer))

    o = d['design_cap']
    if o:
        x = rdflib.BNode()
        add(g, s, [ EIPREAL['designCapacity'], CPSYS['hasValue'] ], x)
        g.add((x, CPSYS['numericalValue'], Literal(o)))
        g.add((x, CPSYS['hasUnitOfMeasure'], CPUNIT['MW']))

    o = d['owner']
    if o:
        if version == 'v2':
            g.add((s, CPSYSV1['isOwnedBy'], o))
        else:
            x = rdflib.BNode()
            g.add((s, CPSYSV1['isOwnedBy'], x))
            g.add((x, CPSYSV1['hasName'], Literal(o)))

    o = d['primary_fuel']
    if o:
        x = rdflib.BNode()
        g.add((s, CPTECSYS['realizes'], x))
        #TODO-AE URGENT 211022 use URL instead of string
        g.add((x, POW['consumesPrimaryFuel'], Literal(o)))

    return name

def create_DUKES_plants(source_file, target_file, version, format, coordinates):

    #filter_strings = [] #['Pencoose', 'Goonhilly_Downs_2', 'Goonhilly_Downs_1']

    graph = rdflib.Graph()
    bind_prefixes(graph)

    dframe = pd.read_csv(source_file, index_col='dukes_id')
    dframe['primary_fuel'] = dframe['primary_fuel'].copy().apply(fuel_UK)

    #size_dbr = len('http://dbpedia.org/resource/')

    if version == 'v2':
        owner_dict = create_owner_dict(graph, dframe) #, filter_strings)

    for i, row in dframe.iterrows():
        # TODO
        #plant_name = row['name'] #[:-3]
        plant_name = row['file_name'][:-7]
        j = plant_name.index('_')
        plant_name = plant_name[j+1:]
        #if filter_strings and plant_name not in filter_strings:
        #    continue

        if version == 'v2':
            owner = owner_dict[row['owner']]
        else:
            owner = row['owner']

        fuel = v(row, 'primary_fuel')
        #fuel_type = convert_fuel_to_subclass(fuel)


        drow = {
            'plant_id': i,
            'type': row['onto_type'],
            'primary_fuel': fuel,
            'plantname': plant_name,
            'owner': owner,
            'country': URIRef(row['address']),
            'year_built': row['year_built'],
            'design_cap': row['design_capacity']
        }

        if coordinates:
            drow.update({
                'long': row['long'],
                'lat': row['lat'],
            })

        create_plant_from_dictionary(graph, drow, version, 'GBR')

    graph.serialize(target_file, format=format)

def convert_fuel_to_subclass(s):
    if s in ['Biomass', 'Solar', 'Wind', 'Nuclear', 'Waste', 'Wind']:
        return s + 'Plant'
    elif s in ['Coal', 'Oil', 'Gas']:
        return 'FossilFuelPlant'
    elif s == 'Hydro':
        return 'HydroelectricPlant'
        #'HydroelectricPlant_PumpHydro', 'PumpHydro', 'HydrogenPlant
    # Cogeneration, 'Wave and Tidal'
    return 'PowerPlant'

def v(row, key):
    value = row[key]
    if pd.isna(value) or value is np.nan or not value:
        value = None
    return value

def create_GPPDB_plants(source_file, target_file, version, frmt, country_short):

    global BASE
    BASE = BASE_GPPD

    #filter_strings = [] #['Pencoose', 'Goonhilly Downs Wind Farm', 'Goonhilly Solar']

    graph = rdflib.Graph()
    bind_prefixes(graph)

    dframe = pd.read_csv(source_file, index_col='gppd_idnr')
    dframe = dframe[dframe['country'] == country_short]

    logging.info('number of rows in GPPD for country=%s is %s', country_short, len(dframe))

    #size_dbr = len('http://dbpedia.org/resource/')

    owner_dict = {}
    #if version == 'v2':
    #    owner_dict = create_owner_dict(g, df)

    for i, row in dframe.iterrows():
        # TODO
        plant_name = v(row, 'name')
        plant_name_norm = normalize(plant_name)

        #if filter_strings and plant_name not in filter_strings:
        #    continue

        owner = v(row, 'owner')
        if owner and version == 'v2':
            #owner = owner_dict[owner]
            owner = get_owner(graph, owner_dict, owner)

        fuel = v(row, 'primary_fuel')
        fuel_type = convert_fuel_to_subclass(fuel)
        country = v(row, 'country_long')
        if country:
            country = DBR[country.replace(' ', '_')]

        row = {
            'plant_id': i,
            'type': fuel_type,
            'primary_fuel': fuel,
            'plantname': plant_name,
            'plantnamenorm': plant_name_norm,
            'owner': owner,
            'country': country,
            'long': v(row, 'longitude'),
            'lat': v(row, 'latitude'),
            # there is no commissioning year for GPPDB GBR
            #'year_built': v(row, 'commissioning_year'),
            'design_cap': v(row, 'capacity_mw')
        }

        create_plant_from_dictionary(graph, row, version, country_short)

    graph.serialize(target_file, format=frmt)


def create_GPPDB_plants_single_files(source_file, target_dir, version, format, country, country_short):

    global BASE
    BASE = BASE_GPPD

    filter_strings = [] #['Pencoose', 'Goonhilly Downs Wind Farm', 'Goonhilly Solar']



    df = pd.read_csv(source_file)
    df = df.sort_values(by='gppd_idnr')

    #size_dbr = len('http://dbpedia.org/resource/')

    owner_dict = {}
    #if version == 'v2':
    #    owner_dict = create_owner_dict(g, df)

    count = 0
    for _, row in df.iterrows():
        # TODO
        plant_name = v(row, 'name')
        if filter_strings and plant_name not in filter_strings:
            continue
        plant_name_norm = normalize(plant_name)

        owner = v(row, 'owner')
        if owner and version == 'v2':
            #owner = owner_dict[owner]
            owner = get_owner(g, owner_dict, owner)

        fuel = v(row, 'primary_fuel')
        fuel_type = convert_fuel_to_subclass(fuel)
        current_country = v(row, 'country_long')
        if current_country != country:
            continue
        if current_country:
            current_country = DBR[current_country.replace(' ', '_')]


        gppd_idnr = v(row, 'gppd_idnr')

        d = {
            'plant_id': gppd_idnr,
            'type': fuel_type,
            'plantname': plant_name,
            'plantnamenorm': plant_name_norm,
            'owner': owner,
            'country': current_country,
            'long': v(row, 'longitude'),
            'lat': v(row, 'latitude'),
            'year_built': v(row, 'commissioning_year'),
            'design_cap': v(row, 'capacity_mw')
        }

        g = rdflib.Graph()
        bind_prefixes(g)
        name = create_plant_from_dictionary(g, d, version, country_short)
        target_file = target_dir + '/' + name + '.owl'
        g.serialize(target_file, format=format)

        count += 1
        print('Finished', count, name)
        #if count >= 10:
        #    break

dict_fuel_UK = {
    'Solar': 'Solar',
    'Wind': 'Wind',
    'Hydro': 'Hydro',
    'NaturalGas': 'Gas',
    'Oil': 'Oil',
    'CoalBiomass': 'Biomass',
    'Coal': 'Coal',
    'Nuclear': 'Nuclear',
    'PumpHydro': 'Hydro',
    'SourGas': 'Gas',
    'Waste': 'Waste',
    'Waste_municipalsolidwaste': 'Waste',
    'Waste_anaerobicdigestion': 'Waste'
}

def fuel_UK(s):
    value = dict_fuel_UK.get(s)
    if not value:
        value = 'Other'
    return value

dict_fuel_German = {
    'Abfall': 'Waste',
    'Biomasse': 'Biomass',
    'Braunkohle': 'Coal',
    'Erdgas': 'Gas',
    'Grubengas': 'Gas',
    'Kernenergie': 'Nuclear',
    'Laufwasser': 'Hydro',
    'Mineralölprodukte': 'Oil',
    'Pumpspeicher': 'Hydro',
    'Solare Strahlungsenergie': 'Solar',
    'Speicherwasser (ohne Pumpspeicher)': 'Hydro',
    'Steinkohle': 'Coal',
    'Windenergie (Offshore-Anlage)': 'Wind',
    'Windenergie (Onshore-Anlage)': 'Wind',
}
def fuel_German(s):
    value = dict_fuel_German.get(s)
    if not value:
        value = 'Other'
    return value

def convert_date_to_year(date):
    if date:
        if isinstance(date, str):
            try:
                i = int(date[-4:])
                return i
            except ValueError:
                return None
    return date

def normalize_loc(location):
    if not type(location) == str:
        return location
    try:
        # Cottbus - Chóśebuz -> Cottbus
        j = location.index(' - ')
        location = location[:j]
    except ValueError:
        pass
    normalized = location.replace('-', ' ').lower().strip()
    return normalized

def normalize_location(plz):
    for i, row in plz.iterrows():
        location = row['ort']
        plz.at[i, 'location_normalized'] = normalize_loc(location)
    return plz

def get_kwl(df):

    df = df.copy()

    # two index values are twice
    # The rows with the same index value (BNA0085b, BNA0083) are nearly the same. We don't delete any row but rename their index
    counts = df['Kraftwerksnummer Bundesnetzagentur'].value_counts()
    duplicates = counts[counts>1].index.to_list()
    print('repair duplicated indices=', duplicates)
    for i, row in df.iterrows():
        idx = row['Kraftwerksnummer Bundesnetzagentur']
        if idx in duplicates:
            df.at[i, 'Kraftwerksnummer Bundesnetzagentur'] = idx + '2'
            duplicates.remove(idx)

    kwl = pd.DataFrame()
    kwl['name'] = df['Kraftwerksname'].copy()
    kwl['unit_name'] = df['Blockname'].copy()
    #kwl['location'] = df['Ort\n(Standort Kraftwerk)'].copy().apply(lambda s : s.lower() if s is not np.nan else s)
    kwl['location'] = df['Ort\n(Standort Kraftwerk)'].copy()
    #kwl['location'] =  kwl['location'].apply(normalize_loc)
    kwl['zip_code'] = df['PLZ\n(Standort Kraftwerk)'].copy()
    kwl['street'] = df['Straße und Hausnummer (Standort Kraftwerk)'].copy()
    kwl['federal_state'] = df['Bundesland'].copy()
    kwl['owner'] = df['Unternehmen']
    kwl['commissioning_year'] = df['Datum der Aufnahme der kommerziellen Stromeinspeisung der Erzeugungseinheit [Datum/jahr]'].copy()
    # convert e.g. string '01.04.2018' into int 2018
    # conversion to int (instead of float) will raise an error since there are 39 rows with commissiong_year = nan
    kwl['commissioning_year'] = kwl['commissioning_year'].apply(convert_date_to_year)
    #kwl['commissioning_year'] = pd.to_numeric(kwl['commissioning_year'], downcast='integer')
    kwl['capacity_mw'] = df['Netto-Nennleistung (elektrische Wirkleistung) in MW'].copy()
    kwl['primary_fuel_orig'] = df['Auswertung\nEnergieträger (Zuordnung zu einem Hauptenergieträger bei Mehreren Energieträgern)'].copy()
    kwl['primary_fuel'] = kwl['primary_fuel_orig'].apply(fuel_German)
    kwl['status'] = df['Kraftwerksstatus \n(in Betrieb/\nvorläufig stillgelegt/\nsaisonale Konservierung\nReservekraftwerk/\nSonderfall)'].copy()

    # 1 = ja / yes, 0 = else (nein / no or empty)
    lambda_cogen = lambda s : 1 if s == 'ja' else 0
    kwl['cogeneration'] = df['Wärmeauskopplung (KWK)\n(ja/nein)'].copy().apply(lambda_cogen)
    kwl['net_trans'] = df['Bezeichnung Verknüpfungspunkt (Schaltanlage) mit dem Stromnetz der Allgemeinen Versorgung gemäß Netzbetreiber'].copy()
    kwl['net_voltage'] = df['Netz- oder Umspannebene des Anschlusses'].copy()
    kwl['net_operator'] = df['Name Stromnetzbetreiber'].copy()

    kwl.index = df['Kraftwerksnummer Bundesnetzagentur'].copy()
    kwl.index.name = 'idx'

    # remove all rows without id because they only contain aggregate values for stations with < 10 MW
    kwl = kwl[kwl.owner != 'EEG-Anlagen < 10 MW']
    kwl = kwl[kwl.owner != 'Nicht-EEG-Anlagen < 10 MW']

    return kwl



def create_KWL_plants_single_files(source_file, target_dir, version, format):

    global BASE
    BASE = BASE_KWL

    #filter_strings = []

    dframe = pd.read_csv(source_file, delimiter=';', encoding='utf8')
    dframe = get_kwl(dframe)
    dframe = dframe.sort_values(by='idx')[:10]


    #size_dbr = len('http://dbpedia.org/resource/')
    country = DBR['Germany']
    owner_dict = {}
    #if version == 'v2':
    #    owner_dict = create_owner_dict(g, df)

    count = 0
    for i, row in tqdm(dframe.iterrows()):

        plant_name = v(row, 'name')
        plant_name_norm = normalize(plant_name)

        #if filter_strings and plant_name not in filter_strings:
        #    continue

        owner = v(row, 'owner')
        if owner and version == 'v2':
            #owner = owner_dict[owner]
            owner = get_owner(graph, owner_dict, owner)

        fuel = v(row, 'primary_fuel')
        fuel_type = convert_fuel_to_subclass(fuel)

        d = {
            'plant_id': i,
            'type': fuel_type,
            'plantname': plant_name,
            'plantnamenorm': plant_name_norm,
            'owner': owner,
            'country': country,
            #'long': v(row, 'longitude'),
            #'lat': v(row, 'latitude'),
            'year_built': v(row, 'commissioning_year'),
            'design_cap': v(row, 'capacity_mw')
        }

        graph = rdflib.Graph()
        bind_prefixes(graph)
        name = create_plant_from_dictionary(graph, d, version, 'DE')

        if str(name).startswith('BNA0201a'):
            name = 'BNA0201a_Windpark_Dörpum_DE'
        elif str(name).startswith('BNA0201b'):
            name = 'BNA0201b_Windpark_Reußenköge'
        target_file = target_dir + '/' + name + '.owl'
        count += 1
        print('Finished', count, name, target_file)
        #if count >= 1960:
        graph.serialize(target_file, format=format)

def create_KWL_plants(source_file, target_file, version, format):

    global BASE
    BASE = BASE_KWL

    graph = rdflib.Graph()
    bind_prefixes(graph)

    #filter_strings = []

    dframe = pd.read_csv(source_file, delimiter=';', encoding='utf8')
    dframe = get_kwl(dframe)
    dframe = dframe.sort_values(by='idx')

    country = DBR['Germany']
    owner_dict = {}
    #if version == 'v2':
    #    owner_dict = create_owner_dict(g, df)

    count = 0
    for i, row in tqdm(dframe.iterrows()):

        plant_name = v(row, 'name')
        plant_name_norm = normalize(plant_name)

        #if filter_strings and plant_name not in filter_strings:
        #    continue

        owner = v(row, 'owner')
        if owner and version == 'v2':
            #owner = owner_dict[owner]
            owner = get_owner(graph, owner_dict, owner)

        fuel = v(row, 'primary_fuel')
        fuel_type = convert_fuel_to_subclass(fuel)

        region = v(row, 'federal_state')
        #if region in ['Luxemburg', 'Österreich', 'Schweiz']:
        #    logging.warning('the record is skipped since the country is not Germany, index=%s, region=%s', i, region)
        #    continue

        try:
            zip_code = v(row, 'zip_code')
            if zip_code is not None:
                zip_code = int(zip_code)
        except ValueError:
            logging.info('invalid zip code format, index=%s, zip code=%s', i, zip_code)
            zip_code = None


        d = {
            'plant_id': i,
            'type': fuel_type,
            'primary_fuel': fuel,
            'plantname': plant_name,
            'plantnamenorm': plant_name_norm,
            'owner': owner,
            'country': country,
            'location': v(row, 'location'),
            'zip_code': zip_code,
            'street': v(row, 'street'),
            'region': region,
            'long': v(row, 'longitude'),
            'lat': v(row, 'latitude'),
            'year_built': v(row, 'commissioning_year'),
            'design_cap': v(row, 'capacity_mw')
        }

        name = create_plant_from_dictionary(graph, d, version, 'DE', use_schema=True)

        '''
        if str(name).startswith('BNA0201a'):
            name = 'BNA0201a_Windpark_Dörpum_DE'
        elif str(name).startswith('BNA0201b'):
            name = 'BNA0201b_Windpark_Reußenköge'
        target_file = target_dir + '/' + name + '.owl'
        count += 1
        print('Finished', count, name, target_file)
        #if count >= 1960:
        graph.serialize(target_file, format=format)
        '''
    graph.serialize(target_file, format=format)

def create_location_file(source_file, target_file, frmt):

    dframe = pd.read_csv(source_file, delimiter=',', encoding='utf8')
    mask = (dframe['Satzart'] == 60)
    dframe = dframe[mask].copy()

    df_conv = pd.DataFrame()

    def fct_munic(s):
        for c in [',', '(', '/']:
            i = s.find(c)
            if i >= 0:
                s = s[0:i]
        return s

    df_conv['municipality'] = dframe['Gemeindename'].copy().apply(fct_munic)

    fct_postalcode = lambda s : None if np.isnan(s) else int(s)
    df_conv['postalcode'] = dframe['Postleitzahl'].copy().apply(fct_postalcode).astype('Int64')

    def fct_str_to_float(s):
        if isinstance(s, str):
            s = s.replace(',', '.')
            return float(s)
        return None

    df_conv['longitude'] = dframe['Längengrad'].copy().apply(fct_str_to_float)
    df_conv['latitude'] = dframe['Breitengrad'].copy().apply(fct_str_to_float)

    #df_conv.to_csv(tgt_file, index=False)

    global BASE
    BASE = BASE_MUN_GER

    graph = rdflib.Graph()
    bind_prefixes(graph)


    wikidata_city = URIRef('https://www.wikidata.org/wiki/Q515')


    for i, row in tqdm(df_conv.iterrows()):
        # TODO: remove characters from URL that are not allowed, e.g. ( or - or . (as in i.d. ....)
        municipality = row['municipality'].strip().replace(' ', '_') + '_DE'
        s = BASE[municipality]
        #graph.add((s, RDF.type, OWL.NamedIndividual))
        graph.add((s, RDF.type, wikidata_city))
        graph.add((s, RDFS.label, Literal(row['municipality'], lang='de')))
        o = row['postalcode']
        if isinstance(o, int):
            o = Literal(o, datatype=XSD.integer)
            graph.add((s, SDO['postalCode'], o))
        o = row['latitude']
        if isinstance(o, float):
            o = Literal(o, datatype=XSD.float)
            graph.add((s, GEO['lat'], o))
        o = row['longitude']
        if isinstance(o, float):
            o = Literal(o, datatype=XSD.float)
            graph.add((s, GEO['long'], o))

        #TODO federal state
        #if o:
        #    g.add((x, SDO['addressRegion'], Literal(o)))

    graph.serializ

def convert_dbpedia_to_ontopowsys(source_file, target_file, format):

    global BASE
    BASE = DBR

    graph = rdflib.Graph()
    bind_prefixes(graph)

    dframe = pd.read_csv(source_file, delimiter=',', encoding='utf8', index_col='idx')
    #dframe = dframe.sort_values(by='idx')

    country = DBR['Germany']
    owner_dict = {}

    for i, row in tqdm(dframe.iterrows()):

        plant_name = v(row, 'name')
        plant_name_norm = plant_name.replace('http://dbpedia.org/resource/', '')
        #plant_name_norm = normalize_plant_name(plant_name)

        #if filter_strings and plant_name not in filter_strings:
        #    continue

        owner = v(row, 'owner')

        fuel = v(row, 'primary_fuel')
        fuel_type = convert_fuel_to_subclass(fuel)

        d = {
            #'plant_id': None,
            'type': fuel_type,
            'plantname': plant_name,
            'plantnamenorm': plant_name_norm,
            'owner': owner,
            'country': country,
            'long': v(row, 'longitude'),
            'lat': v(row, 'latitude'),
            'year_built': v(row, 'commissioning_year'),
            'design_cap': v(row, 'capacity_mw')
        }

        create_plant_from_dictionary(graph, d, None, country_short='', use_schema=False)

    graph.serialize(target_file, format=format)

class ConverterRestaurant():

    def split_restaurant(self, s):

        reg_expressions = ['\D+', '\d+', '\d+[-/] ?\d+-+((\d+)|(DIVE))']
        #     reg_expressions = ['\D+', '\d+', '\D+', '\d+[-/] ?\d+-+((\d+)|(DIVE))']
        streets = ['dr.', 'ave.', 'aves.', 'st.', 'sts.', 'blvd.', 'rd.', ' road', 'way', 'hwy.',
                'pch', 'circle', 'cienega', 'alley', 'drive', 'sq.', 'park s', 'financial center', 'pl.', 'plaza',
                'norcross', 'flamingo', 'pkwy.', 'la.', 'northpoint', 'center', 'ln.', 'court']

        # Fodors special cases
        if s.startswith("Dante's Down the Hatch  Underground Underground Mall  Underground Atlanta Atlanta 404/577-1800 Continental"):
            return ["Dante's Down the Hatch", "", "Underground Underground Mall  Underground Atlanta", "Atlanta", "404/577-1800", "Continental"]
        if s.startswith("La Grotta at Ravinia Dunwoody Rd.  Holiday Inn/Crowne Plaza at Ravinia  Dunwoody Atlanta 770/395-9925 Italian"):
            return ["La Grotta", "", "at Ravinia Dunwoody Rd. Holiday Inn/Crowne Plaza at Ravinia  Dunwoody", "Atlanta", "770/395-9925", "Italian"]
        if s.startswith("Little Szechuan C Buford Hwy.  Northwoods Plaza  Doraville Atlanta 770/451-0192 Asian"):
            return ["Little Szechuan", "", "C Buford Hwy.  Northwoods Plaza  Doraville", "Atlanta", "770/451-0192", "Asian"]
        if s.startswith("Mi Spia Dunwoody Rd.  Park Place  across from Perimeter Mall  Dunwoody Atlanta 770/393-1333 Italian"):
            return ["Mi Spia", "", "Dunwoody Rd.  Park Place  across from Perimeter Mall  Dunwoody", "Atlanta", "770/393-1333", "Italian"]
        if s.startswith("Toulouse B Peachtree Rd. Atlanta 404/351-9533 French"):
            return ["Toulouse", "", "B Peachtree Rd.", "Atlanta", "404/351-9533", "French"]
        if s.startswith("Garden Court Market and New Montgomery Sts. San Francisco 415/546-5011 Old San Francisco"):
            return ["Garden Court", "", "Market and New Montgomery Sts.", "San Francisco", "415/546-5011", "Old San Francisco"]
        if s.startswith("Gaylord's Ghirardelli Sq. San Francisco 415/771-8822 Asian"):
            return ["Gaylord's", "", "Ghirardelli Sq.", "San Francisco", "415/771-8822", "Asian"]
        if s.startswith("Greens Bldg. A Fort Mason San Francisco 415/771-6222 Vegetarian"):
            return ["Greens", "", "Bldg. A Fort Mason", "San Francisco", "415/771-6222", "Vegetarian"]
        if s.startswith("McCormick & Kuleto's Ghirardelli Sq. San Francisco 415/929-1730 Seafood"):
            return ["McCormick & Kuleto's", "", "Ghirardelli Sq.", "San Francisco", "415/929-1730", "Seafood"]
        if s.startswith("Gladstone's 4 Fish 17300 Pacific Coast Hwy. at Sunset Blvd. Pacific Palisades 310/454-3474 American"):
            return ["Gladstone's 4 Fish", "17300", "Pacific Coast Hwy. at Sunset Blvd.", "Pacific Palisades", "310/454-3474", "American"]
        if s.startswith("21 Club 21 W. 52nd St. New York 212/582-7200 American"):
            return ["21 Club", "21", "W. 52nd St.", "New York", "212/582-7200", "American"]
        '''
        if s.startswith("Adrienne 700 5th Ave. at 55th St. New York 212/903-3918 French"):
            return ["Adrienne", "700", "5th Ave. at 55th St.", "New York", "212/903-3918", "French"]
        if s.startswith("Agrotikon 322 E. 14 St.  between 1st and 2nd Aves. New York 212/473-2602 Mediterranean"):
            return ["Agrotikon", "322", "E. 14 St.  between 1st and 2nd Aves.", "New York", "212/473-2602", "Mediterranean"]
        if s.startswith("Aja 937 Broadway at 22nd St. New York 212/473-8388 American"):
            return ["Aja", "937", "Broadway at 22nd St.", "New York", "212/473-8388", "American"]
        if s.startswith("Alamo 304 E. 48th St. New York 212/ 759-0590 Mexican"):
            return ["Alamo", "304", "E. 48th St.", "New York", "212/ 759-0590", "Mexican"]
        '''
        if s.startswith("Splendido Embarcadero 4 San Francisco 415/986-3222 Mediterranean"):
            return ["Splendido Embarcadero", "4", "", "San Francisco", "415/986-3222", "Mediterranean"]


        # Zagats special cases
        if s.startswith("Jody Maroni's Sausage Kingdom 2011 Ocean Front Walk Venice 310-306-1995 Hot Dogs"):
            return ["Jody Maroni's Sausage Kingdom", "2011", "Ocean Front Walk", "Venice", "310-306-1995", "Hot Dogs"]
        if s.startswith("Kuruma Zushi 2nd fl. New York City 212-317-2802 Japanese"):
            return ["Kuruma Zushi", "", "2nd fl.", "New York City", "212-317-2802", "Japanese"]
        if s.startswith("Oyster Bar lower level New York City 212-490-6650 Seafood"):
            return ["Oyster Bar", "", "lower level", "New York City", "212-490-6650", "Seafood"]
        if s.startswith("Tavern on the Green Central Park West New York City 212-873-3200 American (New)"):
            return ["Tavern on the Green", "", "Central Park West", "New York City", "212-873-3200", "American (New)"]
        if s.startswith("Windows on the World 107th fl. New York City 212-524-7000 Eclectic"):
            return ["Windows on the World", "", "107th fl.", "New York City", "212-524-7000", "Eclectic"]
        '''
        if s.startswith("Bradshaw's Restaurant 2911 S. Pharr Court Atlanta 404-261-7015 Southern/Soul"):
            return ["Bradshaw's Restaurant", "2911", "S. Pharr Court", "Atlanta", "404-261-7015", "Southern/Soul"]
        '''

        props = []
        for i, regex in enumerate(reg_expressions):
            span = re.search(regex, s).span()

            if i == 2:
                street_and_city = s[:span[0]]
                phone = s[span[0]: span[1]]
                props.append(street_and_city)
                props.append(phone)
            else:
                prop = s[:span[1]].strip()
                props.append(prop.strip())
            s = s[span[1]:]
        props.append(s.strip())

        # split street name and city
        s = props[2].lower()
        split = -1
        length = -1
        for street in streets:
            try:
                i = s.rindex(street) + len(street)
                if i > split:
                    split = i
                    length = len(street)
            except ValueError as err:
                pass

        if split > 0:
            street = props[2][:split].strip()
            location = props[2][split:].strip()
            props[2] = street
            props.insert(3, location)
        else:
            logging.error('unable to split street and city: %s', props)
            raise ValueError('unable to split street and city', props)

        return props

    def load_to_dframe(self,filename, short):
        rows = []
        with open(filename, encoding='utf8') as f:
            for i, line in tqdm(enumerate(f)):
                if len(line) < 3:
                    continue

                # convert special characters
                line = html.unescape(line)
                line = line.replace('\x02', '')
                line = line.replace('\n', '')

                row = self.split_restaurant(line)
                row.append(line.lower())
                row.append(short + str(i))
                rows.append(row)
        dframe = pd.DataFrame(rows, columns=['name', 'street_no', 'street', 'city', 'phone', 'type', 'description', 'idx'])
        dframe.set_index(['idx'], inplace=True)
        columns = [ str(col) for col in dframe.columns]
        logging.info('loaded and split restaurants, number=%s for file=%s, columns=%s', len(dframe), filename, columns)
        return dframe

    def convert_data(self, src_file, namespace, short, tgt_file, format):

        dframe = self.load_to_dframe(src_file, short)

        global BASE
        BASE = namespace

        graph = rdflib.Graph()
        bind_prefixes(graph)

        for idx, row in dframe.iterrows():
            # TODO: remove characters from URL that are not allowed, e.g. ( or - or . (as in i.d. ....)
            restaurant = idx + '_' + normalize(row['name'])
            s = BASE[restaurant]
            graph.add((s, RDF.type, SDO['Restaurant']))
            graph.add((s, RDFS.label, Literal(row['name'], lang='en')))
            o = row['type']
            if o:
                graph.add((s, SDO['servesCuisine'], Literal(o)))

            x = rdflib.BNode()
            graph.add((s, SDO['address'], x))
            o = row['phone']
            if o:
                graph.add((x, SDO['telephone'], Literal(o)))
            o = row['city']
            if o:
                graph.add((x, SDO['addressLocality'], Literal(o)))


            street_no = row['street_no']
            street = row['street']
            if street_no or street:
                o = street_no if street_no else ''
                o = o + ' ' + street if street else o
                if o:
                    graph.add((x, SDO['streetAddress'], Literal(o.strip())))

        graph.serialize(tgt_file, format=format)

        return dframe, graph

    def convert_matches_to_multi_indices(self, src_file, tgt_file, df1, df2):
        descriptions = []
        previous = ''
        current = ''
        with open(src_file, encoding='utf8') as f:
            for line in f:
                if len(line) < 3 or line.startswith('#'):
                    continue

                # convert special characters
                line = html.unescape(line)
                line = line.replace('\x02', '').replace('\n', '').lower()

                previous = current
                if line.startswith(' '):
                    # some records cover two lines
                    current = previous + line
                    previous = ''
                else:
                    current = line
                    # four entries in the original match-pairs.txt have spelling errors
                    # they are corrected here to get the corresponding entry in zagats.txt and fodors.txt, resp.
                    for prefix in ['cdaniel', 'bcafe', 'ghedgerose']:
                        if prefix in current:
                            current = current[1:]
                            break
                    if 'ext 6108' in current:
                        current = 'café  ritz-carlton  buckhead 3434 peachtree rd. atlanta 404/237-2700  ext 6108 international'
                if previous:
                    descriptions.append(previous)

        descriptions.append(current)
        assert len(descriptions) == 224  # = 2 * 112 matches
        logging.info('number restaurant descriptions=%s', len(descriptions))

        index_pairs = []
        first_index = None
        for i, d in enumerate(descriptions):
            if first_index is None:
                found = df1[df1['description'] == d]
                # the result set only contains a single entry
                if len(found) == 0:
                    logging.error('NOT FOUND: %s', d)
                    continue
                else:
                    first_index = found.index[0]
            else:
                found = df2[df2['description'] == d]
                if len(found) == 0:
                    logging.error('NOT FOUND: %s %s', i, d)
                    first_index = None
                    continue
                second_index = found.index[0]
                index_pairs.append({
                    'idx_1': first_index,
                    'idx_2': second_index,
                    'link': 1
                    })
                first_index = None

        #multi_index = pd.MultiIndex.from_tuples(index_pairs, names=['idx_1', 'idx_2'])
        df_matches = pd.DataFrame(index_pairs)
        df_matches.set_index(['idx_1', 'idx_2'], inplace=True)
        logging.info('number matching pairs=%s', len(df_matches))
        df_matches.to_csv(tgt_file, index=True)

    @staticmethod
    def convert():

        format = 'turtle'
        path = 'C:/my/CARES_CEP_project/CARES_CEP_docs/ontology_matching/original_data/restaurant/original'
        src_file_zagat = path + '/zagats.txt'
        src_file_fodor = path + '/fodors.txt'
        file_matches = path + '/match-pairs.txt'

        converter = ConverterRestaurant()
        df1, _ = converter.convert_data(src_file_zagat, BASE_REST_ZAGAT, 'Z', 'C:/my/tmp/ontomatch/tmp_kwl_files/zagats.ttl', format)
        df2, _ = converter.convert_data(src_file_fodor, BASE_REST_FODOR, 'F', 'C:/my/tmp/ontomatch/tmp_kwl_files/fodors.ttl', format)
        converter.convert_matches_to_multi_indices(file_matches,'C:/my/tmp/ontomatch/tmp_kwl_files/matches_restaurant.csv', df1, df2)

class ConverterDBLP2ACM():

    def load_to_dframe(self,filename):
        dframe = pd.read_csv(filename) #, encoding='utf8')
        dframe = dframe.rename(columns={'id': 'idx'})
        dframe.set_index(['idx'], inplace=True)
        columns = [ str(col) for col in dframe.columns]
        logging.info('loaded bibliographic records, number=%s for file=%s, columns=%s', len(dframe), filename, columns)
        return dframe

    def convert_data(self, src_file, namespace, tgt_file, format):

        dframe = self.load_to_dframe(src_file)

        global BASE
        BASE = namespace

        graph = rdflib.Graph()
        bind_prefixes(graph)

        for idx, row in dframe.iterrows():
            # replace '-' by '' not by '_' to be consistent with IRI's for KWL and DUKES
            # e.g. DBLP ID=conf/sigmod/Galindo-Legaria94
            norm_idx = normalize(str(idx).replace('-', ''))
            s = BASE[norm_idx]
            graph.add((s, RDF.type, SDO['ScholarlyArticle']))
            graph.add((s, SDO['name'], Literal(row['title'], lang='en')))
            graph.add((s, SDO['author'], Literal(row['authors'], lang='en')))

            x = rdflib.BNode()
            graph.add((s, SDO['isPartOf'], x))
            graph.add((x, RDF.type, SDO['PublicationIssue']))
            graph.add((x, SDO['name'], Literal(row['venue'], lang='en')))
            graph.add((x, SDO['datePublished'], Literal(row['year'], datatype=XSD.integer)))

        graph.serialize(tgt_file, format=format)

        return dframe, graph

    def convert_matches_to_multi_indices(self, src_file, tgt_file):
        df_matches = pd.read_csv(src_file)
        df_matches = df_matches.rename(columns={'idDBLP': 'idx_1', 'idACM': 'idx_2'})
        fct = lambda s : normalize(s.replace('-', ''))
        df_matches['idx_1'] = df_matches['idx_1'].copy().apply(fct)
        df_matches['link'] = 1
        df_matches.set_index(['idx_1', 'idx_2'], inplace=True)
        logging.info('number matching pairs=%s', len(df_matches))
        df_matches.to_csv(tgt_file, index=True)

    @staticmethod
    def convert():

        format = 'turtle'
        #path = 'C:/my/CARES_CEP_project/CARES_CEP_docs/ontology_matching/original_data/DBLP-ACM'
        path = 'C:/my/tmp/ontomatch/tmp_dblp_acm'
        src_file_dblp = path + '/DBLP2.csv'
        src_file_acm = path + '/ACM.csv'
        file_matches = path + '/DBLP-ACM_perfectMapping.csv'

        converter = ConverterDBLP2ACM()
        converter.convert_data(src_file_dblp, BASE_BIBL_DBLP, 'C:/my/tmp/ontomatch/tmp_dblp_acm/dblp.ttl', format)
        converter.convert_data(src_file_acm, BASE_BIBL_ACM, 'C:/my/tmp/ontomatch/tmp_dblp_acm/acm.ttl', format)
        converter.convert_matches_to_multi_indices(file_matches,'C:/my/tmp/ontomatch/tmp_dblp_acm/matches_bibliography.csv')

def isnan(value):
    return (isinstance(value, float) and np.isnan(value)) or (value == 'N/A') or (value is None)

class ConverterDBLP1Scholar():

    def load_to_dframe(self,filename):
        dframe = pd.read_csv(filename) #, encoding='utf8')
        dframe = dframe.rename(columns={'id': 'idx'})
        dframe.set_index(['idx'], inplace=True)
        columns = [ str(col) for col in dframe.columns]
        logging.info('loaded bibliographic records, number=%s for file=%s, columns=%s', len(dframe), filename, columns)
        return dframe

    def normalize_internal(self, s:str) -> str:
        return normalize(s.replace('-', '').replace('_', ''))

    def convert_data(self, src_file, namespace, tgt_file, format):

        dframe = self.load_to_dframe(src_file)

        global BASE
        BASE = namespace

        graph = rdflib.Graph()
        bind_prefixes(graph)

        for idx, row in dframe.iterrows():
            # replace '-' by '' not by '_' to be consistent with IRI's for KWL and DUKES
            # e.g. DBLP ID=conf/sigmod/Galindo-Legaria94
            norm_idx = self.normalize_internal(str(idx))
            s = BASE[norm_idx]
            graph.add((s, RDF.type, SDO['ScholarlyArticle']))
            graph.add((s, SDO['name'], Literal(row['title'], lang='en')))

            authors = row['authors']
            if not isnan(authors):
                graph.add((s, SDO['author'], Literal(authors, lang='en')))

            venue = row['venue']
            year = row['year']
            if not (isnan(venue) and isnan(year)):
                x = rdflib.BNode()
                graph.add((s, SDO['isPartOf'], x))
                graph.add((x, RDF.type, SDO['PublicationIssue']))
                if not isnan(venue):
                    graph.add((x, SDO['name'], Literal(venue, lang='en')))
                if not isnan(year):
                    year = int(year)
                    graph.add((x, SDO['datePublished'], Literal(year, datatype=XSD.integer)))

        graph.serialize(tgt_file, format=format)

        return dframe, graph

    def convert_matches_to_multi_indices(self, src_file, tgt_file):
        df_matches = pd.read_csv(src_file)
        df_matches = df_matches.rename(columns={'idDBLP': 'idx_1', 'idScholar': 'idx_2'})
        fct = lambda s : self.normalize_internal(str(s))
        df_matches['idx_1'] = df_matches['idx_1'].apply(fct)
        df_matches['idx_2'] = df_matches['idx_2'].apply(fct)
        df_matches['link'] = 1
        df_matches.set_index(['idx_1', 'idx_2'], inplace=True)
        logging.info('number matching pairs=%s', len(df_matches))
        df_matches.to_csv(tgt_file, index=True)

    @staticmethod
    def convert():

        format = 'turtle'
        path = './data/bibl_DBLP_Scholar/original'
        src_file_dblp = path + '/DBLP1.csv'
        src_file_acm = path + '/Scholar.csv'
        file_matches = path + '/DBLP-Scholar_perfectMapping.csv'

        converter = ConverterDBLP1Scholar()
        tgt_path = './data/bibl_DBLP_Scholar'
        converter.convert_data(src_file_dblp, BASE_BIBL_DBLP, tgt_path + '/dblp1.ttl', format)
        converter.convert_data(src_file_acm, BASE_BIBL_SCHOLAR, tgt_path + '/scholar.ttl', format)
        converter.convert_matches_to_multi_indices(file_matches, tgt_path + '/matches_dblp1_scholar.csv')

class ConverterProduct():

    def load_to_dframe(self,filename):
        dframe = pd.read_csv(filename) #, encoding='utf8')
        dframe = dframe.rename(columns={'id': 'idx'})
        dframe.set_index(['idx'], inplace=True)
        columns = [ str(col) for col in dframe.columns]
        logging.info('loaded products, number=%s for file=%s, columns=%s', len(dframe), filename, columns)
        return dframe

    def convert_data(self, src_file, namespace, tgt_file, format, name_column):

        dframe = self.load_to_dframe(src_file)

        global BASE
        BASE = namespace

        graph = rdflib.Graph()
        bind_prefixes(graph)

        prefix_length = len('http://www.google.com/base/feeds/snippets/')

        for idx, row in dframe.iterrows():

            if idx.startswith('http://www.google.com/base/feeds/snippets/'):
                idx = idx[prefix_length:]
            s = BASE[idx]
            graph.add((s, RDF.type, SDO['Product']))

            o = row[name_column]
            if isinstance(o, str) and o:
                graph.add((s, SDO['name'], Literal(o, lang='en')))

            o = row['manufacturer']
            if isinstance(o, str) and o:
                graph.add((s, SDO['manufacturer'], Literal(o , lang='en')))

            o = row['description']
            if isinstance(o, str) and o:
                graph.add((s, SDO['description'], Literal(o, lang='en')))

            o = row['price']
            if o is not None:
                # 62 out of 3327 Google products contain a price such as '236.18 gbp'
                # we remove the currency here
                if isinstance(o, str) and o.endswith(' gbp'):
                    o = o[:-4]
                o = float(o)
                # 199 out of 1363 Amazon products contain the price 0. which means missing price information
                # Thus, we skip price 0.
                if o > 0.:
                    x = rdflib.BNode()
                    graph.add((s, SDO['offers'], x))
                    graph.add((x, RDF.type, SDO['Offer']))
                    graph.add((x, SDO['price'], Literal(o, datatype=XSD.float)))

        graph.serialize(tgt_file, format=format)

        return dframe, graph

    def convert_matches_to_multi_indices(self, src_file, tgt_file):
        df_matches = pd.read_csv(src_file)
        df_matches = df_matches.rename(columns={'idAmazon': 'idx_1', 'idGoogleBase': 'idx_2'})
        df_matches['link'] = 1
        df_matches.set_index(['idx_1', 'idx_2'], inplace=True)
        logging.info('number matching pairs=%s', len(df_matches))
        df_matches.to_csv(tgt_file, index=True)

    @staticmethod
    def convert():

        format = 'turtle'
        #path = 'C:/my/CARES_CEP_project/CARES_CEP_docs/ontology_matching/original_data/Amazon-GoogleProducts'
        path = 'C:/my/tmp/ontomatch/tmp_product'
        src_file_dblp = path + '/Amazon_small.csv'
        src_file_acm = path + '/GoogleProducts_small.csv'
        file_matches = path + '/Amzon_GoogleProducts_perfectMapping_small.csv'

        converter = ConverterProduct()
        converter.convert_data(src_file_dblp, BASE_PROD_AMAZON, 'C:/my/tmp/ontomatch/tmp_product/amazon_small.ttl', format, name_column='title')
        converter.convert_data(src_file_acm, BASE_PROD_GOOGLE, 'C:/my/tmp/ontomatch/tmp_product/googleproducts_small.ttl', format, name_column='name')
        converter.convert_matches_to_multi_indices(file_matches,'C:/my/tmp/ontomatch/tmp_product/matches_product_small.csv')



if __name__ == '__main__':

    ontomatch.utils.util.init()

    main_dir = './data'

    frmt = 'turtle'
    #frmt = 'owl'
    #frmt = 'xml'
    #frmt = 'nt' # ntriples
    #src_file = 'C:/my/tmp/ontomatch/dukes_owl.csv'
    #tgt_file = 'C:/my/tmp/ontomatch/tmp_kwl_files/dukes_geo_211028.ttl'
    #create_DUKES_plants(source_file=src_file, target_file=tgt_file, version='v1', format=frmt, coordinates=True)

    #country_short='GBR'
    #country_short='DEU'
    #src_file = 'C:/my/CARES_CEP_project/CARES_CEP_docs/ontology_matching/original_data/globalpowerplantdatabasev120/global_power_plant_database.csv'
    #tgt_file = maindir + '/power_plant_GBR/gppd_GBR.ttl'
    #create_GPPDB_plants(source_file=src_file, target_file=tgt_file, version='v1', frmt=frmt, country_short=country_short)

    #tgt_dir = 'C:/my/tmp/ontomatch/tmp_gppd_files'
    #create_GPPDB_plants_single_files(source_file=src_file, target_dir=tgt_dir, version='v1', format=frmt, country='United Kingdom', country_short='UK')
    #create_GPPDB_plants_single_files(source_file=src_file, target_dir=tgt_dir, version='v1', format=frmt, country='Germany', country_short='DE')
    #src_file = 'C:/my/tmp/ontomatch/Kraftwerksliste_CSV_2020_04_UTF8_TMP.csv'
    #tgt_dir = 'C:/my/tmp/ontomatch/tmp_kwl_files'
    #tgt_file = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl_address_211022.ttl'
    #tgt_file = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl.owl'
    #tgt_file = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl.nt'
    #create_KWL_plants_single_files(source_file=src_file, target_dir=tgt_dir, version='v1', format=frmt)
    #create_KWL_plants(source_file=src_file, target_file=tgt_file, version='v1', format=frmt)

    #src_file = 'C:/my/tmp/ontomatch/dbpedia_DEU_converted_v1.csv'
    #tgt_file = 'C:/my/tmp/ontomatch/dbpedia_DEU_converted_ontopowsys.owl'
    #convert_dbpedia_to_ontopowsys(src_file, tgt_file, frmt)

    #src_file = 'C:/my/tmp/ontomatch/Municipalities_Germany_UTF8.csv'
    #tgt_file = 'C:/my/tmp/ontomatch/Municipalities_Germany.ttl'
    #create_location_file(src_file, tgt_file, frmt)

    #ConverterRestaurant.convert()
    #ConverterDBLP2ACM.convert()
    ConverterDBLP1Scholar.convert()
    #ConverterProduct.convert()
