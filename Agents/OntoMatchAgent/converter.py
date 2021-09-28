import numpy as np
import pandas as pd
import rdflib
from rdflib import RDF, RDFS, OWL, Namespace, Literal, URIRef
from rdflib.namespace import XSD
from rdflib.term import _is_valid_uri
from tqdm import tqdm

BASE = Namespace('http://www.theworldavatar.com/kb/powsys/dukes/')
BASE_GPPD = Namespace('http://www.theworldavatar.com/kb/powsys/gppd/')
BASE_KWL = Namespace('http://www.theworldavatar.com/kb/powsys/kwl/')
DBR = Namespace('http://dbpedia.org/resource/')
DBO = Namespace('http://dbpedia.org/ontology/')
PSREAL = Namespace('http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#')
POW = Namespace('http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#')
CPSYS = Namespace('http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#')
CPSYSV1 = Namespace('http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#')
CPSPACEEXT = Namespace('http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#')
CPUNIT = Namespace('http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#')
EIPREAL = Namespace('http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#')
SCHEMA = Namespace('http://schema.org/')

def uri(prefix, entity):
    return URIRef(prefix + entity)

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

def replace_special_symbols(s):
    s = s.replace('/', ' ').replace('\\', ' ')
    return s

def create_plant_from_dictionary(g, d, version, country_short, use_schema=False):

    plant_name_norm = d.get('plantnamenorm')
    if plant_name_norm:
        name = replace_special_symbols(plant_name_norm)
        name = name.replace(' ', '_') + '_' + country_short
    elif d['plantname']:
        name = replace_special_symbols(d['plantname'])
        name = name.replace(' ', '_') + '_' + country_short
    else:
        name = country_short
    if 'plant_id' in d:
        name = d['plant_id'] + '_' + name

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
        g.add((s, SCHEMA['address'], x))
        g.add((x, SCHEMA['addressCountry'], Literal(country_short)))
        o = d['location']
        if o:
            g.add((x, SCHEMA['addressLocality'], Literal(o)))
        o = d['zip_code']
        if o:
            g.add((x, SCHEMA['postalCode'], Literal(o, datatype=XSD.integer)))
        o = d['street']
        if o:
            g.add((x, SCHEMA['streetAddress'], Literal(o)))
        o = d['region']
        if o:
            g.add((x, SCHEMA['addressRegion'], Literal(o)))


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

    return name

def bind_prefixes(g):
    g.bind('base', BASE)
    g.bind('rdf', RDF)
    g.bind('owl', OWL)
    g.bind('dbr', DBR)
    g.bind('psreal', PSREAL)
    g.bind('cpsys', CPSYS)
    g.bind('cpsysv1', CPSYSV1)
    g.bind('cpspaceext', CPSPACEEXT)
    g.bind('cpunit', CPUNIT)
    g.bind('eipreal', EIPREAL)

def create_DUKES_plants(source_file, target_file, version, format):

    filter_strings = [] #['Pencoose', 'Goonhilly_Downs_2', 'Goonhilly_Downs_1']

    graph = rdflib.Graph()
    bind_prefixes(graph)

    dframe = pd.read_csv(source_file)
    #size_dbr = len('http://dbpedia.org/resource/')

    if version == 'v2':
        owner_dict = create_owner_dict(graph, dframe) #, filter_strings)

    for _, row in dframe.iterrows():
        # TODO
        orig_plant_name = row['name'][:-3]
        if filter_strings and orig_plant_name not in filter_strings:
            continue

        if version == 'v2':
            owner = owner_dict[row['owner']]
        else:
            owner = row['owner']

        row = {
            'type': row['type'],
            'plantname': orig_plant_name,
            'owner': owner,
            'country': URIRef(row['adress']),
            'long': row['gis_x'],
            'lat': row['gis_y'],
            'year_built': row['year_built'],
            'design_cap': row['design cap']
        }
        create_plant_from_dictionary(graph, row, version, 'UK')

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

def create_GPPDB_plants(source_file, target_file, version, format):

    global BASE
    BASE = BASE_GPPD

    filter_strings = [] #['Pencoose', 'Goonhilly Downs Wind Farm', 'Goonhilly Solar']

    g = rdflib.Graph()
    bind_prefixes(g)

    df = pd.read_csv(source_file)
    #size_dbr = len('http://dbpedia.org/resource/')

    owner_dict = {}
    #if version == 'v2':
    #    owner_dict = create_owner_dict(g, df)

    for _, row in df.iterrows():
        # TODO
        orig_plant_name = v(row, 'name')
        if filter_strings and orig_plant_name not in filter_strings:
            continue

        owner = v(row, 'owner')
        if owner and version == 'v2':
            #owner = owner_dict[owner]
            owner = get_owner(g, owner_dict, owner)

        fuel = v(row, 'primary_fuel')
        fuel_type = convert_fuel_to_subclass(fuel)
        country = v(row, 'country_long')
        if country:
            country = DBR[country.replace(' ', '_')]

        d = {
            'type': fuel_type,
            'plantname': orig_plant_name,
            'owner': owner,
            'country': country,
            'long': v(row, 'longitude'),
            'lat': v(row, 'latitude'),
            'year_built': v(row, 'commissioning_year'),
            'design_cap': v(row, 'capacity_mw')
        }
        create_plant_from_dictionary(g, d, version, 'UK')

    g.serialize(target_file, format=format)


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
        plant_name_norm = normalize_plant_name(plant_name)

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

def normalize_plant_name(plant_name):
    plant_name_norm = plant_name
    if plant_name_norm:
        plant_name_norm = plant_name_norm.replace('�', '')
        plant_name_norm = plant_name_norm.replace('&', '')
        plant_name_norm = plant_name_norm.replace('.', '')
        plant_name_norm = plant_name_norm.replace('"', '')
        plant_name_norm = plant_name_norm.replace('#', '')
        plant_name_norm = plant_name_norm.replace(',', '_')
        plant_name_norm = plant_name_norm.replace('-', '_')
        plant_name_norm = plant_name_norm.replace('\n', '_')
    return plant_name_norm

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
        plant_name_norm = normalize_plant_name(plant_name)

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
    dframe = dframe.sort_values(by='idx')[:10]

    country = DBR['Germany']
    owner_dict = {}
    #if version == 'v2':
    #    owner_dict = create_owner_dict(g, df)

    count = 0
    for i, row in tqdm(dframe.iterrows()):

        plant_name = v(row, 'name')
        plant_name_norm = normalize_plant_name(plant_name)

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
            'location': v(row, 'location'),
            'zip_code': v(row, 'zip_code'),
            'street': v(row, 'street'),
            'region': v(row, 'federal_state'),
            #'long': v(row, 'longitude'),
            #'lat': v(row, 'latitude'),
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


if __name__ == '__main__':

    frmt = 'turtle'
    #frmt = 'xml'
    #src_file = 'C:/my/tmp/ontomatch/dukes_tmp.csv'
    #tgt_file = 'C:/my/tmp/ontomatch/matching_test_files/4_dukes_all.owl'
    #create_DUKES_plants(source_file=src_file, target_file=tgt_file, version='v1', format=frmt)
    #src_file = 'C:/my/CARES_CEP_project/CARES_CEP_docs/ontology_matching/original_data/globalpowerplantdatabasev120/global_power_plant_database.csv'
    #create_GPPDB_plants(source_file=src_file, target_file=tgt_file, version='v1', format=frmt)
    #tgt_dir = 'C:/my/tmp/ontomatch/tmp_gppd_files'
    #create_GPPDB_plants_single_files(source_file=src_file, target_dir=tgt_dir, version='v1', format=frmt, country='United Kingdom', country_short='UK')
    #create_GPPDB_plants_single_files(source_file=src_file, target_dir=tgt_dir, version='v1', format=frmt, country='Germany', country_short='DE')
    src_file = 'C:/my/tmp/ontomatch/Kraftwerksliste_CSV_2020_04_UTF8_TMP.csv'
    #tgt_dir = 'C:/my/tmp/ontomatch/tmp_kwl_files'
    tgt_file = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl_test.ttl'
    #tgt_file = 'C:/my/tmp/ontomatch/tmp_kwl_files/kwl_test.owl'
    #create_KWL_plants_single_files(source_file=src_file, target_dir=tgt_dir, version='v1', format=frmt)
    create_KWL_plants(source_file=src_file, target_file=tgt_file, version='v1', format=frmt)