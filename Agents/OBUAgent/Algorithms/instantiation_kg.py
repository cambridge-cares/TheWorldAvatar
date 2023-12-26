import csv
import uuid
from rdflib import Graph, RDF, URIRef, RDFS, Literal
ONTO_COFS = 'https://www.theworldavatar.com/kg/ontocofs/'


def create_cofs_iri_str(clz):
    return f'{ONTO_COFS}{clz}_{str(uuid.uuid4())}'

def create_bindingsite(g, bs, bs_iri):
    g.add((bs_iri, RDF.type, URIRef(ONTO_COFS + 'BindingSite')))
    g.add((bs_iri, RDFS.label, Literal(bs)))
    return g

def create_core(g, core, core_iri):
    g.add((core_iri, RDF.type, URIRef(ONTO_COFS + 'Core')))
    g.add((core_iri, RDFS.label, Literal(core)))
    return g

def create_precursor(g, bs_iri, core_iri):
    precursor_iri = URIRef(create_cofs_iri_str('Precursor'))
    g.add((precursor_iri, RDF.type, URIRef(ONTO_COFS + 'Precursor')))
    g.add((core_iri, URIRef(ONTO_COFS + 'compatibleWith'), bs_iri))
    g.add((precursor_iri, URIRef(ONTO_COFS + 'hasCore'), core_iri))
    g.add((precursor_iri, URIRef(ONTO_COFS + 'hasBindingSite'), bs_iri))
    return g

def create_lfr(g, bs1_iri, bs2_iri):
    lfr_iri = URIRef(create_cofs_iri_str('LinkageFormation'))
    g.add((lfr_iri, RDF.type, URIRef(ONTO_COFS + 'LinkageFormation')))
    if bs1_iri is not None:
        g.add((lfr_iri, URIRef(ONTO_COFS + 'hasBindingSite'), bs1_iri))
    if bs2_iri is not None:
        g.add((lfr_iri, URIRef(ONTO_COFS + 'hasBindingSite'), bs2_iri))
    return g

def create_ocn(g, bs_iri, ocn):
    g.add((bs_iri, URIRef(ONTO_COFS + 'outerCoordinationNumber'), Literal(ocn)))
    return g

def create_smarts(g, iri, smarts):
    g.add((iri, URIRef(ONTO_COFS + 'smarts'), Literal(smarts)))
    return g

def instantiate_precursor_lfr_ocn(precursor_file, reaction_file, ocn_file, bs_smarts_file, core_smarts_file, ttl_file=None):
    core_bs_dict = {}
    bs_iri_dict = {}
    core_iri_dict = {}
    g = Graph()
    # Instantiate precursors
    with open(precursor_file, 'r', encoding='utf-8') as csvfile:
        reader = csv.reader(csvfile)
        next(reader)
        for row in reader:
            if row[1] not in core_bs_dict:
                core_bs_dict[row[1]] = []
            core_bs_dict[row[1]].append(row[0]) if row[0] not in core_bs_dict[row[1]] else None

    for core in core_bs_dict:
        core_iri = create_cofs_iri_str('Core')
        core_iri_dict[core] = core_iri
        for bs in core_bs_dict[core]:
            if bs not in bs_iri_dict:
                bs_iri = create_cofs_iri_str('BindingSite')
                bs_iri_dict[bs] = bs_iri
            g = create_bindingsite(g, bs, URIRef(bs_iri_dict[bs]))
            g = create_core(g, core, URIRef(core_iri_dict[core]))
            g = create_precursor(g, URIRef(bs_iri_dict[bs]), URIRef(core_iri_dict[core]))

    # Instantiate reactions
    with open(reaction_file, 'r', encoding='utf-8') as csvfile:
        reader = csv.reader(csvfile)
        next(reader)
        for row in reader:
            if row[1] not in bs_iri_dict and row[1] != 'None':
                bs_iri = create_cofs_iri_str('BindingSite')
                bs_iri_dict[row[1]] = bs_iri
                g = create_bindingsite(g, row[1], URIRef(bs_iri_dict[row[1]]))
            if row[2] not in bs_iri_dict and row[2] != 'None':
                bs_iri = create_cofs_iri_str('BindingSite')
                bs_iri_dict[row[2]] = bs_iri
                g = create_bindingsite(g, row[2], URIRef(bs_iri_dict[row[2]]))
            g = create_lfr(
                g,
                URIRef(bs_iri_dict[row[1]]) if row[1] != 'None' else None,
                URIRef(bs_iri_dict[row[2]]) if row[2] != 'None' else None,
            )

    # Instantiate outer coordination number
    with open(ocn_file, 'r', encoding='utf-8') as csvfile:
        reader = csv.reader(csvfile)
        next(reader)
        for row in reader:
            g = create_ocn(g, URIRef(bs_iri_dict[row[0]]), int(row[1]))

    # Instantiate bs smarts strings
    with open(bs_smarts_file, 'r', encoding='utf-8') as csvfile:
        reader = csv.reader(csvfile)
        next(reader)
        for row in reader:
            g = create_smarts(g, URIRef(bs_iri_dict[row[0]]), row[2])

    # Instantiate core smarts strings
    with open(core_smarts_file, 'r', encoding='utf-8') as csvfile:
        reader = csv.reader(csvfile)
        next(reader)
        for row in reader:
            if row[0] in core_iri_dict:
                g = create_smarts(g, URIRef(core_iri_dict[row[0]]), row[1].strip('"'))

    if ttl_file is not None:
        g.serialize(destination=ttl_file, format='turtle')
    return g, bs_iri_dict, core_iri_dict

def number_of_precursor(sparql_client):
    return len(
        sparql_client.performQuery(
        """
        prefix ocof: <https://www.theworldavatar.com/kg/ontocofs/>
        select distinct ?p
        where {
            ?p a ocof:Precursor.
        }
        """)
    )
