import requests
import uuid
import logging
import itertools

# ---------------------------
# Configuration
# ---------------------------
ENDPOINT_URL = 'http://XX.XXX.XXX.XX:XXXX/blazegraph/namespace/ontocofs/sparql'

# Ontology Prefixes
PREFIXES = """
PREFIX ontocof: <https://www.theworldavatar.com/kg/ontocof/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
"""

# ---------------------------
# Logging Configuration
# ---------------------------
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)  # Capture all levels of logs

formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')

file_handler = logging.FileHandler('cof_discovery.log', mode='a')  # Append mode
file_handler.setLevel(logging.DEBUG)  # Capture all levels for the file
file_handler.setFormatter(formatter)
logger.addHandler(file_handler)

console_handler = logging.StreamHandler()
console_handler.setLevel(logging.INFO)  # Adjust as needed (DEBUG, INFO, etc.)
console_handler.setFormatter(formatter)
logger.addHandler(console_handler)

logging.info("Script has started running.")

# ---------------------------
# Utility Functions
# ---------------------------

def run_sparql_query(query, is_update=False, is_ask=False):
    headers = {}
    data = None

    if is_update:
        headers = {'Content-Type': 'application/sparql-update'}
        data = query
        logging.info("Sending SPARQL UPDATE Query:")
        logging.debug(query)
    else:
        headers = {'Accept': 'application/sparql-results+json'}
        data = {'query': query}
        logging.info("Sending SPARQL SELECT/ASK Query:")
        logging.debug(query)

    try:
        response = requests.post(ENDPOINT_URL, data=data, headers=headers)
        response.raise_for_status()

        if is_update:
            logging.info("SPARQL UPDATE successful.")
            return True

        json_response = response.json()

        if is_ask:
            return json_response.get('boolean', False)
        else:
            return json_response.get('results', {}).get('bindings', [])

    except requests.exceptions.HTTPError as e:
        logging.error(f"SPARQL query failed: {e}")
        logging.error(f"Response Text: {response.text}")
        return [] if not is_update and not is_ask else False
    except requests.exceptions.RequestException as e:
        logging.error(f"Request exception: {e}")
        return [] if not is_update and not is_ask else False
    except ValueError as e:
        logging.error(f"JSON decoding failed: {e}")
        return [] if not is_update and not is_ask else False

def construct_uri(uri_str):
    if not uri_str.startswith("<") and not uri_str.endswith(">"):
        return f"<{uri_str}>"
    return uri_str

# ---------------------------
# SPARQL Query Functions
# ---------------------------

def get_all_linkages():
    query = PREFIXES + """
    SELECT DISTINCT ?linkage WHERE {
        ?linkage rdf:type ontocof:Linkage .
    }
    """
    results = run_sparql_query(query)
    linkages = set(r['linkage']['value'] for r in results if 'linkage' in r)
    logging.info(f"  Retrieved {len(linkages)} Linkage(s).")
    return linkages

def get_binding_sites(linkage_uri):
    query = PREFIXES + f"""
    SELECT DISTINCT ?bindingSite WHERE {{
        {construct_uri(linkage_uri)} ontocof:involvesBindingSite ?bindingSite .
    }}
    """
    results = run_sparql_query(query)
    binding_sites = set(r['bindingSite']['value'] for r in results if 'bindingSite' in r)
    logging.info(f"Linkage {linkage_uri} has {len(binding_sites)} binding site(s).")
    return binding_sites

def get_precursors_with_binding_sites(binding_site_uris):
    if not binding_site_uris:
        logging.info("No Binding Sites provided for precursor retrieval.")
        return set()
    binding_sites_str = ', '.join(construct_uri(bs) for bs in binding_site_uris)
    query = PREFIXES + f"""
    SELECT DISTINCT ?precursor WHERE {{
        ?precursor rdf:type ontocof:Precursor .
        ?precursor ontocof:hasBindingSite ?bs .
        FILTER(?bs IN ({binding_sites_str}))
    }}
    """
    results = run_sparql_query(query)
    precursors = set(r['precursor']['value'] for r in results if 'precursor' in r)
    logging.info(f"    Retrieved {len(precursors)} Precursor(s) with these Binding Sites.")
    return precursors

def get_linkage_gbus(linkage_uri):
    query = PREFIXES + f"""
    SELECT DISTINCT ?linkage_gbu WHERE {{
        {construct_uri(linkage_uri)} ontocof:functionsAsLGBU ?linkage_gbu .
    }}
    """
    results = run_sparql_query(query)
    linkage_gbus = set(r['linkage_gbu']['value'] for r in results if 'linkage_gbu' in r)
    logging.info(f"    Retrieved {len(linkage_gbus)} Linkage_GBU(s) via functionsAsLGBU.")
    return linkage_gbus

def get_assembly_models_with_linkage_gbus(linkage_gbu_uris):
    if not linkage_gbu_uris:
        logging.info("    No Linkage_GBU(s) provided for Assembly Model retrieval.")
        return set()
    linkage_gbus_str = ', '.join(construct_uri(g) for g in linkage_gbu_uris)
    query = PREFIXES + f"""
    SELECT DISTINCT ?assemblyModel WHERE {{
        ?assemblyModel rdf:type ontocof:AssemblyModel .
        ?assemblyModel ontocof:hasLinkageGenericBuildingUnit ?linkage_gbu .
        FILTER(?linkage_gbu IN ({linkage_gbus_str}))
    }}
    """
    results = run_sparql_query(query)
    assembly_models = set(r['assemblyModel']['value'] for r in results if 'assemblyModel' in r)
    logging.info(f"    Retrieved {len(assembly_models)} Assembly Model(s).")
    return assembly_models

def get_gbus_with_assembly_models(assembly_model_uris):
    if not assembly_model_uris:
        logging.info("    No Assembly Models provided for GBU retrieval.")
        return set()
    assembly_models_str = ', '.join(construct_uri(am) for am in assembly_model_uris)
    query = PREFIXES + f"""
    SELECT DISTINCT ?gbu WHERE {{
        ?assemblyModel ontocof:hasGenericBuildingUnit ?gbu .
        FILTER(?assemblyModel IN ({assembly_models_str}))
    }}
    """
    results = run_sparql_query(query)
    gbus = set(r['gbu']['value'] for r in results if 'gbu' in r)
    logging.info(f"    Retrieved {len(gbus)} GBU(s).")
    return gbus

def get_precursors_with_gbus(gbu_uris):
    if not gbu_uris:
        logging.info("    No GBUs provided for precursor retrieval.")
        return set()
    gbus_str = ', '.join(construct_uri(g) for g in gbu_uris)
    query = PREFIXES + f"""
    SELECT DISTINCT ?precursor WHERE {{
        ?precursor rdf:type ontocof:Precursor .
        ?precursor ontocof:functionsAsGBU ?gbu .
        FILTER(?gbu IN ({gbus_str}))
    }}
    """
    results = run_sparql_query(query)
    precursors = set(r['precursor']['value'] for r in results if 'precursor' in r)
    logging.info(f"    Retrieved {len(precursors)} Precursor(s) with matching GBUs.")
    return precursors

def get_gbu_requirements_for_assembly_model(assembly_model_uri):
    query = PREFIXES + f"""
    SELECT ?gbuLabel (COUNT(?gbuLabel) AS ?count) WHERE {{
        {construct_uri(assembly_model_uri)} ontocof:hasGenericBuildingUnit ?gbu .
        ?gbu ontocof:hasGBULabel ?gbuLabel .
    }}
    GROUP BY ?gbuLabel
    """
    results = run_sparql_query(query)
    gbu_requirements = {}
    for r in results:
        label = r['gbuLabel']['value']
        count = int(r['count']['value'])
        gbu_requirements[label] = count
    logging.info(f"    GBU Requirements for Assembly Model {assembly_model_uri}: {gbu_requirements}")
    if not gbu_requirements:
        logging.warning(f"    No GBU requirements found for Assembly Model {assembly_model_uri}.")
    return gbu_requirements

def get_binding_site_labels(linkage_uri):
    binding_site_uris = get_binding_sites(linkage_uri)
    if not binding_site_uris:
        return set()

    binding_sites_str = ', '.join(construct_uri(bs) for bs in binding_site_uris)
    query = PREFIXES + f"""
    SELECT DISTINCT ?label WHERE {{
        ?bs ontocof:hasBindingSiteLabel ?label .
        FILTER(?bs IN ({binding_sites_str}))
    }}
    """
    results = run_sparql_query(query)
    labels = set(r['label']['value'] for r in results if 'label' in r)
    logging.info(f"    Retrieved binding site labels for linkage {linkage_uri}: {labels}")
    return labels

def get_precursors_matching_gbu_and_binding(linkage_binding_labels, gbu_label):
    binding_site_labels_str = ', '.join(f'"{label}"' for label in linkage_binding_labels)
    query = PREFIXES + f"""
    SELECT DISTINCT ?precursor WHERE {{
        ?precursor rdf:type ontocof:Precursor .
        ?precursor ontocof:hasBindingSite ?bs .
        ?bs ontocof:hasBindingSiteLabel ?bindingSiteLabel .
        ?precursor ontocof:functionsAsGBU ?gbu .
        ?gbu ontocof:hasGBULabel ?gbuLabel .
        FILTER(?gbuLabel = "{gbu_label}")
        FILTER(?bindingSiteLabel IN ({binding_site_labels_str}))
    }}
    """
    results = run_sparql_query(query)
    precursors = set(r['precursor']['value'] for r in results if 'precursor' in r)
    logging.info(f"    Retrieved {len(precursors)} Precursors with GBU label '{gbu_label}' and required binding sites.")
    return precursors

def get_existing_framework_construct(am_uri, linkage_uri, precursor_uris):
    if not precursor_uris:
        return False
    ask_parts = " ".join([f"?frameworkConstruct ontocof:hasPrecursor {construct_uri(p)} ." for p in precursor_uris])
    query = PREFIXES + f"""
    ASK {{
        ?frameworkConstruct rdf:type ontocof:FrameworkConstruct .
        ?frameworkConstruct ontocof:hasAssemblyModel {construct_uri(am_uri)} .
        ?frameworkConstruct ontocof:hasLinkage {construct_uri(linkage_uri)} .
        {ask_parts}
    }}
    """
    exists = run_sparql_query(query, is_update=False, is_ask=True)
    logging.info(f"    Framework Construct Exists: {exists}")
    return exists

def create_framework_construct(am_uri, linkage_uri, precursor_uris):
    new_cof_uuid = str(uuid.uuid4())
    new_cof_uri = f"https://www.theworldavatar.com/kg/ontocof/FrameworkConstruct_{new_cof_uuid}"

    precursors_triples = " ".join(
        f"{construct_uri(new_cof_uri)} ontocof:hasPrecursor {construct_uri(p)} ."
        for p in precursor_uris
    )
    insert_query = PREFIXES + f"""
    INSERT DATA {{
        {construct_uri(new_cof_uri)} rdf:type ontocof:FrameworkConstruct .
        {construct_uri(new_cof_uri)} ontocof:hasAssemblyModel {construct_uri(am_uri)} .
        {construct_uri(new_cof_uri)} ontocof:hasLinkage {construct_uri(linkage_uri)} .
        {precursors_triples}
    }}
    """
    success = run_sparql_query(insert_query, is_update=True)
    if success:
        logging.info(f"    Created new Framework Construct: {new_cof_uri}")
        return new_cof_uri
    else:
        logging.error(f"    Failed to create Framework Construct: {new_cof_uri}")
        return None

# ---------------------------
# Main Algorithm
# ---------------------------

def discover_new_framework_constructs():
    I = set()  # Newly created Framework Constructs

    logging.info("Starting discovery of new Framework Constructs.")

    # Step 1: Retrieve all Linkages
    A = get_all_linkages()
    logging.info(f"  Total Linkages Retrieved: {len(A)}")

    # Step 2: Iterate over each Linkage
    for linkage_uri in A:
        logging.info(f"\nProcessing Linkage: {linkage_uri}")

        # Retrieve binding sites
        A_bs = get_binding_sites(linkage_uri)
        if len(A_bs) != 2:
            logging.info("    Skipping linkage as it does not have exactly two binding sites.")
            continue

        # Retrieve all precursors that have these binding sites
        B_prime = get_precursors_with_binding_sites(A_bs)
        if not B_prime:
            logging.info("    No Precursors found with the associated Binding Sites.")
            continue

        # Retrieve Linkage_GBU instances
        linkage_gbus = get_linkage_gbus(linkage_uri)
        if not linkage_gbus:
            logging.info("    No Linkage_GBU(s) associated with this Linkage.")
            continue

        # Retrieve Assembly Models associated with the Linkage_GBU(s)
        assembly_models = get_assembly_models_with_linkage_gbus(linkage_gbus)
        if not assembly_models:
            logging.info("    No Assembly Models associated with the Linkage_GBU(s).")
            continue

        # Retrieve GBUs associated with these Assembly Models
        E_prime = get_gbus_with_assembly_models(assembly_models)
        if not E_prime:
            logging.info("    No GBUs associated with the Assembly Models.")
            continue

        # Retrieve all precursors that can function as these GBUs
        B_double_prime = get_precursors_with_gbus(E_prime)
        if not B_double_prime:
            logging.info("    No Precursors found with the associated GBUs.")
            continue

        # Intersect to find common precursors that appear in both sets
        G = B_prime.intersection(B_double_prime)
        logging.info(f"    Intersection of Precursors (G): {G}")

        if not G:
            logging.info("    No common Precursors found between Binding Sites and GBUs.")
            continue

        # Retrieve binding site labels from the linkage (needed for final combination step)
        binding_site_labels = get_binding_site_labels(linkage_uri)

        # For each Assembly Model, determine GBU requirements
        for am_uri in assembly_models:
            gbu_requirements = get_gbu_requirements_for_assembly_model(am_uri)
            num_gbus_required = len(gbu_requirements)

            if num_gbus_required == 1:
                label = next(iter(gbu_requirements))
                required_count = gbu_requirements[label]

                precursors_matching = get_precursors_matching_gbu_and_binding(binding_site_labels, label)

                if len(precursors_matching) < required_count:
                    logging.warning(f"      Not enough precursors for GBU label '{label}' in Assembly Model '{am_uri}'. Required: {required_count}, Available: {len(precursors_matching)}")
                    continue

                combinations = list(itertools.combinations(precursors_matching, required_count))
                logging.info(f"      Generated {len(combinations)} combination(s) for GBU label '{label}'.")

                for combo in combinations:
                    exists = get_existing_framework_construct(am_uri, linkage_uri, frozenset(combo))
                    if exists:
                        logging.info(f"      Framework Construct for Assembly Model {am_uri} and precursors {combo} already exists.")
                        continue
                    new_cof_uri = create_framework_construct(am_uri, linkage_uri, combo)
                    if new_cof_uri:
                        I.add(new_cof_uri)

            elif num_gbus_required == 2:
                labels = list(gbu_requirements.keys())
                if len(labels) != 2:
                    logging.warning(f"      Expected 2 GBU labels for dual GBU Assembly Model '{am_uri}', found {len(labels)}.")
                    continue
                label1, label2 = labels
                count1, count2 = gbu_requirements[label1], gbu_requirements[label2]

                if not binding_site_labels:
                    logging.warning(f"      No binding site labels found for Linkage '{linkage_uri}'. Skipping.")
                    continue

                precursors_label1 = get_precursors_matching_gbu_and_binding(binding_site_labels, label1)
                precursors_label2 = get_precursors_matching_gbu_and_binding(binding_site_labels, label2)

                if len(precursors_label1) < count1 or len(precursors_label2) < count2:
                    logging.warning(f"      Not enough precursors for GBU labels '{label1}' or '{label2}' in Assembly Model '{am_uri}'.")
                    continue

                combinations_label1 = list(itertools.combinations(precursors_label1, count1))
                combinations_label2 = list(itertools.combinations(precursors_label2, count2))
                logging.info(f"      Generated {len(combinations_label1)} combination(s) for GBU label '{label1}'.")
                logging.info(f"      Generated {len(combinations_label2)} combination(s) for GBU label '{label2}'.")

                all_combinations = itertools.product(combinations_label1, combinations_label2)
                for combo_pair in all_combinations:
                    combo_precursors = set(combo_pair[0]).union(set(combo_pair[1]))
                    exists = get_existing_framework_construct(am_uri, linkage_uri, frozenset(combo_precursors))
                    if exists:
                        logging.info(f"      Framework Construct for Assembly Model {am_uri} and precursors {combo_precursors} already exists.")
                        continue
                    new_cof_uri = create_framework_construct(am_uri, linkage_uri, combo_precursors)
                    if new_cof_uri:
                        I.add(new_cof_uri)
            else:
                logging.warning(f"      Unsupported number of GBUs ({num_gbus_required}) in Assembly Model '{am_uri}'. Skipping.")
                continue

    return I  # Make sure to return I at the end of the function

# ---------------------------
# Main Function
# ---------------------------

def main():
    logging.info("Starting Framework Construct Discovery Algorithm...")
    new_framework_constructs = discover_new_framework_constructs()
    logging.debug(f"discover_new_framework_constructs() returned: {new_framework_constructs}")
    logging.info(f"\nDiscovery complete. Number of new Framework Constructs found: {len(new_framework_constructs)}")

    if new_framework_constructs:
        logging.info("New Framework Constructs:")
        for fc in new_framework_constructs:
            logging.info(fc)
    else:
        logging.info("No new Framework Constructs were discovered.")

if __name__ == "__main__":
    main()
