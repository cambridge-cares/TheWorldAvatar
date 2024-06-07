import csv
import sys
import uuid
import os
import re

import UpdateKG as KG

# Define the XML template
xml_template = '''<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF
   xmlns:ns1="http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#"
   xmlns:ns2="http://www.ontology-of-units-of-measure.org/resource/om-2/"
   xmlns:ns3="http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#"
   xmlns:ns4="http://www.w3.org/2002/07/owl#"
   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
>
    <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ontomops/MetalOrganicPolyhedra_{id_hash}">
        <ns1:hasMOPFormula rdf:datatype="http://www.w3.org/2001/XMLSchema#string">{mop_formula}</ns1:hasMOPFormula>
        <rdfs:label rdf:datatype="http://www.w3.org/2001/XMLSchema#string">None</rdfs:label>
        <ns1:hasProvenance rdf:resource="http://www.theworldavatar.com/kb/ontomops/Provenance_f1518a4f-652a-4fd7-a418-6676e1bccb6d"/>
        <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#MetalOrganicPolyhedra"/>
        <ns3:hasCharge rdf:resource="http://www.theworldavatar.com/kb/ontomops/Charge_{id_hash}"/>
        <ns3:hasMolecularWeight rdf:resource="http://www.theworldavatar.com/kb/ontomops/MolecularWeight_{id_hash}"/>
        <ns1:hasAssemblyModel rdf:resource="{am_id}"/>
        <ns1:hasChemicalBuildingUnit rdf:resource="{cbu_iri_1}"/>
        <ns1:hasChemicalBuildingUnit rdf:resource="{cbu_iri_2}"/>
    </rdf:Description>
    <rdf:Description rdf:about="http://www.ontology-of-units-of-measure.org/resource/om-2/Measure_MolecularWeight_{id_hash}">
        <rdf:type rdf:resource="http://www.ontology-of-units-of-measure.org/resource/om-2/Measure"/>
        <ns2:hasUnit rdf:resource="http://www.ontology-of-units-of-measure.org/resource/om-2/MolarMassUnit"/>
        <ns2:hasNumericalValue rdf:datatype="http://www.w3.org/2001/XMLSchema#string">{mop_mw}</ns2:hasNumericalValue>
    </rdf:Description>
        <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ontomops/MolecularWeight_{id_hash}">
        <ns2:hasValue rdf:resource="http://www.ontology-of-units-of-measure.org/resource/om-2/Measure_MolecularWeight_{id_hash}"/>
        <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#MolecularWeight"/>
    </rdf:Description>
    <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ontomops/Charge_{id_hash}">
        <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Charge"/>
        <ns2:hasValue rdf:resource="http://www.ontology-of-units-of-measure.org/resource/om-2/Measure_Charge_{id_hash}"/>
    </rdf:Description>
    <rdf:Description rdf:about="http://www.ontology-of-units-of-measure.org/resource/om-2/Measure_Charge_{id_hash}">
        <ns2:hasUnit rdf:resource="http://theworldavatar.com/resource/ontouom/elementary_charge"/>
        <ns2:hasNumericalValue rdf:datatype="http://www.w3.org/2001/XMLSchema#string">{mop_charge}</ns2:hasNumericalValue>
        <rdf:type rdf:resource="http://www.ontology-of-units-of-measure.org/resource/om-2/Measure"/>
    </rdf:Description>
    <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ontomops/Provenance_f1518a4f-652a-4fd7-a418-6676e1bccb6d">
        <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#Provenance"/>
        <ns1:hasReferenceDOI rdf:datatype="http://www.w3.org/2001/XMLSchema#string">"Not in OntoMOPs KG"</ns1:hasReferenceDOI>
    </rdf:Description>
</rdf:RDF>
'''

def main(filename, output_folder):
    # Create the output folder if it doesn't exist
    os.makedirs(output_folder, exist_ok=True)
    counter     = 0
    SPARQL_QUERY_ENDPOINT       = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    SPARQL_UPDATE_ENDPOINT      = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    KG_USERNAME                 = 'bg_user'
    KG_PASSWORD                 = 'admin'
    updaterCBU                  = KG.UpdateKG(
                query_endpoint          = SPARQL_QUERY_ENDPOINT,
                update_endpoint         = SPARQL_UPDATE_ENDPOINT,
                kg_user                 = KG_USERNAME,
                kg_password             = KG_PASSWORD
                )
    # Read the CSV file
    with open(filename, mode='r') as file:
        csv_reader                                      = csv.DictReader(file)
        cbu_mapping                                     = {}
        am_mapping                                      = {}
        for k, row in enumerate(csv_reader):
            print(f"row={row}")
            assembly_model_value, point_group_value     = row['Assembly Model'].split('___')
            mop_formula_value                           = row['MOP Formula']
            mop_charge_value                            = row['MOP Charge']
            mop_mw_value                                = row['MOP MW']
            id_hash_value                               = str(uuid.uuid4())
            Provenance                                  = row['ReferenceDOI']
            # dictionairy for wrongly assigned am:
            assembly_model_map = {
                "(3-planar)x4(3-pyramidal)x4": "(3-pyramidal)x4(3-planar)x4",
                "(3-planar)x4(2-bent)x6": "(2-bent)x6(3-planar)x4",
                "(3-planar)x8(2-bent)x12":"(2-bent)x12(3-planar)x8"
            }
            cbu_model_map = {
                "(C5NH3)2(CO2)2": "(C5H3N)2(CO2)2"
            }

            # Replace the value if it exists in the dictionary
            if assembly_model_value in assembly_model_map:
                assembly_model_value = assembly_model_map[assembly_model_value]

            # remove "("" and ")"
            point_group_value                           = point_group_value.replace("(", "").replace(")", "")
            # deduce cbu
            cbu_iris                                    = []
            cbus                                        = re.findall(r'\[([^\[\]]+)\]', mop_formula_value)
            # kind of cache the used cbus and ams to reduce the required number of queries.
            for cbu in cbus:
                if cbu in cbu_model_map:
                    cbu                                 = cbu_model_map[cbu]
                if cbu in cbu_mapping:
                    cbu_iri                             = cbu_mapping[cbu]
                else:
                    vars_cbu                            = " ?CBUIRI"
                    where_cbu                           = f"""?CBUIRI     om:hasCBUFormula    "[{cbu}]"  ."""
                    cbu_iri                             = updaterCBU.query_triple(where_cbu, vars_cbu)[0]
                    # change approach and write new entry if there is no matching cbu
                    if len(cbu_iri) != 1:
                        print(f"Error for row:{row}, cbu: {cbu}, cbu iris: {cbu_iri}.")
                        raise Exception("Multiple possible IRIs")
                    cbu_mapping[cbu] = cbu_iri
                cbu_iris.append(cbu_iri["CBUIRI"])
            if assembly_model_value in am_mapping:
                am_iri                                  = am_mapping[assembly_model_value]
            else:   
                # get am iri            
                where_am            =   f"""
                                        ?MOPIRI         om:hasAssemblyModel     ?AMIRI                  .
                                        ?AMIRI       	os:value                "{assembly_model_value}"  .
                                        """
                var_am              =   " DISTINCT ?AMIRI"
                am_iri              = updaterCBU.query_triple(where_am, var_am)[0]
                print(f"am iri: {am_iri}, {assembly_model_value}")
            # Create the XML content
            xml_content             = xml_template.format(
                id_hash             = id_hash_value,
                mop_formula         = mop_formula_value,
                mop_charge          = mop_charge_value,
                mop_mw              = mop_mw_value,
                point_group         = point_group_value,
                assembly_model      = assembly_model_value,
                cbu_iri_1           = cbu_iris[0],
                cbu_iri_2           = cbu_iris[1],
                am_id               = am_iri["AMIRI"]
            )
            # Define the output file path, putting this distinction earlier would save time and 
            # queries but with this there is an additional testing benefit
            if Provenance=="Not in OntoMOPs KG":
                output_file_path = os.path.join(output_folder, f'{153+counter}.ominp.json.om.owl')
                # Write the XML content to the file
                with open(output_file_path, 'w') as xml_file:
                    xml_file.write(xml_content)
                counter += 1
                print(f'Generated XML file: {output_file_path}')
    print(f"There have been: {counter} files generated!")
if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python read_csv.py <filename> <output_folder>")
        sys.exit(1)

    filename = sys.argv[1]
    output_folder = sys.argv[2]
    main(filename, output_folder)
