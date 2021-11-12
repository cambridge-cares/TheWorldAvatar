def queryBuildingUnitsTemplate(modularity, planarity):
    """This query searches for chemical building units that posses the same modularity and planarity as the generic builing units of the assembly model."""

    queryStr = """
    PREFIX ontomops: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
    PREFIX ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX purl: <http://purl.org/dc/elements/1.1/identifier>

    SELECT ?BuildingUnit ?BuildingUnit_Formula ?Binding_Functionality ?Directionality ?Modularity_Type ?Planarity_Type
    WHERE
    {?MetalOrganicPolyhedron ontomops:hasBuildingUnit ?BuildingUnit .
    ?BuildingUnit ontospecies:hasMolecularFormula ?BuildingUnit_Formula .
    ?BuildingUnit ontokin:hasUniqueSpecies ?Species .
    ?BuildingUnit ontomops:hasModularity ?Modularity .
    ?Modularity ontospecies:value ?Modularity_Type .
    ?Modularity ontospecies:value #MODVALUE .
    ?BuildingUnit ontomops:hasPlanarity ?Planarity .
    ?Planarity  ontospecies:value ?Planarity_Type .
    ?Planarity  ontospecies:value "#PLANVALUE" .
    ?BuildingUnit ontomops:hasBindingSite ?BindingSite .
    ?BindingSite  ontospecies:value ?Binding_Functionality .
    ?BuildingUnit ontomops:hasBindingDirection ?BindingDirection .
    ?BindingDirection  ontospecies:value ?Directionality .
    }"""
    queryStr = queryStr.replace('#MODVALUE', str(modularity))
    queryStr = queryStr.replace('#PLANVALUE', str(planarity))
    return queryStr

def queryAssemblyModel(assemblyModelString):
    """This query allocates MOPs of a particular assembly model."""
    queryStr2 = """
    PREFIX ontomops: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
    PREFIX ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX purl: <http://purl.org/dc/elements/1.1/identifier>

    SELECT ?MetalOrganicPolyhedron ?Model ?MOP ?Charge
    WHERE
    {?MetalOrganicPolyhedron ontomops:hasChemicalFormula ?ChemicalFormula .
     ?ChemicalFormula ontospecies:value ?MOP .
     ?MetalOrganicPolyhedron ontospecies:hasCharge ?MOPCharge .
     ?MOPCharge ontospecies:value ?Charge .
     ?MetalOrganicPolyhedron ontomops:hasPolyhedralShape ?PolyhedralShape .
     ?PolyhedralShape ontospecies:value ?Model .
     ?PolyhedralShape ontospecies:value "#MODELVALUE" .
     FILTER ((?Model) = "#MODELVALUE") .
    }
    """
    queryStr2 = queryStr2.replace('#MODELVALUE', str(assemblyModelString))
    return queryStr2

def queryMOPFormula(mopFormula):
    """This query is allocating the chemical building units from the queried MOPs from query 2"""
    queryStr3 = """
    PREFIX ontomops: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
    PREFIX ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX purl: <http://purl.org/dc/elements/1.1/identifier>

    SELECT ?BuildingUnitFormula ?MOP
    WHERE
    {?MetalOrganicPolyhedron ontomops:hasBuildingUnit ?BuildingUnit .
     ?BuildingUnit ontospecies:hasMolecularFormula ?BuildingUnitFormula .
     ?MetalOrganicPolyhedron ontomops:hasChemicalFormula ?ChemicalFormula .
     ?ChemicalFormula ontospecies:value ?MOP .
     ?ChemicalFormula ontospecies:value "#MOPFORMULA"

    }
    """
    queryStr3 = queryStr3.replace('#MOPFORMULA', str(mopFormula))
    return queryStr3