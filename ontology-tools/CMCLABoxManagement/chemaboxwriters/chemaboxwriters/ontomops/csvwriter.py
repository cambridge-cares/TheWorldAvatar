# -*- coding: utf-8 -*-
"""
Created on Thu Mar  4 16:10:02 2021

@author: angir
"""

import json
import csv

from io import StringIO
import chemaboxwriters.common.commonvars as commonv
from chemaboxwriters.common import PREFIXES

onto_spec = PREFIXES["onto_spec"]
onto_mops = PREFIXES["onto_mops"]
mops_pref = PREFIXES["mops_pref"]
rdf_pref = PREFIXES["rdf_pref"]
uom_pref = PREFIXES["uom_pref"]
unres_pref = PREFIXES["unres_pref"]                                              

def om_csvwriter(data):
    data = json.loads(data)
    gen_id = data[commonv.ENTRY_UUID]

    csvfile = StringIO(newline='')

    spamwriter = csv.writer(csvfile, delimiter=',',
                            quotechar='"', quoting=csv.QUOTE_MINIMAL)

    mops_id = data[commonv.ENTRY_IRI]


    spamwriter = csv.writer(csvfile, delimiter=',',
                                quotechar='|', quoting=csv.QUOTE_MINIMAL)
    spamwriter.writerow(['Source', 'Type', 'Target', 'Relation','Value','Data Type'])

    #Write the main initialization for now.
    spamwriter.writerow(['ABoxOntoMOPs','Ontology',onto_mops,'http://www.w3.org/2002/07/owl#imports','',''])
    spamwriter.writerow(['ABoxOntoMOPs','Ontology',mops_pref,'base','',''])
    spamwriter.writerow([mops_pref + mops_id,'Instance',onto_mops + '#MetalOrganicPolyhedra',
                            '','',''])

    #Write the properties directly connected to MOPS instance that then terminate.
    spamwriter.writerow([rdf_pref + '#label','Data Property',mops_pref +  mops_id,
    '',data["Mops_Label"],'String']) #label for the MOP
    spamwriter.writerow([onto_mops + '#hasMOPFormula','Data Property',mops_pref +  mops_id,
    '',data["Mops_Formula"],'String']) #Chemical formula for the MOP
    spamwriter.writerow([onto_mops + '#hasCCDCNumber','Data Property',mops_pref + mops_id,
    '',data["Mops_CCDC_Number"],'String']) #CCDC No. for the MOP
    #spamwriter.writerow([onto_mops + '#hasXYZGeometry','Data Property',mops_pref + mops_id,
    #'',data["Mops_Geometry"],'String']) #XYZ Geometry in string form for the MOP.

    #Write the Provenance of the MOPs.
    spamwriter.writerow([mops_pref + 'Provenance_' + gen_id, 'Instance', onto_mops + '#Provenance',
    '','','']) #Initialize the Provenance object for the MOPs.
    spamwriter.writerow([mops_pref +  mops_id,'Instance',mops_pref + 'Provenance_' + gen_id,
    onto_mops + '#hasProvenance','','']) # Connect the Provenance to the MOPs instance.
    spamwriter.writerow([onto_mops + '#hasReferenceDOI','Data Property',mops_pref + 'Provenance_' + gen_id,
    '',data["Mops_Reference_DOI"],'String'])

    #Write the Molecular Weight section for the MOPs.
    spamwriter.writerow([mops_pref + 'MolecularWeight_' + gen_id, 'Instance', onto_spec + '#MolecularWeight',
    '','','']) #Initialize the Molecular Weight object for the MOPs.
    spamwriter.writerow([mops_pref + mops_id, 'Instance', mops_pref + 'MolecularWeight_' + gen_id,
    onto_spec + '#hasMolecularWeight','','']) #Link the Molecular Weight object to the MOPs.
    spamwriter.writerow([uom_pref + 'Measure_MolecularWeight_' + gen_id, 'Instance', uom_pref + 'Measure',
     '','','']) #This is the Measure from the Ontology of Units of Measure that we will use for Molecular Weight.
    spamwriter.writerow([mops_pref + 'MolecularWeight_' + gen_id, 'Instance',uom_pref + 'Measure_MolecularWeight_' + gen_id,
     uom_pref + 'hasValue','','']) #Link the Measure to the Molecular Weight instance.
    spamwriter.writerow([uom_pref + 'hasNumericalValue','Data Property',uom_pref + 'Measure_MolecularWeight_' + gen_id,
    '',data["Mops_Molecular_Weight"],'String']) #Link the Numerical Value of Molecular Weight to the Measure.
    spamwriter.writerow([uom_pref + 'MolarMassUnit', 'Instance', uom_pref + 'Unit',
    '','','']) #Take the MolarMass Unit instance from the UOM ontology.
    spamwriter.writerow([uom_pref + 'Measure_MolecularWeight_' + gen_id, 'Instance',uom_pref + 'MolarMassUnit',
    uom_pref + 'hasUnit','',''])

    #Write the Charge section for the MOPs.
    spamwriter.writerow([mops_pref + 'Charge_' + gen_id, 'Instance', onto_spec + '#Charge',
    '','','']) #Initialize the Charge object for the MOPs.
    spamwriter.writerow([mops_pref + mops_id, 'Instance', mops_pref + 'Charge_' + gen_id,
    onto_spec + '#hasCharge','','']) #Link the Charge object to the MOPs.
    spamwriter.writerow([uom_pref + 'Measure_Charge_' + gen_id, 'Instance', uom_pref + 'Measure',
     '','','']) #This is the Measure from the Ontology of Units of Measure that we will use for Charge.
    spamwriter.writerow([mops_pref + 'Charge_' + gen_id, 'Instance',uom_pref + 'Measure_Charge_' + gen_id,
     uom_pref + 'hasValue','','']) #Link the Measure to the Charge instance.
    spamwriter.writerow([uom_pref + 'hasNumericalValue','Data Property',uom_pref + 'Measure_Charge_' + gen_id,
    '',data["Mops_Charge"],'String']) #Link the Numerical Value of Charge to the Measure.
    spamwriter.writerow([unres_pref + 'elementary_charge', 'Instance', uom_pref + 'Unit',
    '','','']) #Take the elementary charge Unit instance from our extension of the UOM ontology.
    spamwriter.writerow([uom_pref + 'Measure_Charge_' + gen_id, 'Instance',unres_pref + 'elementary_charge',
    uom_pref + 'hasUnit','',''])

    #Write the Cavity section for the MOPs.
    spamwriter.writerow([mops_pref + 'Cavity_' + gen_id, 'Instance', onto_mops + '#Cavity',
    '','','']) #Initialize the Cavity object for the MOPs.
    spamwriter.writerow([mops_pref + mops_id, 'Instance', mops_pref + 'Cavity_' + gen_id,
    onto_mops + '#hasCavity','','']) #Link the Cavity object to the MOPs.
    spamwriter.writerow([mops_pref + 'Volume_' +gen_id, 'Instance', onto_mops + '#Volume',
    '','','']) #Initialize the Volume object for the MOPs.
    spamwriter.writerow([mops_pref + 'Cavity_' + gen_id, 'Instance', mops_pref + 'Volume_' + gen_id,
    onto_mops + '#hasMOPCavityVolume','','']) #Link the Volume object to the cavity.
    spamwriter.writerow([uom_pref + 'Measure_Volume_' + gen_id, 'Instance', uom_pref + 'Measure',
     '','','']) #This is the Measure from the Ontology of Units of Measure that we will use for Volume.
    spamwriter.writerow([mops_pref + 'Volume_' + gen_id, 'Instance',uom_pref + 'Measure_Volume_' + gen_id,
     uom_pref + 'hasValue','','']) #Link the Measure to the Volume instance.
    spamwriter.writerow([uom_pref + 'hasNumericalValue','Data Property',uom_pref + 'Measure_Volume_'  +gen_id,
    '',data["Mops_CavityVolume"],'String']) #Link the Numerical Value of Volume to the Measure.
    spamwriter.writerow([uom_pref + 'cubicNanometre', 'Instance', uom_pref + 'Unit',
    '','','']) #Take the Cubic Nanometre Unit instance from the UOM ontology.
    spamwriter.writerow([uom_pref + 'Measure_Volume_' + gen_id, 'Instance',uom_pref + 'cubicNanometre',
    uom_pref + 'hasUnit','',''])

    #Write the Assembly Model initialization and shape/symmetry related instances.
    spamwriter.writerow([mops_pref + 'AssemblyModel_' + gen_id, 'Instance', onto_mops + '#AssemblyModel',
    '','','']) #Initialize the Assembly Model object for the MOPs.
    spamwriter.writerow([mops_pref + mops_id, 'Instance', mops_pref + 'AssemblyModel_' + gen_id,
    onto_mops + '#hasAssemblyModel','','']) #Connect the MOPs instance to the Assembly Model instance.
    spamwriter.writerow([onto_mops + '#hasSymmetryPointGroup', 'Data Property', mops_pref + 'AssemblyModel_' + gen_id,'',
    data["Mops_Symmetry_Point_Group"],'String']) #Write the Symmetry point group for the MOPs.
    spamwriter.writerow([mops_pref + data["Mops_Polyhedral_Shape"] + '_' + gen_id, 'Instance', onto_mops + '#' + data["Mops_Polyhedral_Shape"],
    '','','']) #Initialize an instance of Polyhedral Shape that is the given shape from the JSON file.
    spamwriter.writerow([mops_pref + 'AssemblyModel_' + gen_id, 'Instance', mops_pref + data["Mops_Polyhedral_Shape"] + '_' + gen_id,
    onto_mops + '#hasPolyhedralShape','','']) #Connect the Assembly model to polyhedral shape.
    spamwriter.writerow([onto_mops + '#hasSymbol','Data Property',mops_pref + data["Mops_Polyhedral_Shape"] + '_' + gen_id,''
    ,data["Mops_Polyhedral_Shape_Symbol"],'String'])

    #Write the information about the Chemical and Generic Building units.
    for i in range(len(data["Mops_Chemical_Building_Units"])): #We will loop through all the building units in the JSON.
        spamwriter.writerow([mops_pref + 'ChemicalBuildingUnit_' + gen_id + '_' + str(i), 'Instance', onto_mops + '#ChemicalBuildingUnit',
        '','','']) #Instantiate the Chemical Building Unit.
        spamwriter.writerow([mops_pref + mops_id, 'Instance', mops_pref + 'ChemicalBuildingUnit_' + gen_id + '_' + str(i),
        onto_mops + '#hasChemicalBuildingUnit','','']) #Connect the CBU instance to the MOPs instance.
        spamwriter.writerow([onto_mops + '#hasCBUFormula','Data Property',mops_pref + 'ChemicalBuildingUnit_' + gen_id + '_' + str(i),
        '',data["Mops_Chemical_Building_Units"][i]["CBU_Formula"],'']) #CBU Formula
        spamwriter.writerow([mops_pref + data["Mops_Chemical_Building_Units"][i]["BindingDirection"] +"Binding_" + gen_id,
        'Instance',onto_mops + '#' +  data["Mops_Chemical_Building_Units"][i]["BindingDirection"] + 'Binding','','','']) #Instantiate the binding direction for the CBU
        spamwriter.writerow([mops_pref + 'ChemicalBuildingUnit_' + gen_id + '_' + str(i), 'Instance',
        mops_pref + data["Mops_Chemical_Building_Units"][i]["BindingDirection"] +"Binding_" + gen_id, onto_mops + '#hasBindingDirection','',''])
        #Connect Binding direction instance to CBU instance.
        spamwriter.writerow([mops_pref + 'ChemicalBuildingUnit_' + gen_id + '_' + str(i), 'Instance',
        data["Mops_Chemical_Building_Units"][i]["OntoSpecies_IRI"],onto_spec + '#hasUniqueSpecies','','']) #Connect CBU to OntoSpecies entry.

        spamwriter.writerow([mops_pref + data["Mops_Chemical_Building_Units"][i]["Binding_Site"] +"Site_" + gen_id,
        'Instance',onto_mops + '#' + data["Mops_Chemical_Building_Units"][i]["Binding_Site"] + 'Site','','','']) #Instantiate the binding site for the CBU.
        spamwriter.writerow([mops_pref + 'ChemicalBuildingUnit_' + gen_id + '_' + str(i), 'Instance',
        mops_pref + data["Mops_Chemical_Building_Units"][i]["Binding_Site"] +"Site_" + gen_id, onto_mops + '#hasBindingSite','',''])
        #Connect Binding site instance to CBU instance.
        spamwriter.writerow([rdf_pref + '#label','Data Property', mops_pref + data["Mops_Chemical_Building_Units"][i]["Binding_Site"] +"Site_" + gen_id,
        '',data["Mops_Chemical_Building_Units"][i]["Binding_Site_Label"],'String']) #label for the Binding Site.
        spamwriter.writerow([onto_mops + '#hasOuterCoordinationNumber','Data Property',mops_pref + data["Mops_Chemical_Building_Units"][i]["Binding_Site"] +"Site_" + gen_id,
        '',data["Mops_Chemical_Building_Units"][i]["Binding_SiteCoordNumber"],'String'])

        spamwriter.writerow([mops_pref + 'Core_' + gen_id + '_' + str(i),'Instance', onto_mops + '#Core','','',''])
        # Instantiate the Core for this CBU.
        spamwriter.writerow([mops_pref + 'ChemicalBuildingUnit_' + gen_id + '_' + str(i), 'Instance',
        mops_pref + 'Core_' + gen_id + '_' + str(i), onto_mops + '#hasCore','','']) #Connect the Core instance to the CBU instance.
        spamwriter.writerow([rdf_pref + '#label','Data Property',mops_pref + 'Core_' + gen_id + '_' + str(i),
        '',data["Mops_Chemical_Building_Units"][i]["CoreLabel"],'String']) #Attach label to Core.
        spamwriter.writerow([mops_pref + 'Substituent_Core_'  + gen_id + '_' + str(i),'Instance', onto_mops + '#Substituent'
        ,'','','']) #Instantiate the Core Substituent.
        spamwriter.writerow([mops_pref + 'Core_' + gen_id + '_' + str(i),'Instance',mops_pref + 'Substituent_Core_'  + gen_id + '_' + str(i)
        ,onto_mops + '#hasSubstituent','','']) #Connect the Core Substituent to the Core.
        spamwriter.writerow([rdf_pref + '#label','Data Property',mops_pref + 'Substituent_Core_'  + gen_id + '_' + str(i),
        '',data["Mops_Chemical_Building_Units"][i]["CoreSubstituentLabel"],'String']) #Attach label to Core Substituent.

        spamwriter.writerow([mops_pref + 'Spacer_' + gen_id + '_' + str(i),'Instance', onto_mops + '#Spacer','','',''])
        # Instantiate the Spacer for this CBU.
        spamwriter.writerow([mops_pref + 'ChemicalBuildingUnit_' + gen_id + '_' + str(i), 'Instance',
        mops_pref + 'Spacer_' + gen_id + '_' + str(i), onto_mops + '#hasSpacer','','']) #Connect the Spacer instance to the CBU instance.
        spamwriter.writerow([rdf_pref + '#label','Data Property',mops_pref + 'Spacer_' + gen_id + '_' + str(i),
        '',data["Mops_Chemical_Building_Units"][i]["SpacerLabel"],'String']) #Attach label to Spacer.
        spamwriter.writerow([mops_pref + 'Substituent_Spacer_'  + gen_id + '_' + str(i),'Instance', onto_mops + '#Substituent'
        ,'','','']) #Instantiate the Spacer Substituent.
        spamwriter.writerow([mops_pref + 'Spacer_' + gen_id + '_' + str(i),'Instance',mops_pref + 'Substituent_Spacer_'  + gen_id + '_' + str(i)
        ,onto_mops + '#hasSubstituent','','']) #Connect the Spacer Substituent to the Core.
        spamwriter.writerow([rdf_pref + '#label','Data Property',mops_pref + 'Substituent_Spacer_'  + gen_id + '_' + str(i),
        '',data["Mops_Chemical_Building_Units"][i]["SpacerSubstituentLabel"],'String']) #Attach label to Spacer Substituent.

        spamwriter.writerow([mops_pref + 'GenericBuildingUnit_' + gen_id + '_' + str(i), 'Instance', onto_mops + '#GenericBuildingUnit',
        '','','']) #Instantiate the corresponding Generic Building Unit.
        spamwriter.writerow([mops_pref + 'AssemblyModel_' + gen_id, 'Instance', mops_pref + 'GenericBuildingUnit_' + gen_id + '_' + str(i),
        onto_mops + '#hasGenericBuildingUnit','','']) #Connect the GBU instance to the Assembly Model instance.
        spamwriter.writerow([onto_mops + '#hasPlanarity','Data Property',mops_pref + 'GenericBuildingUnit_' + gen_id + '_' + str(i),
        '',data["Mops_Chemical_Building_Units"][i]["GenericUnitPlanarity"],'String']) #Planarity of GBU.
        spamwriter.writerow([onto_mops + '#hasModularity','Data Property',mops_pref + 'GenericBuildingUnit_' + gen_id + '_' + str(i),
        '',data["Mops_Chemical_Building_Units"][i]["GenericUnitModularity"],'String']) #Modularity of GBU.
        spamwriter.writerow([mops_pref + 'GenericBuildingUnitNumber_' + gen_id + '_' + str(i), 'Instance', onto_mops + '#GenericBuildingUnitNumber',
        '','','']) #Instantiate the corresponding Generic Building Unit Number.
        spamwriter.writerow([mops_pref + 'AssemblyModel_' + gen_id, 'Instance', mops_pref + 'GenericBuildingUnitNumber_' + gen_id + '_' + str(i),
        onto_mops + '#hasGenericBuildingUnitNumber','','']) #Connect the GBU Number instance to the Assembly Model instance.
        spamwriter.writerow([mops_pref + 'GenericBuildingUnitNumber_' + gen_id + '_' + str(i), 'Instance',
        mops_pref + 'GenericBuildingUnit_' + gen_id + '_' + str(i),onto_mops + '#isNumberOf','','']) #Connect the GBU Number to its GBU.
        spamwriter.writerow([onto_spec + '#value','Data Property',mops_pref + 'GenericBuildingUnitNumber_' + gen_id + '_' + str(i)
        ,'',data["Mops_Chemical_Building_Units"][i]["GenericUnitNumber"],'String']) #Give the GBU Number its value.

        spamwriter.writerow([mops_pref + 'ChemicalBuildingUnit_' + gen_id + '_' + str(i), 'Instance',
        mops_pref + 'GenericBuildingUnit_' + gen_id + '_' + str(i),onto_mops + '#isFunctioningAs','','']) #Connect the CBU to its corresonding GBU


    csvcontent = csvfile.getvalue()
    csvfile.close()
    return [csvcontent]