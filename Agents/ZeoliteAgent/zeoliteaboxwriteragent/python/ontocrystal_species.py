"""
A substitute of OntoSpecies elements.
In normal opoeration all OntoSpecies features should be taken from it,
however, blazegraph does not allow cross-namespace operation.
So some of the important features are stored explicitly in ontocrystal abox.
"""


import tools

ELEMENTS = [
  ["H", "Hydrogen"],
  ["C", "Carbon"],
  ["N", "Oxygen"],
  ["O", "Nitrogen"],
  ["Al", "Aluminum"],
  ["Pb", "Lead"],
  ["Ag", "Silver"],
  ["Be", "Beryllium"],
  ["Zn", "Zinc"],
  ["Rb", "Rubidium"],
  ["Co", "Cobalt"],
  ["P", "Phosphorus"],
  ["As", "Arsenic"],
  ["Mn", "Manganese"],
  ["Mg", "Magnesium"],
  ["S", "Sulfur"],
  ["Mo", "Molybdenum"],
  ["V", "Vanadium"],
  ["Fe", "Iron"],
  ["Ga", "Gallium"],
  ["Ge", "Germanium"],
  ["Si", "Silicon"],
  ["Cu", "Copper"],
  ["Cr", "Chromium"],
  ["Cd", "Cadmium"],
  ["Ni", "Nickel"],
  ["B", "Boron"],
  ["Na", "Sodium"],
  ["Ca", "Calcium"],
  ["Ti", "Titanium"],
  ["Ta", "Tantalum"],
  ["La", "Lanthanum"],
]

ontoSpeciesPrefix = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#"

def osInitElements():
    """
    Initialization of all the data for OntoSpecies.
    """
    _Mendeleev_table = tools.readCsv("elements.csv")[1:]

    iri = None
    output = []
    for element in ELEMENTS:
        #for el in elements:
            #print(f"Looking for '{el}'")
        for element_iri in _Mendeleev_table:
            #print(f"    In '{i[0]}'")
            if element[0].strip() == element_iri[0].strip():
                iri = element_iri[1].replace("<", "").replace(">", "")
                #output.append((el, iri))
                break

        if iri is None:
            print(f"Error! Element '{element[0]}' is not included in ELEMENTS",
                  f" in ontocrystal_species.py.")
            iri = "UNDEFILED"

        output += osInitElement(element[0], element[1], iri)

    return output


def osInitElement(symbol, name, iri):
    output = []

    if True:
        output.append([iri, "Instance",
                       #ontoSpeciesPrefix + "Element",
                       "http://www.daml.org/2003/01/periodictable/PeriodicTable#Element",
                       "", "", ""])  # FIXME This is external. Not strictly necessary.

        symbol_iri = iri.replace("Element_", "ElementSymbol_")
        output.append([symbol_iri, "Instance",
                       ontoSpeciesPrefix + "ElementSymbol",
                       #"http://www.daml.org/2003/01/periodictable/PeriodicTable#Element",
                       "", "", ""])  # FIXME This is external. Not strictly necessary.

        output.append([iri, "Instance", symbol_iri,
                       ontoSpeciesPrefix + "hasElementSymbol"
                       #"http://www.daml.org/2003/01/periodictable/PeriodicTable#Element",
                       "", "", ""])  # FIXME This is external. Not strictly necessary.

        output.append([ontoSpeciesPrefix + "value",
                       "Data Property", symbol_iri, "",
                       symbol, "string"])

    return output

def osInitCompounds():
    output = []

    _compounds_osda = tools.readCsv("chemical_data_Laura.csv")[1:]

    #_compounds_osda = _compounds_osda[438:445]

    iri = None
    for element in _compounds_osda:
        name = element[0].strip()
        iri = element[4].replace("<", "").replace(">", "").strip()
        formula =  element[1].strip()

        if iri is None or iri == "":
            print(f"Error! Compound '{element[0]}' is not included in Compounds",
                  f" in ontocrystal_species.py.")
            iri = "UNDEFILED"
            continue

        output += osInitCompound(iri, name, formula)
        if False:
        #if formula == "C17H32N2+2":
            print(iri, name, formula)
            print(type(name))
            print(output[-4])
            print(output[-3])
            print(output[-2])
            print(output[-1])
            #1/0

    _compounds_osda = tools.readCsv("Compounds_to_IRI_23.csv")[1:]

    iri = None
    for element in _compounds_osda:
        name = element[1].strip()
        iri = element[7].replace("<", "").replace(">", "").strip()
        formula = element[3].strip()

        if iri is None or iri == "":
            print(f"Error! Compound '{element[0]}' is not included in Compounds",
                  f" in ontocrystal_species.py.")
            iri = "UNDEFILED"
            continue

        output += osInitCompound(iri, name, formula)


    _compounds_osda = tools.readCsv("Compounds_Laura.csv")[1:]

    iri = None
    for element in _compounds_osda:
        name = element[1].strip()
        iri = element[7].replace("<", "").replace(">", "").strip()
        formula =  element[3].strip()

        if iri is None or iri == "":
            print(f"Error! Compound '{element[0]}' is not included in Compounds",
                  f" in ontocrystal_species.py.")
            iri = "UNDEFILED"
            continue

        output += osInitCompound(iri, name, formula)

    return output
    pass

def osInitCompound(iri, name=None, formula=None):
    output = []
    output.append([iri, "Instance", ontoSpeciesPrefix + "Species", "", "", ""])

    if name and name != "None":
        output.append([ontoSpeciesPrefix + "name", "Data Property", iri,
                       "", name, "string"])

    if formula and formula != "None" :
        output.append([ontoSpeciesPrefix + "formula", "Data Property", iri,
                       "", formula, "string"])

    return output
    pass

if __name__ == "__main__":
    output = []
    output += osInitElements()
    for line in output:
        #print(line)
        pass

    output += osInitCompounds()
    for line in output:
        #print(line)
        pass

    #output = output[2234:2240]
    #print("======================================")
    for line in output:
        #print(line)
        pass
    tools.writeCsv("fffffffffff.csv", output)
    print(len(output))
