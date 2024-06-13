from constants.namespace import ONTOZEOLITE
from model.rdf_orm import RDFEntity, RDFField


class OntozeoliteZeoliteFramework(RDFEntity):
    framework_code: str = RDFField(path=ONTOZEOLITE.hasFrameworkCode)


class OntozeoliteZeoliticMaterial(RDFEntity):
    chemical_formula: str = RDFField(path=ONTOZEOLITE.hasChemicalFormula)
