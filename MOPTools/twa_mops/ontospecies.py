from __future__ import annotations
from typing import List, Optional
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty
import om
import geo

from rdkit.Chem import GetPeriodicTable
from rdkit.Chem.rdmolfiles import MolFromXYZFile

# NOTE TODO this script is incomplete as it only contains necessary classes and properties for the MOPs project
# NOTE TODO a complete OGM representation for OntoSpecies is yet to be implemented
# NOTE TODO it should also be moved to a place that accessible to all other ontologies
class OntoSpecies(BaseOntology):
    base_url = 'http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#'


# object properties
HasMolecularWeight = ObjectProperty.create_from_base('HasMolecularWeight', OntoSpecies)
HasCharge = ObjectProperty.create_from_base('HasCharge', OntoSpecies)
HasGeometry = ObjectProperty.create_from_base('HasGeometry', OntoSpecies)

# data properties
HasGeometryFile = DatatypeProperty.create_from_base('HasGeometryFile', OntoSpecies)


# classes
class Charge(BaseClass):
    rdfs_isDefinedBy = OntoSpecies
    hasValue: om.HasValue[om.Measure]

class MolecularWeight(BaseClass):
    rdfs_isDefinedBy = OntoSpecies
    hasValue: om.HasValue[om.Measure]

    @classmethod
    def from_xyz_file(cls, xyz_file: str) -> MolecularWeight:
        periodic_table = GetPeriodicTable()
        mol = MolFromXYZFile(xyz_file)
        atoms = [a.GetSymbol() for a in mol.GetAtoms()]
        molecular_weight = sum(periodic_table.GetAtomicWeight(atom) for atom in atoms)
        return cls(hasValue=om.Measure(hasNumericalValue=molecular_weight, hasUnit=om.gramPerMole))

class Geometry(BaseClass):
    rdfs_isDefinedBy = OntoSpecies
    hasGeometryFile: HasGeometryFile[str]
    hasPoints: Optional[List[geo.Point]] = None

    @property
    def geometry_file(self) -> str:
        return list(self.hasGeometryFile)[0]

    @classmethod
    def from_points(cls, points: List[geo.Point], file_name: str) -> Geometry:
        file_name = file_name + '.xyz' if not file_name.endswith('.xyz') else file_name
        pts = [p for p in points if p.label.lower() not in ['x', 'center']]
        with open(file_name, 'w') as f:
            f.write(f'{len(pts)}\n\n')
            for pt in pts:
                # enforce normal decimal numbers to avoid scientific notation which breaks reading the xyz file using rdkit
                f.write(f'{pt.label} {pt.x:.20f} {pt.y:.20f} {pt.z:.20f}\n')
        return cls(hasGeometryFile=file_name, hasPoints=points)

    def load_xyz_from_geometry_file(self, sparql_client):
        lst_pt = []
        remote_file_path = list(self.hasGeometryFile)[0]
        downloaded_file_path = remote_file_path.split('/')[-1]
        sparql_client.download_file(remote_file_path, downloaded_file_path)
        mol = MolFromXYZFile(downloaded_file_path)
        for a in mol.GetAtoms():
            pos = mol.GetConformer().GetAtomPosition(a.GetIdx())
            pt = geo.Point(x=pos.x, y=pos.y, z=pos.z, label=a.GetSymbol())
            lst_pt.append(pt)
        self.hasPoints = lst_pt
