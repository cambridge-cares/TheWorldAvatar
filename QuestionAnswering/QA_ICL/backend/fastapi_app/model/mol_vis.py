from os import PathLike
from pydantic import BaseModel


class AtomGeometry(BaseModel):
    symbol: str
    x: float
    y: float
    z: float

    def __str__(self):
        return "{element} {X} {Y} {Z}".format(
            element=self.symbol, X=self.x, Y=self.y, Z=self.z
        )


class MoleculeGeometry(BaseModel):
    atoms: list[AtomGeometry]

    def to_xyz_str(self):
        return """{number_of_atoms}
{comment_line}
{atoms}""".format(
            number_of_atoms=len(self.atoms),
            comment_line="",
            atoms="\n".join(str(atom) for atom in self.atoms),
        )
