from ..base.base_parser import BaseParser

class GaussianParser(BaseParser):

    def getLevelOfTheory(self,line):
        """ This function reads the level of theory from a Gaussian log file.
            The current function is not quite accurate. It needs work."""

        line = line.strip().lower()
        LevelOfTheory = ''
        if line.startswith('#p') or line.startswith('#n') or line.startswith('#t'):
            line = line.split()
            LevelOfTheory = line[1].split('/')[0]
        return LevelOfTheory
    
    def getSpinMultiplicity(self,line):
        """ This function reads the multiplicity from a Gaussian log file."""

        line = line.strip().lower()
        SpinMultiplicity = ''
        if 'multiplicity' in line:
            line = line.split()
            SpinMultiplicity = line[-1]
        return SpinMultiplicity

    def getSymmetryNumber(self,line):
        """ This function reads the molecular symmetry from a Gaussian log file."""

        line = line.strip().lower()
        SymmetryNumber = ''
        if 'rotational symmetry number' in line:
            line = line.split()
            SymmetryNumber = line[-1].replace('.','')
        return SymmetryNumber