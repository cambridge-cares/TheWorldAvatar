from ..base.base_parser import BaseParser

class GaussianParser(BaseParser):

    def parseLog(self,logFile,parsedResults):
        # Instantiate empty lists to be populated by the parser. This means
        # if the parser find nothing it will return an empty list
        LevelOfTheory = []
        SpinMultiplicity = []
        SymmetryNumber = []

        # Start parsing the log file
        with open(logFile, "rt") as logFileHandle:
            # The parser searches each line of the file for objects of interest
            # each item found in the file is appended to the corresponding object
            for line in logFileHandle:
                # Search for the level of theory
                tmp = self.getLevelOfTheory(line)
                if tmp: 
                    LevelOfTheory.append(tmp)
                # Search for the spin multiplicity
                tmp = self.getSpinMultiplicity(line)
                if tmp:
                    SpinMultiplicity.append(tmp)
                # Search for the symmetry number
                tmp = self.getSymmetryNumber(line)
                if tmp:
                    SymmetryNumber.append(tmp)

        # Assign found results to the parsedResults dictionary
        parsedResults['LevelOfTheory'] = LevelOfTheory
        parsedResults['SpinMultiplicity'] = SpinMultiplicity
        parsedResults['SymmetryNumber'] = SymmetryNumber

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