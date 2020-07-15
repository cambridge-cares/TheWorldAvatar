import os
import re
from typing import List
from base_parser import base_parser

class gaussian_parser(base_parser):

    def getLevelOfTheory(self,line):
        """ This function reads the level of theory from a Gaussian log file.
            The current function is not quite accurate. It needs work."""

            if line.startswith(' #p'):
                line = line.split()
                level_of_theory = line[1].split('/')[0]
                #print(level_of_theory)
            if line.startswith(' #n'):
                line = line.split()
                level_of_theory = line[1].split('/')[0]
                #print(level_of_theory)
            if line.startswith(' #t'):
                line = line.split()
                level_of_theory = line[1].split('/')[0]
                #print(level_of_theory)
        return level_of_theory
    
    def getSpinMult(self,filePath):
        """ This function reads the multiplicity from a Gaussian log file."""
        lines: List[str] = []

        with open(filePath, "rt") as gaulogfile:
            for line in gaulogfile :
                lines.append(line)

        for line in lines:
            if 'Multiplicity' in line:
                mult = line[28]
        return mult

    def getSymmetryNumber(self,filePath):
        """ This function reads the molecular symmetry from a Gaussian log file."""
        lines: List[str] = []

        with open(filePath, "rt") as gaulogfile:
            for line in gaulogfile :
                lines.append(line)

        for line in lines:
            if 'Rotational symmetry number' in line:
                Symmetry = line[29]
                return Symmetry