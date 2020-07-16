#This is a template input file for Arkane.
#The keywords (in CAPS) are replaced by the CompChem parser before calling Arkane.

#Main parameters
modelChemistry = LEVELOFTHEORY
useHinderedRotors = False
useBondCorrections = False
useAtomCorrections = False

#Species declarations
species(MYSPECIES,MYSPECPY)

#Get thermo stuff
#thermo('myspecies','NASA')