class admsPol(object):
    def __init__(self, PolName, PolParNumDepositionData, PolParDiameter,PolParDensity, PolParMassFraction ,PolPollutantType = 1 , PolGasDepVelocityKnown = 1,  PolGasDepositionVelocity=0 ,PolGasType=1, PolParDepVelocityKnown =0, PolParTermVelocityKnown=0,   PolParDepositionVelocity =0,PolParTerminalVelocity=0   ,   PolWetWashoutKnown =1, PolWetWashout  = 0, PolWetWashoutA = 1.0e-4, PolWetWashoutB  =6.4e-1, PolConvFactor      =1, PolBkgLevel   = 0, PolBkgUnits ="ug/m3"):

        self.PolName = PolName
        self.PolPollutantType =PolPollutantType
        self.PolGasDepVelocityKnown =PolGasDepVelocityKnown
        self.PolGasDepositionVelocity =PolGasDepositionVelocity
        self.PolGasType = PolGasType
        self.PolParDepVelocityKnown = PolParDepVelocityKnown
        self.PolParTermVelocityKnown = PolParTermVelocityKnown 
        self.PolParNumDepositionData =PolParNumDepositionData
        self.PolParDepositionVelocity = [PolParDepositionVelocity]* PolParNumDepositionData
        self.PolParTerminalVelocity = [PolParTerminalVelocity]* PolParNumDepositionData
        self.PolParDiameter =PolParDiameter
        self.PolParDensity =PolParDensity
        self.PolParMassFraction=PolParMassFraction
        self.PolWetWashoutKnown =PolWetWashoutKnown
        self.PolWetWashout =PolWetWashout
        self.PolWetWashoutA =PolWetWashoutA
        self.PolWetWashoutB   =PolWetWashoutB     
        self.PolConvFactor     =PolConvFactor         
        self.PolBkgLevel=PolBkgLevel
        self.PolBkgUnits=PolBkgUnits

    def __repr__(self):
        attrs = self.__dict__.items()

        return '<admsPol {}>'.format(['{} {}'.format(attr, value) for (attr, value) in attrs])

