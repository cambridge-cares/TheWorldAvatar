class admsSrc(object):

    def __init__(self, SrcName, SrcHeight, SrcDiameter,  SrcVertVeloc,SrcTemperature, SrcMolWeight, SrcDensity, SrcSpecHeatCap,  SrcPollutants,SrcPolEmissionRate,SrcX1 , SrcY1, SrcVolFlowRate = 0,SrcSourceType = 0, SrcReleaseAtNTP = 0, SrcEffluxType = 0, SrcBuoyancyType=0, SrcPercentNOxAsNO2 = 5, SrcL1 =1, SrcL2 =1, SrcFm = 1, SrcFb = 1, SrcMassFlux = 1, SrcAngle1 = 0 , SrcAngle2 = 0, SrcMassH2O = 0,SrcUseVARFile=1,SrcNumGroups=1, SrcGroup=  "Grouptank001", SrcNumVertices =0, SrcTraNumTrafficFlows=0, SrcNumPollutants = 1,SrcPolTotalemission =1, SrcPolStartTime=0,SrcPolDuration =0, SrcNumIsotopes =0  ):

        self.SrcName = SrcName
        self.SrcHeight = SrcHeight
        self.SrcDiameter = SrcDiameter
        self.SrcVolFlowRate = SrcVolFlowRate
        self.SrcVertVeloc = SrcVertVeloc
        self.SrcTemperature = SrcTemperature
        self.SrcMolWeight = SrcMolWeight
        self.SrcDensity = SrcDensity
        self.SrcSpecHeatCap = SrcSpecHeatCap
        self.SrcSourceType = SrcSourceType
        self.SrcReleaseAtNTP = SrcReleaseAtNTP
        self.SrcEffluxType = SrcEffluxType
        self.SrcBuoyancyType = SrcBuoyancyType
        self.SrcPercentNOxAsNO2 = SrcPercentNOxAsNO2
        self.SrcX1 = SrcX1
        self.SrcY1 = SrcY1
        self.SrcL1 = SrcL1
        self.SrcL2 = SrcL2
        self.SrcFm = SrcFm
        self.SrcFb = SrcFb
        self.SrcMassFlux = SrcMassFlux
        self.SrcAngle1 = SrcAngle1
        self.SrcAngle2 = SrcAngle2
        self.SrcMassH2O = SrcMassH2O
        self.SrcUseVARFile = SrcUseVARFile
        self.SrcNumGroups = SrcNumGroups
        self.SrcGroup = SrcGroup
        self.SrcNumVertices = SrcNumVertices
        self.SrcTraNumTrafficFlows = SrcTraNumTrafficFlows
        self.SrcNumPollutants = SrcNumPollutants
        self.SrcNumPollutants = SrcNumPollutants
        self.SrcPollutants = SrcPollutants
        self.SrcPolEmissionRate = SrcPolEmissionRate
        self.SrcPolTotalemission = SrcPolTotalemission
        self.SrcPolStartTime = SrcPolStartTime
        self.SrcPolDuration = SrcPolDuration
        self.SrcNumIsotopes = SrcNumIsotopes



    def setMainBuilding(self, mainBdn):
        self.SrcMainBuilding = mainBdn


