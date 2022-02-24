&ADMS_HEADER
Comment = "This is an ADMS parameter file"
Model = "ADMS"
Version = 5.2
FileVersion = 8
Complete = 1
/

&ADMS_PARAMETERS_SUP
SupSiteName                    = "Test1110"
SupProjectName                 = "Project110"
SupUseAddInput                 = 0
SupAddInputPath                = " "
SupReleaseType                 = 0
SupModelBuildings              = 0
SupModelComplexTerrain         = 0
SupModelCoastline              = 0
SupPufType                     = 0
SupCalcChm                     = 0
SupCalcDryDep                  = 0
SupCalcWetDep                  = 1
SupCalcPlumeVisibility         = 0
SupModelFluctuations           = 0
SupModelRadioactivity          = 0
SupModelOdours                 = 0
SupOdourUnits                  = "ou_e"
SupPaletteType                 = 1
SupUseTimeVaryingEmissions     = 0
SupTimeVaryingEmissionsType    = 2
SupTimeVaryingVARPath          = " "
SupTimeVaryingFACPath          = " "
SupTimeVaryingEmissionFactorsWeekday =
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
SupTimeVaryingEmissionFactorsSaturday =
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
SupTimeVaryingEmissionFactorsSunday =
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
/
&ADMS_PARAMETERS_MET
MetLatitude               = 1.0e+0
MetDataSource             = 0
MetDataFileWellFormedPath = "C:\TOMCAT\webapps\ROOT\ADMSTEST\Test1110.out.met"
MetWindHeight             = 1.0e+1
MetWindInSectors          = 0
MetWindSectorSizeDegrees  = 1.0e+1
MetDataIsSequential       = 0
MetUseSubset              = 0
MetSubsetHourStart        = 1
MetSubsetDayStart         = 1
MetSubsetMonthStart       = 1
MetSubsetYearStart        = 2016
MetSubsetHourEnd          = 0
MetSubsetDayEnd           = 1
MetSubsetMonthEnd         = 1
MetSubsetYearEnd          = 2017
MetUseVerticalProfile     = 0
MetVerticalProfilePath    = " "
Met_DS_RoughnessMode      = 1
Met_DS_Roughness          = 5.0e-1
Met_DS_UseAdvancedMet     = 0
Met_DS_SurfaceAlbedoMode  = 0
Met_DS_SurfaceAlbedo      = 2.3e-1
Met_DS_PriestlyTaylorMode = 0
Met_DS_PriestlyTaylor     = 1.0e+0
Met_DS_MinLmoMode         = 0
Met_DS_MinLmo             = 1.0e+0
Met_DS_PrecipFactorMode   = 0
Met_DS_PrecipFactor       = 1.0e+0
Met_MS_RoughnessMode      = 3
Met_MS_Roughness          = 1.0e-1
Met_MS_UseAdvancedMet     = 0
Met_MS_SurfaceAlbedoMode  = 3
Met_MS_SurfaceAlbedo      = 2.3e-1
Met_MS_PriestlyTaylorMode = 3
Met_MS_PriestlyTaylor     = 1.0e+0
Met_MS_MinLmoMode         = 3
Met_MS_MinLmo             = 1.0e+0
MetHeatFluxType           = 0
MetInclBoundaryLyrHt      = 1
MetInclSurfaceTemp        = 0
MetInclLateralSpread      = 0
MetInclRelHumidity        = 0
MetHandNumEntries         = 0
/
&ADMS_PARAMETERS_BLD
BldNumBuildings = 0
/
&ADMS_PARAMETERS_HIL
HilGridSize          = 2
HilUseTerFile        = 1
HilUseRoughFile      = 0
HilTerrainPath       = " "
HilRoughPath         = " "
HilCreateFlowField   = 0
/
&ADMS_PARAMETERS_CST
CstPoint1X           = 0.0e+0
CstPoint1Y           = 0.0e+0
CstPoint2X           = -1.000e+3
CstPoint2Y           = 1.000e+3
CstLandPointX        = 5.00e+2
CstLandPointY        = 5.00e+2
/
&ADMS_PARAMETERS_FLC
FlcAvgTime           = 9.00e+2
FlcUnitsPollutants   = "ug/m3"
FlcUnitsIsotopes     = "Bq/m3"
FlcCalcToxicResponse = 0
FlcToxicExp          = 1.0e+0
FlcCalcPercentiles   = 0
FlcNumPercentiles    = 0
FlcCalcPDF           = 0
FlcPDFMode           = 0
FlcNumPDF            = 0
/
&ADMS_PARAMETERS_GRD
GrdType                = 0
GrdCoordSysType        = 0
GrdSpacingType         = 0
GrdRegularMin          = 
  3.48000e+5 1.34000e+5 1.0e+1
  1.0e+1 0.0e+0 0.0e+0
GrdRegularMax          = 
  3.75000e+5 1.51955e+5 1.0e+1
  1.000e+3 3.30e+2 0.0e+0
GrdRegularNumPoints    = 
  201 134 1
  10 12 1
GrdVarSpaceNumPointsX  = 0
GrdVarSpaceNumPointsY  = 0
GrdVarSpaceNumPointsZ  = 0
GrdVarSpaceNumPointsR  = 0
GrdVarSpaceNumPointsTh = 0
GrdVarSpaceNumPointsZp = 0
GrdPtsNumPoints        = 0 0
GrdPolarCentreX = 0.0e+0
GrdPolarCentreY = 0.0e+0
GrdPtsUsePointsFile  = 0
GrdPtsPointsFilePath = " "
/
&ADMS_PARAMETERS_PUF
PufStart            = 1.00e+2
PufStep             = 1.00e+2
PufNumSteps         = 10
/
&ADMS_PARAMETERS_GAM
GamCalcDose         = 0
/
&ADMS_PARAMETERS_OPT
OptNumOutputs               = 1
OptPolName                  =
  "CO2"
OptInclude                  =
  1
OptShortOrLong              =
  0
OptSamplingTime             =
  1.0e+0
OptSamplingTimeUnits        =
  3
OptCondition                =
  0
OptNumPercentiles           =
  0
OptNumExceedences           =
  0
OptPercentiles              =
  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0
OptExceedences              =
  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0
OptUnits                    =
  "ug/m3"
OptGroupsOrSource           = 0
OptAllSources               = 1
OptNumGroups                = 1
OptIncludedGroups           =
  "Group001"
OptIncludedSource           = "Source001"
OptCreateComprehensiveFile  = 0
/
&ADMS_PARAMETERS_CHM
ChmScheme            = 2
/
&ADMS_PARAMETERS_BKG
BkgFilePath     = " "
BkgFixedLevels  = 2
/
&ADMS_PARAMETERS_ETC
SrcNumSources    = 7
PolNumPollutants = 13
PolNumIsotopes   = 0
/
&ADMS_COORDINATESYSTEM
ProjectedEPSG               = 0
/
&ADMS_MAPPERPROJECT
ProjectFilePath               = " "
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "NOx"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 5.2e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "NO2"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 5.2e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "NO"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 8.0e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "O3"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 5.0e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "VOC"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 3.1e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "SO2"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 3.7e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "PM10"
PolPollutantType         = 1
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-5
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 1.0e+0
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ug/m3"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "PM2.5"
PolPollutantType         = 1
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  2.5e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 1.0e+0
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ug/m3"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "CO"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 8.6e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "BENZENE"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 3.1e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "BUTADIENE"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 4.5e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "HCl"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 0
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 6.589e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "CO2"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 1
PolGasDepositionVelocity = 0.0e+0
PolGasType               = 1
PolParDepVelocityKnown   = 1
PolParTermVelocityKnown  = 1
PolParNumDepositionData  = 1
PolParDepositionVelocity =
  0.0e+0
PolParTerminalVelocity =
  0.0e+0
PolParDiameter =
  1.0e-6
PolParDensity =
  1.000e+3
PolParMassFraction =
  1.0e+0
PolWetWashoutKnown = 1
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 7.7e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_SOURCE_DETAILS
SrcName          = "Source001"
SrcMainBuilding  = "(Main)"
SrcHeight        = 6.0e+1
SrcDiameter      = 2.5e+0
SrcVolFlowRate   = 5.8905e+1
SrcVertVeloc     = 1.2e+1
SrcTemperature   = 1.40e+2
SrcMolWeight     = 2.8966e+1
SrcDensity       = 1.225e+0
SrcSpecHeatCap   = 1.012e+3
SrcSourceType    = 0
SrcReleaseAtNTP  = 0
SrcEffluxType    = 0
SrcBuoyancyType  = 0
SrcPercentNOxAsNO2 = 5.0e+0
SrcX1            = 3.5824356e+5
SrcY1            = 1.4154805e+5
SrcL1            = 1.0e+0
SrcL2            = 1.0e+0
SrcFm            = 1.0e+0
SrcFb            = 1.0e+0
SrcMassFlux      = 1.0e+0
SrcAngle1        = 0.0e+0
SrcAngle2        = 0.0e+0
SrcMassH2O       = 0.0e+0
SrcUseVARFile    = 1
SrcNumGroups     = 1
SrcGroup =
  "Group001"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "CO2"
SrcPolEmissionRate =
  5.6911e+4
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/

&ADMS_SOURCE_DETAILS
SrcName          = "Source002"
SrcMainBuilding  = "(Main)"
SrcHeight        = 6.0e+1
SrcDiameter      = 2.5e+0
SrcVolFlowRate   = 5.8905e+1
SrcVertVeloc     = 1.2e+1
SrcTemperature   = 1.40e+2
SrcMolWeight     = 2.8966e+1
SrcDensity       = 1.225e+0
SrcSpecHeatCap   = 1.012e+3
SrcSourceType    = 0
SrcReleaseAtNTP  = 0
SrcEffluxType    = 0
SrcBuoyancyType  = 0
SrcPercentNOxAsNO2 = 5.0e+0
SrcX1            = 3.5837691e+5
SrcY1            = 1.4141893e+5
SrcL1            = 1.0e+0
SrcL2            = 1.0e+0
SrcFm            = 1.0e+0
SrcFb            = 1.0e+0
SrcMassFlux      = 1.0e+0
SrcAngle1        = 0.0e+0
SrcAngle2        = 0.0e+0
SrcMassH2O       = 0.0e+0
SrcUseVARFile    = 1
SrcNumGroups     = 1
SrcGroup =
  "Group001"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "CO2"
SrcPolEmissionRate =
  5.6911e+4
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/

&ADMS_SOURCE_DETAILS
SrcName          = "Source003"
SrcMainBuilding  = "(Main)"
SrcHeight        = 6.0e+1
SrcDiameter      = 2.5e+0
SrcVolFlowRate   = 5.8905e+1
SrcVertVeloc     = 1.2e+1
SrcTemperature   = 1.40e+2
SrcMolWeight     = 2.8966e+1
SrcDensity       = 1.225e+0
SrcSpecHeatCap   = 1.012e+3
SrcSourceType    = 0
SrcReleaseAtNTP  = 0
SrcEffluxType    = 0
SrcBuoyancyType  = 0
SrcPercentNOxAsNO2 = 5.0e+0
SrcX1            = 3.5853778e+5
SrcY1            = 1.4126018e+5
SrcL1            = 1.0e+0
SrcL2            = 1.0e+0
SrcFm            = 1.0e+0
SrcFb            = 1.0e+0
SrcMassFlux      = 1.0e+0
SrcAngle1        = 0.0e+0
SrcAngle2        = 0.0e+0
SrcMassH2O       = 0.0e+0
SrcUseVARFile    = 1
SrcNumGroups     = 1
SrcGroup =
  "Group001"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "CO2"
SrcPolEmissionRate =
  5.6911e+4
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/

&ADMS_SOURCE_DETAILS
SrcName          = "Source004"
SrcMainBuilding  = "(Main)"
SrcHeight        = 5.0e+1
SrcDiameter      = 2.5e+0
SrcVolFlowRate   = 5.8905e+1
SrcVertVeloc     = 1.2e+1
SrcTemperature   = 1.40e+2
SrcMolWeight     = 2.8966e+1
SrcDensity       = 1.225e+0
SrcSpecHeatCap   = 1.012e+3
SrcSourceType    = 0
SrcReleaseAtNTP  = 0
SrcEffluxType    = 0
SrcBuoyancyType  = 0
SrcPercentNOxAsNO2 = 5.0e+0
SrcX1            = 3.5567511e+5
SrcY1            = 1.3865465e+5
SrcL1            = 1.0e+0
SrcL2            = 1.0e+0
SrcFm            = 1.0e+0
SrcFb            = 1.0e+0
SrcMassFlux      = 1.0e+0
SrcAngle1        = 0.0e+0
SrcAngle2        = 0.0e+0
SrcMassH2O       = 0.0e+0
SrcUseVARFile    = 1
SrcNumGroups     = 1
SrcGroup =
  "Group001"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "CO2"
SrcPolEmissionRate =
  6.5473e+4
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/

&ADMS_SOURCE_DETAILS
SrcName          = "Source005"
SrcMainBuilding  = "(Main)"
SrcHeight        = 4.0e+1
SrcDiameter      = 2.5e+0
SrcVolFlowRate   = 5.8905e+1
SrcVertVeloc     = 1.2e+1
SrcTemperature   = 1.40e+2
SrcMolWeight     = 2.8966e+1
SrcDensity       = 1.225e+0
SrcSpecHeatCap   = 1.012e+3
SrcSourceType    = 0
SrcReleaseAtNTP  = 0
SrcEffluxType    = 0
SrcBuoyancyType  = 0
SrcPercentNOxAsNO2 = 5.0e+0
SrcX1            = 3.5253399e+5
SrcY1            = 1.4166991e+5
SrcL1            = 1.0e+0
SrcL2            = 1.0e+0
SrcFm            = 1.0e+0
SrcFb            = 1.0e+0
SrcMassFlux      = 1.0e+0
SrcAngle1        = 0.0e+0
SrcAngle2        = 0.0e+0
SrcMassH2O       = 0.0e+0
SrcUseVARFile    = 1
SrcNumGroups     = 1
SrcGroup =
  "Group001"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "CO2"
SrcPolEmissionRate =
  7.3800e+4
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/

&ADMS_SOURCE_DETAILS
SrcName          = "Source006"
SrcMainBuilding  = "(Main)"
SrcHeight        = 4.0e+1
SrcDiameter      = 2.5e+0
SrcVolFlowRate   = 5.8905e+1
SrcVertVeloc     = 1.2e+1
SrcTemperature   = 1.40e+2
SrcMolWeight     = 2.8966e+1
SrcDensity       = 1.225e+0
SrcSpecHeatCap   = 1.012e+3
SrcSourceType    = 0
SrcReleaseAtNTP  = 0
SrcEffluxType    = 0
SrcBuoyancyType  = 0
SrcPercentNOxAsNO2 = 5.0e+0
SrcX1            = 3.5806548e+5
SrcY1            = 1.4200228e+5
SrcL1            = 1.0e+0
SrcL2            = 1.0e+0
SrcFm            = 1.0e+0
SrcFb            = 1.0e+0
SrcMassFlux      = 1.0e+0
SrcAngle1        = 0.0e+0
SrcAngle2        = 0.0e+0
SrcMassH2O       = 0.0e+0
SrcUseVARFile    = 1
SrcNumGroups     = 1
SrcGroup =
  "Group001"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "CO2"
SrcPolEmissionRate =
  2.2030e+4
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/

&ADMS_SOURCE_DETAILS
SrcName          = "Source007"
SrcMainBuilding  = "(Main)"
SrcHeight        = 4.0e+1
SrcDiameter      = 2.5e+0
SrcVolFlowRate   = 5.8905e+1
SrcVertVeloc     = 1.2e+1
SrcTemperature   = 1.40e+2
SrcMolWeight     = 2.8966e+1
SrcDensity       = 1.225e+0
SrcSpecHeatCap   = 1.012e+3
SrcSourceType    = 0
SrcReleaseAtNTP  = 0
SrcEffluxType    = 0
SrcBuoyancyType  = 0
SrcPercentNOxAsNO2 = 5.0e+0
SrcX1            = 3.5808466e+5
SrcY1            = 1.4193878e+5
SrcL1            = 1.0e+0
SrcL2            = 1.0e+0
SrcFm            = 1.0e+0
SrcFb            = 1.0e+0
SrcMassFlux      = 1.0e+0
SrcAngle1        = 0.0e+0
SrcAngle2        = 0.0e+0
SrcMassH2O       = 0.0e+0
SrcUseVARFile    = 1
SrcNumGroups     = 1
SrcGroup =
  "Group001"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "CO2"
SrcPolEmissionRate =
  2.2030e+4
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/
