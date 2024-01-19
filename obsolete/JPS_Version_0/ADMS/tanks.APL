&ADMS_HEADER
Comment = "This is an ADMS parameter file"
Model = "ADMS"
Version = 5.2
FileVersion = 8
Complete = 1
/

&ADMS_PARAMETERS_SUP
SupSiteName                    = "Jurong Island dispersion site"
SupProjectName                 = "chlorine leakage tank dispersion"
SupUseAddInput                 = 0
SupAddInputPath                = " "
SupReleaseType                 = 0
SupModelBuildings              = 1
SupModelComplexTerrain         = 0
SupModelCoastline              = 0
SupPufType                     = 0
SupCalcChm                     = 0
SupCalcDryDep                  = 1
SupCalcWetDep                  = 1
SupCalcPlumeVisibility         = 0
SupModelFluctuations           = 0
SupModelRadioactivity          = 0
SupModelOdours                 = 0
SupOdourUnits                  = "ou_e"
SupPaletteType                 = 1
SupUseTimeVaryingEmissions     = 1
SupTimeVaryingEmissionsType    = 0
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
MetLatitude               = 1.29e+0
MetDataSource             = 0
MetDataFileWellFormedPath = test.met
MetWindHeight             = 1.0e+1
MetWindInSectors          = 0
MetWindSectorSizeDegrees  = 1.0e+1
MetDataIsSequential       = 1
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
MetVerticalProfilePath    = "C:\Program Files (x86)\CERC\ADMS 5\Data\profiles.prf"
Met_DS_RoughnessMode      = 1
Met_DS_Roughness          = 5.0e-1
Met_DS_UseAdvancedMet     = 0
Met_DS_SurfaceAlbedoMode  = 1
Met_DS_SurfaceAlbedo      = 2.3e-1
Met_DS_PriestlyTaylorMode = 1
Met_DS_PriestlyTaylor     = 1.0e+0
Met_DS_MinLmoMode         = 1
Met_DS_MinLmo             = 3.45e+1
Met_DS_PrecipFactorMode   = 1
Met_DS_PrecipFactor       = 4.5e-1
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
MetInclBoundaryLyrHt      = 0
MetInclSurfaceTemp        = 0
MetInclLateralSpread      = 0
MetInclRelHumidity        = 0
MetHandNumEntries         = 1
MetWindSpeed =
  3.5e+0
MetWindDirection =
  2.70e+2
MetJulianDayNum =
  6.0e+1
MetLocalTime =
  2.6e+0
MetCloudAmount =
  2.5e+0
MetSurfaceHeatFlux =
  0.0e+0
MetBoundaryLayerHeight =
  8.00e+2
MetSurfaceTemp =
  1.5e+1
MetLateralSpread =
  7.5e+0
MetYear =
  2017
MetRelHumidity =
  5.0e+1
/
&ADMS_PARAMETERS_BLD
BldNumBuildings = 4
BldName =
  "Building001" "Building002" "Building003" "Building004"
BldType =
  0 0 0 0
BldX =	
	355104.4
BldY =	
	139409.9
BldHeight =	
	40.0
BldLength =	
	100.68
BldWidth =	
	42.754
BldAngle =	
	46.3
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
  3.5026638e+5 1.34886226e+5 6.0e+1
  1.0e+1 0.0e+0 0.0e+0
GrdRegularMax          = 
  3.60016296e+5 1.43154472e+5 6.0e+1
  1.000e+3 3.30e+2 0.0e+0
GrdRegularNumPoints    = 
  31 31 1
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
OptNumOutputs               = 4
OptPolName                  =
  "Cl2" "CH3Cl" "ISOBUTYLENE" "NH3"
OptInclude =	
	  0 0 0 1
OptShortOrLong              =
  0 0 0 0
OptSamplingTime             =
  1.0e+0 1.0e+0 1.0e+0 1.0e+0
OptSamplingTimeUnits        =
  3 3 3 3
OptCondition                =
  0 0 0 0
OptNumPercentiles           =
  0 0 0 0
OptNumExceedences           =
  0 0 0 0
OptPercentiles              =
  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0
  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0
  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0
  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0
OptExceedences              =
  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0
  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0
  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0
  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0  0.0e+0
OptUnits                    =
  "ug/m3" "ug/m3" "ug/m3" "ug/m3"
OptGroupsOrSource           = 1
OptAllSources               = 1
OptNumGroups                = 1
OptIncludedGroups           =
  "Grouptank"
OptIncludedSource =	TankID_1354
OptCreateComprehensiveFile  = 0
/
&ADMS_PARAMETERS_CHM
ChmScheme            = 2
/
&ADMS_PARAMETERS_BKG
BkgFilePath     = "C:\Program Files (x86)\CERC\ADMS 5\Data\backgrnd.bgd"
BkgFixedLevels  = 1
/
&ADMS_PARAMETERS_ETC
SrcNumSources    = 4
PolNumPollutants = 16
PolNumIsotopes   = 0
/
&ADMS_COORDINATESYSTEM
ProjectedEPSG               = 32648
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
PolName                  = "Cl2"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 0
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
PolWetWashoutKnown = 0
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 3.5e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "CH3Cl"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 0
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
PolWetWashoutKnown = 0
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 4.922e-1
PolBkgLevel        = 6.0e-1
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "ISOBUTYLENE"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 0
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
PolWetWashoutKnown = 0
PolWetWashout      = 0.0e+0
PolWetWashoutA     = 1.0e-4
PolWetWashoutB     = 6.4e-1
PolConvFactor      = 4.43e-1
PolBkgLevel        = 0.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_POLLUTANT_DETAILS
PolName                  = "NH3"
PolPollutantType         = 0
PolGasDepVelocityKnown   = 0
PolGasDepositionVelocity =	0.0
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
PolWetWashoutKnown = 0
PolWetWashout =	0.0
PolWetWashoutA =	1.0E-4
PolWetWashoutB =	0.64
PolConvFactor =	4.814749574305678e-06
PolBkgLevel        = 6.0e+0
PolBkgUnits        = "ppb"
/

&ADMS_SOURCE_DETAILS
SrcName          = "Tank1574"
SrcMainBuilding  = "(Main)"
SrcHeight        = 5.0e+0
SrcDiameter      = 1.0e-2
SrcVolFlowRate   = 1.0e-3
SrcVertVeloc     = 1.5e+1
SrcTemperature   = 2.5e+1
SrcMolWeight     = 7.1e+1
SrcDensity       = 1.225e+0
SrcSpecHeatCap   = 4.9778e+2
SrcSourceType    = 0
SrcReleaseAtNTP  = 0
SrcEffluxType    = 0
SrcBuoyancyType  = 0
SrcPercentNOxAsNO2 = 5.0e+0
SrcX1            = 3.553073173e+5
SrcY1            = 1.394330335e+5
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
  "Grouptank"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "Cl2"
SrcPolEmissionRate =
  9.1043e+1
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/

&ADMS_SOURCE_DETAILS
SrcName          = "Tank1354"
SrcMainBuilding  = "Building003"
SrcHeight =	5
SrcDiameter =	5
SrcVolFlowRate   = 5.0e-3
SrcVertVeloc =	0.0
SrcTemperature =	300
SrcMolWeight =	17.031
SrcDensity       = 1.225e+0
SrcSpecHeatCap =	2110.3025
SrcSourceType    = 0
SrcReleaseAtNTP  = 0
SrcEffluxType    = 0
SrcBuoyancyType  = 0
SrcPercentNOxAsNO2 = 5.0e+0
SrcX1 =	355164.385908621
SrcY1 =	139449.37189365
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
  "Grouptank"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "NH3"
SrcPolEmissionRate =	
	16716.187517095743
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/

&ADMS_SOURCE_DETAILS
SrcName          = "Tank1329"
SrcMainBuilding  = "Building004"
SrcHeight        = 5.0e+0
SrcDiameter      = 1.0e-2
SrcVolFlowRate   = 1.0e-3
SrcVertVeloc     = 1.5e+1
SrcTemperature   = 3.0e+1
SrcMolWeight     = 1.7e+1
SrcDensity       = 1.225e+0
SrcSpecHeatCap   = 1.30153e+3
SrcSourceType    = 0
SrcReleaseAtNTP  = 0
SrcEffluxType    = 0
SrcBuoyancyType  = 0
SrcPercentNOxAsNO2 = 5.0e+0
SrcX1            = 3.51821663e+5
SrcY1            = 1.40111581048817e+5
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
  "Grouptank"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "ISOBUTYLENE"
SrcPolEmissionRate =
  5.223e+1
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/

&ADMS_SOURCE_DETAILS
SrcName          = "Tank1328"
SrcMainBuilding  = "Building004"
SrcHeight        = 5.0e+0
SrcDiameter      = 3.0e-2
SrcVolFlowRate   = 1.1e-2
SrcVertVeloc     = 1.5e+1
SrcTemperature   = 3.0e+1
SrcMolWeight     = 5.049e+1
SrcDensity       = 1.225e+0
SrcSpecHeatCap   = 8.12299e+2
SrcSourceType    = 0
SrcReleaseAtNTP  = 0
SrcEffluxType    = 0
SrcBuoyancyType  = 0
SrcPercentNOxAsNO2 = 5.0e+0
SrcX1            = 3.5183116239098e+5
SrcY1            = 1.40117941882916e+5
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
  "Grouptank"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "CH3Cl"
SrcPolEmissionRate =
  9.382e+1
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/
 = 0.0e+0
SrcUseVARFile    = 1
SrcNumGroups     = 1
SrcGroup =
  "Grouptank"
SrcNumVertices = 0
SrcTraNumTrafficFlows = 0
SrcNumPollutants      = 1
SrcPollutants =
  "CH3Cl"
SrcPolEmissionRate =
  9.382e+1
SrcPolTotalemission =
  1.0e+0
SrcPolStartTime =
  0.0e+0
SrcPolDuration =
  0.0e+0
SrcNumIsotopes        = 0
/
