
    &ADMS_HEADER
    Comment = "This is an ADMS parameter file"
    Model = "ADMS"
    Version = 5.2
    FileVersion = 8
    Complete = 1
    /


    &ADMS_PARAMETERS_SUP
    SupSiteName                    = "terrain dispersion site"
    SupProjectName                 = "chlorine leakage tank dispersion"
    SupUseAddInput                 = 0
    SupAddInputPath                = " "
    SupReleaseType                 = 0
    SupModelBuildings              = 1
    SupModelComplexTerrain         = 1
    SupModelCoastline              = 0
    SupPufType                     = 0
    SupCalcChm                     = 0
    SupCalcDryDep                  = 0
    SupCalcWetDep                  = 0
    SupCalcPlumeVisibility         = 0
    SupModelFluctuations           = 0
    SupModelRadioactivity          = 0
    SupModelOdours                 = 0
    SupOdourUnits                  = "ou_e"
    SupPaletteType                 = 1
    SupUseTimeVaryingEmissions     = 0
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
    MetLatitude               = 1.09e+0
    MetDataSource             = 1
    MetDataFileWellFormedPath = "/home/zhouxiaochi/Documents/JPS/JParkSimulator-git/JPS/python/caresjpsadmsinputs/test.met"
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
    Met_DS_Roughness          = 1.5e+0
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
    MetInclSurfaceTemp        = 1
    MetInclLateralSpread      = 0
    MetInclRelHumidity        = 0
    MetHandNumEntries         = 1
    MetWindSpeed =
      3.06e+0
    MetWindDirection =
      6.0e+1
    MetJulianDayNum =
      2.47e+2
    MetLocalTime =
      5.0e+0
    MetCloudAmount =
      5.0e+0
    MetSurfaceHeatFlux =
      0.0e+0
    MetBoundaryLayerHeight =
      8.00e+2
    MetSurfaceTemp =
      2.8e+1
    MetLateralSpread =
      7.5e+0
    MetYear =
      2017
    MetRelHumidity =
      7.4e+1
    /

&ADMS_PARAMETERS_BLD
BldAngle=145.85590312049192   146.25741189083308
BldHeight=15662216.17976596   15662223.149057355
BldLength=30.122461370311427   12.555408675023168
BldName="BuildingGUID_2C309B8E-A55D-4437-91E4-F379BE5FC5BA"   "BuildingGUID_999_1170"
BldNumBuildings=  2
BldType=0   0
BldWidth=21.319223954020483   3.787896912078192
BldX=-3972173.144071371   -3972291.7388090347
BldY=10911360.166618004   10911412.644709103
/

    &ADMS_PARAMETERS_HIL
    HilGridSize          = 2
    HilUseTerFile        = 1
    HilUseRoughFile      = 0
    HilTerrainPath       = "C:\Users\kevin\Downloads\A48\terrain accurate\singaporeterrain.ter"
    HilRoughPath         = " "
    HilCreateFlowField   = 1
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
      3.46000e+5 1.36500e+5 1.00e+2
      1.0e+1 0.0e+0 0.0e+0
    GrdRegularMax          = 
      3.85500e+5 1.61700e+5 1.00e+2
      1.000e+3 3.30e+2 0.0e+0
    GrdRegularNumPoints    = 
      80 80 1
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
    GrdPtsUsePointsFile  = 1
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
OptAllSources=  0
OptCondition=0
OptCreateComprehensiveFile=  0
OptExceedences=0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
OptGroupsOrSource=  1
OptInclude=1
OptIncludedGroups=  "Grouptank001"
OptIncludedSource=
OptNumExceedences=0
OptNumGroups=  1
OptNumOutputs=  1
OptNumPercentiles=0
OptPercentiles=0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
OptPolName="http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/substance/substance.owl#chlorine"
OptSamplingTime=1
OptSamplingTimeUnits=3
OptShortOrLong=0
OptUnits="ug/m3"   "ug/m3"   "ug/m3"   "ug/m3"
/

    &ADMS_PARAMETERS_CHM
    ChmScheme            = 2
    /


    &ADMS_PARAMETERS_BKG
    BkgFilePath     = "D:\ADMS 5.2\Test files\tank1574leakage\background condition.bgd"
    BkgFixedLevels  = 1
    /
    &ADMS_PARAMETERS_ETC
    SrcNumSources    = 1
    PolNumPollutants = 16
    PolNumIsotopes   = 0
    /

&ADMS_COORDINATESYSTEM
ProjectedEPSG = 32648

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
    PolGasDepositionVelocity = 5.0e+0
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
    PolWetWashout      = 1.0e-4
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
    PolWetWashoutKnown = 1
    PolWetWashout      = 1.0e-4
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
    PolWetWashoutKnown = 1
    PolWetWashout      = 1.0e-4
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
    PolWetWashout      = 1.0e-4
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 1.462e+0
    PolBkgLevel        = 6.0e+0
    PolBkgUnits        = "ppb"
    /


