
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
    SupModelComplexTerrain         = 0
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
    MetDataSource             = 0
    MetDataFileWellFormedPath = "C:\Users\nasac\Documents\GIT\JPS\python\caresjpsadmsinputs\test.met"
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
BldAngle=0.0   64.06391913991176   63.8860873697093   62.533516186061   153.6083092001291   153.6062361437698   62.40695834723154   153.7192098064336   62.66250519556777   153.63927720769354   70.85011770550763   102.65273150454249   140.28130867746722   102.71120642156859   140.1059902135607   141.87973246540707   141.8378698412866   51.15574977249284   141.85726338817358   52.132308671569966
BldHeight=37.544   2.9479999999999995   10.163   9.948   10.207999999999998   10.181   8.495000000000001   10.174   9.981   10.332   3.8180000000000005   16.275000000000002   22.083000000000002   24.349   11.158   10.994   11.02   9.749   11.026   10.32
BldLength=21.994791406845472   3.5010548773057315   12.42453784351756   11.992398715566665   11.54724764706535   11.687001263590085   36.02745619798401   23.943691319508492   12.18068970379577   21.55373604732266   10.097839849636468   50.932170565715694   77.84151104831635   88.9055006696234   86.83811639753972   10.883841592585773   10.254170383502814   10.612550150794341   10.41038124170562   21.53451535471602
BldName="C-9AD4-2880E3A60972"   "5-9A3C-E4457B796F3C"   "E-9105-4F953FA50FE1"   "1-836D-3845D068012A"   "6-87B6-6BB5E80E692B"   "8-997C-1E86F79AF802"   "3-80EE-1768B1729924"   "E-B48C-D3600B96BB1E"   "C-B62B-5F115CAD5117"   "E-996B-2F423D46A9E7"   "4-A485-CA001E91C7FD"   "9-9704-70D673C4744B"   "0-95A2-7C8CC2319B7E"   "0-812D-FDCA5F55198A"   "A-8E2A-245D4E6ECE38"   "9-AA14-2227230CEFE4"   "6-8F20-BD21053BD890"   "7-8E06-F540A63AF235"   "D-883C-CDECEFEA986B"   "7-8C34-F285B6EDB89E"
BldNumBuildings=  20
BldType=1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
BldWidth=0.0   2.1863527662027455   16.6691075064258   15.847145562234376   16.543349307636653   16.71367695325049   10.763834586891047   10.196489685096378   14.484646410741185   5.6796918563345296   36.6426969005909   12.297131127957927   17.13276583276476   29.960374440069312   11.797302946051403   14.077410746047896   14.152802100314855   4.845199423736175   14.051795270705233   9.573724968860287
BldX=79794.1484375   79787.640625   79891.390625   79914.0703125   79909.65625   79914.75   79921.453125   79904.6640625   79925.125   79923.5546875   79735.75   79759.09375   79826.1484375   79785.65625   79851.59375   79892.8203125   79886.375   79928.265625   79879.9765625   79920.625
BldY=454786.21875   454766.46875   454821.53125   454775.5625   454806.6875   454796.25   454748.5625   454824.03125   454780.5625   454830.8125   454692.1875   454874.8125   454925.78125   454908.8125   454939.0625   454944.5625   454952.8125   454941.75   454960.875   454959.65625
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
      79731.0 454666.0 0.00e+0
      1.0e+1 0.0e+0 0.0e+0
    GrdRegularMax          = 
      79931.0 454966.0 3.00e+1
      1.000e+3 3.30e+2 0.0e+0
    GrdRegularNumPoints    = 
      80 80 4
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
OptCondition=0   0   0   0   0
OptCreateComprehensiveFile=  0
OptExceedences=0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
OptGroupsOrSource=  1
OptInclude=1   1   1   1   1
OptIncludedGroups=  "Grouptank001"
OptIncludedSource="http://www.theworldavatar.com/Plant-001.owl#Plant-001"
OptNumExceedences=0   0   0   0   0
OptNumGroups=  1
OptNumOutputs=  5
OptNumPercentiles=0   0   0   0   0
OptPercentiles=0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
OptPolName="CO2"   "CO"   "NO2"   "HC"   "NOx"
OptSamplingTime=1   1   1   1   1
OptSamplingTimeUnits=3   3   3   3   3
OptShortOrLong=0   0   0   0   0
OptUnits="ug/m3"   "ug/m3"   "ug/m3"   "ug/m3"   "ug/m3"
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
    PolNumPollutants = 18
    PolNumIsotopes   = 0
    /

&ADMS_COORDINATESYSTEM
ProjectedEPSG = 28992

/

    &ADMS_MAPPERPROJECT
    ProjectFilePath               = " "
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
    PolConvFactor      = 5.47e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
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
	
	&ADMS_POLLUTANT_DETAILS
    PolName                  = "HC"
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
    PolConvFactor      = 0.802e+0
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /

&ADMS_SOURCE_DETAILS
SrcAngle1=  0
SrcAngle2=  0
SrcBuoyancyType=  0
SrcDensity=  0.0
SrcDiameter=  1.0
SrcEffluxType=  3
SrcFb=  1
SrcFm=  1
SrcGroup=  "Grouptank001"
SrcHeight=  83.607
SrcL1=  1
SrcL2=  1
SrcMainBuilding=  "5-9A3C-E4457B796F3C"
SrcMassFlux=  19.224
SrcMassH2O=  0
SrcMolWeight=  28.2916
SrcName=  "http://www.theworldavatar.com/Plant-001.owl#Plant-001"
SrcNumGroups=  1
SrcNumIsotopes=  0
SrcNumPollutants=  5
SrcNumVertices=  0
SrcPercentNOxAsNO2=  5
SrcPolDuration=0   0   0   0   0
SrcPolEmissionRate=446.0   0.0   800.0   1970.0   0.0
SrcPolStartTime=0   0   0   0   0
SrcPolTotalemission=1   1   1   1   1
SrcPollutants="CO"   "NOx"   "HC"   "CO2"   "NO2"
SrcReleaseAtNTP=  0
SrcSourceType=  0
SrcSpecHeatCap=  497.941
SrcTemperature=  30.0
SrcTraNumTrafficFlows=  0
SrcUseVARFile=  1
SrcVertVeloc=  0.0
SrcVolFlowRate=  0
SrcX1=  79835.9857
SrcY1=  454761.139634
/
