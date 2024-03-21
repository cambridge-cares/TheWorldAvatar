from enum import Enum


class OPltPlotAttrKey(Enum):
    LAND_USE_TYPE_TYPE = "LandUseTypeType"
    GROSS_PLOT_RATIO = "GrossPlotRatio"
    IS_AWAITING_DETAILED_GPR_EVAL = "IsAwaitingDetailedGPREvaluation"
    PLOT_AREA = "PlotArea"
    GROSS_FLOOR_AREA = "GrossFloorArea"


PLOT_ATTR_LABELS = {
    OPltPlotAttrKey.LAND_USE_TYPE_TYPE: ["land use type", "land use classification"],
    OPltPlotAttrKey.GROSS_PLOT_RATIO: ["gross plot ratio"],
    OPltPlotAttrKey.PLOT_AREA: ["plot area"],
    OPltPlotAttrKey.GROSS_FLOOR_AREA: ["gross floor area"],
}

PLOT_ATTR_2_PRED = {
    OPltPlotAttrKey.LAND_USE_TYPE_TYPE: "ozng:hasLandUseType/a",
    OPltPlotAttrKey.IS_AWAITING_DETAILED_GPR_EVAL: "oplnrgl:isAwaitingDetailedGPREvaluation",
    OPltPlotAttrKey.GROSS_PLOT_RATIO: "^oplnrgl:appliesTo/oplnrgl:allowsGrossPlotRatio/om:hasValue",
    OPltPlotAttrKey.PLOT_AREA: "oplt:hasPlotArea/om:hasValue",
    OPltPlotAttrKey.GROSS_FLOOR_AREA: "oplt:hasMaximumPermittedGPR/om:hasValue",
}

OZNG_LANDUSETYPE_LABELS = {
    "Agriculture": ["agriculture"],
    "BeachArea": ["beach area"],
    "Business1": ["Business 1", "B1", "clean and light industries"],
    "Business2": ["Business 2", "B2", "general and special industries"],
    "BusinessPark": ["business park"],
    "Cemetery": ["cemetery"],
    "CivicAndCommunityInstitution": [
        "Civic & Community Institution (C&CI)",
        "civic, community, cultural assocation/clan",
    ],
    "Commercial": ["commercial"],
    "CommercialAndResidential": ["mixed commercial and residential"],
    "CommercialOrInstitution": ["commercial or community institution facilities"],
    "EducationalInstitution": ["educational institution"],
    "HealthAndMedicalCare": ["health and medical care"],
    "HotelType": ["hotel"],
    "MixedUse": ["mixed used"],
    "OpenSpace": ["open space"],
    "Park": ["park"],
    "PlaceOfWorship": ["place of worship"],
    "PortOrAirport": ["port or airport"],
    "RapidTransit": ["rapid transit"],
    "ReserveSite": ["not yet determined"],
    "Residential": ["residential"],
    "ResidentialOrInstitution": ["residential or community institution facilities"],
    "ResidentialWithCommercialAtFirstStorey": [
        "residential with commercial at 1st storey"
    ],
    "Road": ["road"],
    "SpecialUse": ["special purposes"],
    "SportsAndRecreation": ["sports and recreational"],
    "TransportFacilities": ["transport facilities"],
    "Utility": ["public utilities and telecommunication infrastructure"],
    "Waterbody": ["drainage and water areas"],
}
