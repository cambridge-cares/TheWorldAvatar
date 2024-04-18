from .model import PlotNumAttrKey


LAND_USE_TYPE_PREDICATE = "ontozoning:hasLandUseType"
PLOT_NUM_ATTR_PREDICATES = {
    PlotNumAttrKey.GROSS_PLOT_RATIO: "^opr:appliesTo/opr:allowsGrossPlotRatio/om:hasValue",
    PlotNumAttrKey.PLOT_AREA: "ontoplot:hasPlotArea/om:hasValue",
    PlotNumAttrKey.GROSS_FLOOR_AREA: "ontoplot:hasMaximumPermittedGFA/om:hasValue",
}
