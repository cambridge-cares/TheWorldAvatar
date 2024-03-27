from importlib.resources import files

from .model import PlotNumAttrKey

LAND_USE_TYPES = (
    files("resources.zaha.sg_land_lots")
    .joinpath("land_use_type_iris.txt")
    .read_text()
    .split()
)


LAND_USE_TYPE_PREDICATE = "ontozoning:hasLandUseType"
PLOT_NUM_ATTR_PREDICATES = {
    PlotNumAttrKey.GROSS_PLOT_RATIO: "^opr:appliesTo/opr:allowsGrossPlotRatio/om:hasValue",
    PlotNumAttrKey.PLOT_AREA: "ontoplot:hasPlotArea/om:hasValue",
    PlotNumAttrKey.GROSS_FLOOR_AREA: "ontoplot:hasMaximumPermittedGPR/om:hasValue",
}
