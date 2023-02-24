package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

public class OntoBimConstant {
    // restrict instantiation
    private OntoBimConstant() {
    }

    // General symbols
    public static final String UNDERSCORE = "_";

    // Generic properties
    public static final String RDF_TYPE = NamespaceMapper.RDF_NAMESPACE + "type";

    // Quantity properties
    public static final String OM_HAS_VALUE = NamespaceMapper.OM_NAMESPACE + "hasValue";
    public static final String OM_HAS_NUMERICAL_VALUE = NamespaceMapper.OM_NAMESPACE + "hasNumericalValue";
    public static final String OM_HAS_UNIT = NamespaceMapper.OM_NAMESPACE + "hasUnit";
    public static final String SKOS_NOTATION = NamespaceMapper.SKOS_NAMESPACE + "notation";

    // OntoBim properties
    public static final String BIM_HAS_REF_ELEVATION = NamespaceMapper.BIM_NAMESPACE + "hasRefElevation";
    public static final String BIM_HAS_TER_ELEVATION = NamespaceMapper.BIM_NAMESPACE + "hasTerrainElevation";
    public static final String BIM_HAS_IFC_REPRESENTATION = NamespaceMapper.BIM_NAMESPACE + "hasIfcRepresentation";
    public static final String BOT_HAS_BUILDING = NamespaceMapper.BOT_NAMESPACE + "hasBuilding";
    public static final String BOT_HAS_STOREY = NamespaceMapper.BOT_NAMESPACE + "hasStorey";
    public static final String BIM_HAS_ROOM = NamespaceMapper.BIM_NAMESPACE + "hasRoom";

    // OntoBim classes
    public static final String SITE_CLASS = "Site";
    public static final String BUILDING_CLASS = "Building";
    public static final String STOREY_CLASS = "Storey";
    public static final String ROOM_CLASS = "Room";
    public static final String BOT_SITE_CLASS = NamespaceMapper.BOT_NAMESPACE + SITE_CLASS;
    public static final String BOT_BUILDING_CLASS = NamespaceMapper.BOT_NAMESPACE + BUILDING_CLASS;
    public static final String BOT_STOREY_CLASS = NamespaceMapper.BOT_NAMESPACE + STOREY_CLASS;
    public static final String BIM_ROOM_CLASS = NamespaceMapper.BIM_NAMESPACE + ROOM_CLASS;

    // OM classes
    public static final String HEIGHT_CLASS = NamespaceMapper.OM_NAMESPACE + "Height";
    public static final String MEASURE_CLASS = NamespaceMapper.OM_NAMESPACE + "Measure";
    public static final String LENGTH_CLASS = NamespaceMapper.OM_NAMESPACE + "Length";
}
