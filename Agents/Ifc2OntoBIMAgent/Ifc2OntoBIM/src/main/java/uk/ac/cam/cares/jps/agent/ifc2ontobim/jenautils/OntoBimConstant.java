package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

public class OntoBimConstant {
    // restrict instantiation
    private OntoBimConstant() {}

    // Generic properties
    public static final String RDF_TYPE = NamespaceMapper.RDF_NAMESPACE + "type";

    // Quantity properties
    public static final String OM_HAS_VALUE = NamespaceMapper.OM_NAMESPACE + "hasValue";
    public static final String OM_HAS_NUMERICAL_VALUE = NamespaceMapper.OM_NAMESPACE + "hasNumericalValue";
    public static final String OM_HAS_UNIT = NamespaceMapper.OM_NAMESPACE + "hasUnit";
    public static final String SKOS_NOTATION = NamespaceMapper.SKOS_NAMESPACE + "notation";

    // Elevation properties
    public static final String BIM_HAS_REF_ELEVATION = NamespaceMapper.BIM_NAMESPACE + "hasRefElevation";
    public static final String BIM_HAS_TER_ELEVATION = NamespaceMapper.BIM_NAMESPACE + "hasTerrainElevation";

    // OM classes
    public static final String HEIGHT_CLASS = NamespaceMapper.OM_NAMESPACE + "Height";
    public static final String MEASURE_CLASS = NamespaceMapper.OM_NAMESPACE + "Measure";
    public static final String LENGTH_CLASS = NamespaceMapper.OM_NAMESPACE + "Length";
}
