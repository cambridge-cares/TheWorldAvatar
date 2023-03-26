package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcgeometry;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.rdf.model.RDFNode;

import java.util.List;

/**
 * This class calls different methods, depending on the input, to create the relevant SPARQL query for geometries and geospatial operators.
 *
 * @author qhouyee
 */
public class GeomConstructBuilderMediator {
    protected static final String DIRECTION_VAR = "?direction";
    protected static final String DIRECTION_AXIS_VAR = "?axisdirection";
    protected static final String DIRECTION_REF_VAR = "?refdirection";
    protected static final String CARTPOINT_VAR = "?cartesianpoint";
    private ConstructBuilder builder;

    /**
     * Standard Constructor.
     */
    public GeomConstructBuilderMediator() {
    }

    /**
     * Create the SPARQL query syntax for Construct queries of geometry elements.
     *
     * @param builder Construct Builder object to add Construct query statements.
     * @param iriList List of IRI of the specific geometry class.
     * @return The SPARQL query string for geometry elements.
     */
    public String createSparqlQuery(ConstructBuilder builder, List<RDFNode> iriList) {
        this.builder = builder;
        switchConstructFunctionDependingOnIRI(iriList);
        return this.builder.buildString();
    }

    /**
     * A utility method to call other functions that generate SPARQL query statements based on the inputs.
     */
    private void switchConstructFunctionDependingOnIRI(List<RDFNode> iriList) {
        if (!iriList.isEmpty()) {
            String sampleIri = iriList.get(0).toString();
            if (sampleIri.contains("IfcFacetedBrep")) {
                IfcGeometryConstructBuilder.constructFacetedBrepRepresentationTriples(iriList, this.builder);
            } else if (sampleIri.contains("IfcExtrudedAreaSolid")) {
                IfcGeometryConstructBuilder.constructExtrudedAreaSolidRepresentationTriples(iriList, this.builder);
            } else if (sampleIri.contains("IfcBooleanClippingResult")) {
                IfcGeometryConstructBuilder.constructBooleanClippingResultRepresentationTriples(iriList, this.builder);
            } else if (sampleIri.contains("IfcPolygonalBoundedHalfSpace")) {
                IfcGeometryConstructBuilder.constructPolygonalBoundedHalfSpaceRepresentationTriples(iriList, this.builder);
            } else if (sampleIri.contains("IfcPolyline")) {
                IfcGeometryConstructBuilder.constructPolylineRepresentationTriples(iriList, this.builder);
            }
        }
    }
}
