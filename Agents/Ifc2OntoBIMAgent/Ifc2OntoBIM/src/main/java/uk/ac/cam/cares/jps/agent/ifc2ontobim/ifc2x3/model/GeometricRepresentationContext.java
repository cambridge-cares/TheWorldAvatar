package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;

public class GeometricRepresentationContext {
    private Double precision = null;
    private String northDirIri = null;
    private final String iri;
    private final String wcsIri;
    private final Double dimension;

    /**
     * Standard Constructor initialising the necessary and optional inputs.
     *
     * @param iri                   The instance IRI of IfcGeometricRepresentationContext in IfcOwl.
     * @param spaceDimension        A field for the integer dimension count of the coordinate space.
     * @param precision             An optional field for the value of the geometric models' precision.
     * @param worldCoordinateSysIri The IRI for establishing the IFC project's engineering coordinate system.
     * @param trueNorthDirectionIRI An optional field for indicating the IRI of the True North direction vector.
     */
    public GeometricRepresentationContext(String iri, String spaceDimension, String precision, String worldCoordinateSysIri, String trueNorthDirectionIRI) {
        // Generate new geometric representation context IRI
        this.iri = StatementHandler.createInstanceFromIRI(iri, OntoBimConstant.GEOM_CONTEXT_CLASS);
        this.dimension = Double.valueOf(spaceDimension);
        this.wcsIri = StatementHandler.createInstanceFromIRI(worldCoordinateSysIri, OntoBimConstant.LOCAL_PLACEMENT_CLASS);
        // Parse the optional values
        if (precision != null) {
            this.precision = Double.valueOf(precision);
        }
        if (trueNorthDirectionIRI != null) {
            this.northDirIri = trueNorthDirectionIRI;
        }
    }

    public String getIri() {
        return this.iri;
    }

    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_GEOM_CONTEXT_CLASS);
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_SPACE_DIMENSION, this.dimension);
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_WCS, this.wcsIri);
        StatementHandler.addOptionalStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_PRECISION, this.precision);
        StatementHandler.addOptionalStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_TRUE_NORTH, this.northDirIri);
    }
}
