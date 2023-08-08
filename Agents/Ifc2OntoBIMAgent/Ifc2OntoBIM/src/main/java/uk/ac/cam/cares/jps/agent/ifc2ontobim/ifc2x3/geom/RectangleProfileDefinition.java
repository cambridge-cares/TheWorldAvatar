package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the RectangleProfileDefinition concept in OntoBIM.
 *
 * @author qhouyee
 */
public class RectangleProfileDefinition {
    private final String iri;
    private final String profileType;
    private final String positionIRI;
    private final Double xDimension;
    private final Double yDimension;


    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param profileType The type of profile.
     * @param positionIri The starting position of the profile, indicated by a local placement.
     * @param xDim        The x dimension extent of the profile.
     * @param yDim        The y dimension extent of the profile.
     */
    public RectangleProfileDefinition(String profileType, String positionIri, String xDim, String yDim) {
        this.iri = NamespaceMapper.getBaseNameSpace() + OntoBimConstant.RECTANGLE_PROFILE_DEFINITION_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.profileType = profileType;
        this.positionIRI = positionIri;
        this.xDimension = Double.valueOf(xDim);
        this.yDimension = Double.valueOf(yDim);
    }

    public String getIri() {
        return this.iri;
    }

    /**
     * Generate and add the statements required for this class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_RECTANGLE_PROFILE_DEFINITION_CLASS);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_PROFILE_TYPE, this.profileType);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_LOCAL_POSITION, this.positionIRI);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_RECTANGLE_PROFILE_X_EXTENT, this.xDimension);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_RECTANGLE_PROFILE_Y_EXTENT, this.yDimension);
    }
}
