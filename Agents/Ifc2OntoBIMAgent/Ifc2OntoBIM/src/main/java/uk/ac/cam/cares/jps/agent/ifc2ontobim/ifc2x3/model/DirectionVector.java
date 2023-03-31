package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the DirectionVector concept in OntoBIM.
 *
 * @author qhouyee
 */
public class DirectionVector {
    private final String iri;
    private final Double[] dirRatios;

    /**
     * Standard Constructor initialising the necessary and optional inputs.
     *
     * @param xDirRatio A field for the x direction ratio.
     * @param yDirRatio A field for the y direction ratio.
     * @param zDirRatio An optional field for the z direction ratio.
     */
    public DirectionVector(String xDirRatio, String yDirRatio, String zDirRatio) {
        String prefix = NamespaceMapper.getBaseNameSpace();
        this.iri = prefix + OntoBimConstant.DIR_VEC_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        // Initialise the array and add coordinates
        this.dirRatios = new Double[3];
        this.dirRatios[0] = Double.valueOf(xDirRatio);
        this.dirRatios[1] = Double.valueOf(yDirRatio);
        if (zDirRatio != null) {
            this.dirRatios[2] = Double.valueOf(zDirRatio);
        }
    }

    public String getIri() {
        return this.iri;
    }

    public Double[] getDirRatios() {
        return this.dirRatios;
    }


    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_DIR_VEC_CLASS);
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_X_DIR_RATIO, this.dirRatios[0]);
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_Y_DIR_RATIO, this.dirRatios[1]);
        StatementHandler.addOptionalStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_Z_DIR_RATIO, this.dirRatios[2]);
    }
}
