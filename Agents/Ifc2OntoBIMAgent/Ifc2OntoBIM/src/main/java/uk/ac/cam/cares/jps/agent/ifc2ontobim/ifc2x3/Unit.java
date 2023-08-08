package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StringUtils;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the Unit concept in the OM ontology.
 *
 * @author qhouyee
 */
public class Unit {
    private final String iri;
    private final String omClass;
    private final String symbol;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param omClass The Unit class in OM.
     * @param symbol  The symbol representing this unit.
     */
    public Unit(String omClass, String symbol) {
        String unit = StringUtils.getStringAfterLastCharacterOccurrence(omClass, StringUtils.SLASH);
        this.iri = NamespaceMapper.getBaseNameSpace() + unit + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.omClass = omClass;
        this.symbol = symbol;
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
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.RDF_TYPE, this.omClass);
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.SKOS_NOTATION, this.symbol, false);
    }
}
