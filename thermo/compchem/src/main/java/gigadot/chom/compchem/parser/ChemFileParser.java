package gigadot.chom.compchem.parser;

import gigadot.chom.compchem.CompChem;
import java.io.IOException;
import org.apache.log4j.Logger;

/**
 * Chemical file parser base class. This is made into an
 * abstract class so that it can have pre-defined Logger and default methods
 * for all its subclasses.
 *
 * @author Weerapong Phadungsukanan
 */
public abstract class ChemFileParser {

    /**
     * Apache log4j logger object.
     */
    protected Logger logger = Logger.getLogger(getClass());

    /**
     * Default constructor.
     */
    public ChemFileParser() {
    }

    /**
     * Parse interface which parse from given file path and store information in
     * CompChem object. This is for reusing JChemDocument object.
     *
     * @param file String path to input file to be read and parsed.
     * @return 
     * @throws IOException IO exception, normally due to reading file.
     */
    public abstract CompChem parse(String file) throws IOException;
}
