package gigadot.chom.chem.reader;

import gigadot.chom.chem.structure.Compound;
import java.io.IOException;
import org.apache.log4j.Logger;

/**
 * Chemical file reader base class for JChemDocument. This is made into an
 * abstract class so that it can have pre-defined Logger and default read method
 * for all its subclasses.
 *
 * @author Weerapong Phadungsukanan
 */
public abstract class ChemFileReader {

    /**
     * Apache log4j logger object.
     */
    protected Logger logger = Logger.getLogger(getClass());

    /**
     * Default constructor.
     */
    public ChemFileReader() {
    }

    /**
     * Default read interface which reads from given file path and return new
     * JChemDocument object. abstract void read(String file, JChemDocument jDoc)
     * must be implemented.
     *
     * @param file String path to input file to be read and parsed.
     * @return New JChemDocument object with the stored information reading from
     * given files.
     * @throws IOException IO exception, normally due to reading file.
     * @see #read(String, JChemDocument)
     */
    public Compound read(String file) throws IOException {
        Compound compound = new Compound();
        read(file, compound);
        return compound;
    }

    /**
     * Read interface which reads from given file path and store information in
     * JChemDocument object. This is for reusing JChemDocument object.
     *
     * @param file String path to input file to be read and parsed.
     * @param compound 
     * @throws IOException IO exception, normally due to reading file.
     */
    public abstract void read(String file, Compound compound) throws IOException;
}
