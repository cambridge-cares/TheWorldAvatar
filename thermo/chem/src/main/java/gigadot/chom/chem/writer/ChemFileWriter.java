package gigadot.chom.chem.writer;

import gigadot.chom.chem.structure.Compound;
import java.io.IOException;

/**
 * 
 *
 * @author Weerapong Phadungsukanan
 */
public abstract class ChemFileWriter {
    public abstract void write(String file, Compound mDoc) throws IOException;
}
