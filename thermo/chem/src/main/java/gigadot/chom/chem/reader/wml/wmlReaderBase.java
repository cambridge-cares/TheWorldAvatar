package gigadot.chom.chem.reader.wml;

import gigadot.chom.chem.structure.Compound;
import java.io.IOException;
import nu.xom.Document;
import org.apache.log4j.Logger;

/**
 * 
 *
 * @author Weerapong Phadungsukanan
 */
public abstract class wmlReaderBase {
    Logger logger = Logger.getLogger(getClass());
    public abstract void read(Document xml, Compound mDoc) throws IOException;
}
