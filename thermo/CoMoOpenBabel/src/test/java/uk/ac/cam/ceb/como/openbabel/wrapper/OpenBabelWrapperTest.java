package uk.ac.cam.ceb.como.openbabel.wrapper;

import uk.ac.cam.ceb.como.openbabel.cml.OpenBabelException;
import java.io.File;
import java.util.List;
import org.apache.commons.io.FileUtils;
import org.junit.Test;

/**
 * Unit test for simple App.
 */

public class OpenBabelWrapperTest {

    @Test
    public void executeTest() throws OpenBabelException {
        // interconversion via command line
        OpenBabelWrapper.setTempDir(new File("test_data/"));
        List<String> content = OpenBabelWrapper.execute("cml", "inchi", new File("test_data/cml/C2H4.cml"), "");
        assert(!content.isEmpty());
        assert(content.get(0).replace(System.getProperty("line.separator"), "").equals("InChI=1S/C2H4/c1-2/h1-2H2"));
        
        content = OpenBabelWrapper.execute("cml", "inchi", new File("test_data/cml/C2H4.cml"), "--SNon");
        assert(!content.isEmpty());
        assert(content.get(0).replace(System.getProperty("line.separator"), "").equals("InChI=1S/C2H4/c1-2/h1-2H2"));
        
        content = OpenBabelWrapper.execute("cml", "smi", new File("test_data/cml/C2H4.cml"), "");
        assert(!content.isEmpty());
        assert(content.get(0).replace(System.getProperty("line.separator"), "").trim().startsWith("C=C"));
        
        FileUtils.deleteQuietly(new File("test_data/.jbabel"));
    }

}