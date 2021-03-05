/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.openbabel.util;

import uk.ac.cam.ceb.como.openbabel.util.OpenBabelUtil;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class OpenBabelUtilTest {

    @Test
    public void executableExistsTest() {
        assert (OpenBabelUtil.executableExists());
    }

    @Test
    public void getVersionTest() {
        assert (OpenBabelUtil.getVersion() != null);
    }
}
