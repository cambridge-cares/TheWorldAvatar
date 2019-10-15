/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser.compact.g09;

import gigadot.chom.chem.structure.Compound;
import java.util.Map;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class CompoundReaderTest {

    @Test
    //@Ignore
    public void readerTest() {
        String path = "test_data/g09/reader_single/";
        try {
            Map<String, Compound> compounds = G09CompactCompoundReader.getCompounds(path);
            assert (compounds.size() == 1);
            Compound c = null;
            for (String key : compounds.keySet()) {
                c = compounds.get(key);
            }
            assert(CompoundComparison.isCH4(c));
        } catch (Exception e) {
            System.out.println();
        }
    }

    @Test
    public void multipleReaderTest() throws Exception {
        String path = "test_data/g09/reader_multiple/";
        Map<String, Compound> compounds = G09CompactCompoundReader.getCompounds(path);
        for (String key : compounds.keySet()) {
            if (key.endsWith("CH4.g09")) {
                assert(CompoundComparison.isCH4(compounds.get(key)));
            } else if (key.endsWith("C2H4.g09")) {
                assert(CompoundComparison.isC2H4(compounds.get(key)));
            } else if (key.endsWith("C3H8.g09")) {
                assert(CompoundComparison.isC3H8(compounds.get(key)));
            } else {
                assert(false);
            }
        }
    }
}
