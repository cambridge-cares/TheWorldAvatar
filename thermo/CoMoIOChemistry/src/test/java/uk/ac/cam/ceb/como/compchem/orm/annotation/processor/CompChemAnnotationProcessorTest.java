package uk.ac.cam.ceb.como.compchem.orm.annotation.processor;

import uk.ac.cam.ceb.como.compchem.orm.annotation.processor.CompChemAnnotationProcessor;
import uk.ac.cam.ceb.como.compchem.info.ComputedTargetInfo;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemIOUtils;
import uk.ac.cam.ceb.como.compchem.info.ComputedInfo;
import java.io.IOException;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class CompChemAnnotationProcessorTest {

    /**
     * Test of read method, of class CompChemAnnotationProcessor.
     */
    @Test
    public void testRead() throws IOException {
        System.out.println("===============");
        ComputedInfo target = new ComputedTargetInfo();
        target.setFinalSCFEnergyInHartree(100.1);
        CompChem cc = new CompChem();
        CompChemAnnotationProcessor ccap = new CompChemAnnotationProcessor<ComputedInfo>(cc);
        CompChem compChem = ccap.getCompChem(target);
        CompChemIOUtils.write(System.out, compChem);
        ccap.getEntity(ComputedTargetInfo.class);
    }
    /**
     * Test of read method, of class CompChemAnnotationProcessor.
     */
    @Test
    @Ignore
    public void testWrite() throws IOException {
        System.out.println("===============");
        CompChem cc = new CompChem();
        ComputedInfo target = new ComputedTargetInfo();
        CompChemAnnotationProcessor ccap = new CompChemAnnotationProcessor<ComputedInfo>(cc);
        CompChem compChem = ccap.getCompChem(target);
        CompChemIOUtils.write(System.out, compChem);
    }

}
