package uk.ac.cam.ceb.como.io.chem.file.writer.compchem;

import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.chem.structure.Compound;
import uk.ac.cam.ceb.como.chem.structure.CompoundDelegator;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemElementUtils;
import uk.ac.cam.ceb.como.compchem.orm.annotation.processor.AnnotationUtils;
import uk.ac.cam.ceb.como.compchem.orm.annotation.processor.CompChemAnnotationProcessor;

import nu.xom.Nodes;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLIdentifier;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLTable;
import uk.ac.cam.ceb.como.chem.structure.util.BasicCompoundConverter;
import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemIOUtils;
import uk.ac.cam.ceb.como.io.chem.file.writer.ChemFileWriter;

/**
 *
 * @author pb556
 */

public class CompChemWriter extends ChemFileWriter<Compound> {

    protected CMLIdentifier createCMLIdentifier(String convention, String IDValue) throws RuntimeException {
        CMLIdentifier cmlID = new CMLIdentifier();
        cmlID.setConvention(convention);
        cmlID.setCMLValue(IDValue);
        return cmlID;
    }

    protected void addJobListModuleIdentifiers(CMLModule joblist_mod) {
        joblist_mod.appendChild(createCMLIdentifier("chemid:EmpiricalFormula", BasicCompoundConverter.convertToEmpiricalFormula(content)));
    }

    @Override
    public void clear() throws Exception {
        f = null;
        content = null;
    }

    @Override
    public void write() throws Exception {
        CompChemWrapper ccw = AnnotationUtils.getCompChemWrapper(content);
        if (ccw == null) {
            ccw = new CompChemWrapper();
        }
        CompChemAnnotationProcessor<CompoundDelegator> ccap = new CompChemAnnotationProcessor<>(ccw);
        CompChem cc = ccap.getCompChem(content);

        if (content.getVibrationCount() > 0) {// process frequencies
            CMLProperty vibtable = CompChemElementUtils.getOrAddProperty(ccw.getJob(), "cc:vibrations", CompChem.COMPCHEM_NS);
            double[] df = new double[content.getVibrationCount()];
            for (int i = 0; i < content.getVibrationCount(); i++) {
                df[i] = content.getVibration(i).getFrequency();
            }
            CMLArray farray = new CMLArray(df);
            farray.setUnits("nonSi:GHz");
            farray.setDictRef("cc:frequencies");
            CMLElement table = (CMLElement) vibtable.getFirstCMLChild(CMLTable.TAG);
            if (table == null) {
                table = new CMLTable();
                vibtable.appendChild(table);
            }
            String ccPrefix = cc.getPrefixForNamespace(CompChem.COMPCHEM_NS);
            Nodes freqs = table.cmlQuery("./cml:array[@dictRef='" + ccPrefix + ":frequencies']");
            for (int i = 0; i < freqs.size(); i++) {
                table.removeChild(freqs.get(i));
            }
            table.appendChild(farray);
        }

        CompChemIOUtils.write(f, cc);
    }
    /**
    //       <parameter dictRef="cc:method">
    //          <scalar>B97-1</scalar>
    //       </parameter>
    //       <parameter dictRef="cc:basis">
    //          <scalar>6-311+G(d,p)</scalar>
    //       </parameter>
    //       <parameter dictRef="cc:goal">
    //          <scalar>Geometry Optimization</scalar>
    //       </parameter>
    //       <parameter dictRef="g:goal">
    //          <scalar>Opt</scalar>
    //       </parameter>
    //       <parameter dictRef="g:opt_max_cycles">
    //          <scalar dataType="xsd:integer" units="si:none">200</scalar>
    //       </parameter>
    //       <parameter dictRef="g:opt_estm_fc">
    //          <scalar>NewEstmFC</scalar>
    //       </parameter>
    //       <parameter dictRef="g:goal">
    //          <scalar>Freq</scalar>
    //       </parameter>
    //       <parameter dictRef="g:option">
    //          <scalar>Integral</scalar>
    //       </parameter>
    //       <parameter dictRef="g:integral_grid">
    //          <scalar>UltraFine</scalar>
    //       </parameter>
    //       <parameter dictRef="g:option">
    //          <scalar>Guess</scalar>
    //       </parameter>
    //       <parameter dictRef="g:guess_hf_initial">
    //          <scalar>Mix</scalar>
    //       </parameter>
     * @param jDoc
     */
}
