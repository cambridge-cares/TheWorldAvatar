package gigadot.chom.chem.writer;

import gigadot.chom.compchem.CompChemWrapper;
import gigadot.chom.chem.structure.Compound;
import java.io.IOException;
import gigadot.chom.chem.structure.CompoundDelegator;
import gigadot.chom.chem.structure.tool.CompoundConverter;
import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemElementUtils;
import gigadot.chom.compchem.CompChemIOUtils;
import gigadot.chom.compchem.orm.annotation.processor.AnnotationUtils;
import gigadot.chom.compchem.orm.annotation.processor.CompChemAnnotationProcessor;
import java.io.File;
import nu.xom.Nodes;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLIdentifier;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLTable;

/**
 *
 * @author Weerapong Phadungsukanan
 */
public class CompChemWriter extends ChemFileWriter {

    protected Compound compound = null;

    @Override
    public void write(String file, Compound compound) throws IOException {
        this.compound = compound;
        CompChemWrapper ccw = AnnotationUtils.getCompChemWrapper(compound);
        if (ccw == null) {
            ccw = new CompChemWrapper();
        }
        CompChemAnnotationProcessor<CompoundDelegator> ccap = new CompChemAnnotationProcessor<CompoundDelegator>(ccw);
        CompChem cc = ccap.getCompChem(this.compound);

        if (compound.getVibrationCount() > 0) {// process frequencies
            CMLProperty vibtable = CompChemElementUtils.getOrAddProperty(ccw.getJob(), "cc:vibrations", CompChem.COMPCHEM_NS);
            double[] df = new double[compound.getVibrationCount()];
            for (int i = 0; i < compound.getVibrationCount(); i++) {
                df[i] = compound.getVibration(i).Frequency;
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

        CompChemIOUtils.write(new File(file), cc);
    }

    protected CMLIdentifier createCMLIdentifier(String convention, String IDValue) throws RuntimeException {
        CMLIdentifier cmlID = new CMLIdentifier();
        cmlID.setConvention(convention);
        cmlID.setCMLValue(IDValue);
        return cmlID;
    }

    protected void addJobListModuleIdentifiers(CMLModule joblist_mod) {
        joblist_mod.appendChild(createCMLIdentifier("chemid:EmpiricalFormula", CompoundConverter.convertToEmpiricalFormula(compound)));
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
