/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.chem.file.writer.gau;

import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;
import org.xmlcml.cml.element.CMLMolecule;
import uk.ac.cam.ceb.como.io.chem.file.writer.ChemFileWriter;
import uk.ac.cam.ceb.como.openbabel.cml.OpenBabelCMLConverter;

/**
 *
 * @author pb556
 */
public class GauWriter extends ChemFileWriter<CMLMolecule> implements GauWriterIntf {

    private String rMethod = "# no route card defined";
    private String uMethod = "# no route card defined";
    private String title = " ";
    private String routeCard = "# no route card defined";
    private boolean fRMethod = false;
    private boolean fUMethod = false;
    private final Logger logger = Logger.getLogger(getClass());

    public GauWriter() {
    }

    @Override
    public void setRouteCard(String routeCard) throws Exception {
        this.routeCard = routeCard;
    }

    @Override
    public void setRMethod(String method) throws Exception {
        fRMethod = true;
        rMethod = method;
    }

    @Override
    public void setUMethod(String method) throws Exception {
        fUMethod = true;
        uMethod = method;
    }

    @Override
    public void setTitle(String title) throws Exception {
        this.title = title;
    }

    @Override
    public String getRMethod() throws Exception {
        return rMethod;
    }

    @Override
    public String getUMethod() throws Exception {
        return uMethod;
    }

    private String genContent() throws Exception {
        if (getContent() == null) {
            throw new Exception("No CMLMolecule defined.");
        }
        try {
            CMLMolecule mol = getContent();
            String convert = OpenBabelCMLConverter.convert(mol, "gau");

            String route = getRouteCard();
            if (fRMethod && fUMethod) {
                if (mol.getSpinMultiplicity() == 1) {
                    route = route.replace(getUMethod(), getRMethod());
                } else {
                    route = route.replace(getRMethod(), getUMethod());
                }
            }
            convert = convert.replace("#Put Keywords Here, check Charge and Multiplicity.", route);
            return convert.replaceFirst("0  1", mol.getFormalCharge() + "  " + mol.getSpinMultiplicity());
        } catch (Exception ex) {
            logger.error("Conversion of CMLMolecule object was not possible.");
        }
        return null;
    }

    @Override
    public String getRouteCard() throws Exception {
        return routeCard;
    }

    @Override
    public String getTitle() throws Exception {
        return title;
    }

    @Override
    public void write() throws Exception {
        try {
            if (getFile() == null) {
                throw new Exception("No output file defined.");
            }
        } catch (Exception ex) {
            throw new Exception("Invalid output file defined.");
        }
        try {
            String convert = genContent();
            FileUtils.writeStringToFile(getFile(), convert);
            return;
        } catch (Exception ex) {
        }
        throw new Exception("Unsuccessful writing operation.");
    }

    @Override
    public void clear() throws Exception {
        rMethod = "# no route card defined";
        uMethod = "# no route card defined";
        title = " ";
        routeCard = "# no route card defined";
        fRMethod = false;
        fUMethod = false;
        try {
            setContent(null);
        } catch (Exception ex) {
            throw new Exception(ex);
        }
    }
}
