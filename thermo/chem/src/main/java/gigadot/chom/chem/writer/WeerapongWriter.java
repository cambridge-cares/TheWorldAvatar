package gigadot.chom.chem.writer;

import gigatools.lite.constant.PhysicalConstants;
import gigadot.chom.compchem.property.PMOI;
import gigadot.chom.compchem.property.RotationalConstants;
import gigadot.chom.model.brownie.Atom;
import gigadot.chom.chem.structure.Bond;
import gigadot.chom.chem.property.Vibration;
import gigadot.chom.chem.structure.Compound;
import gigadot.chom.model.cookie.EnthalpyOfFormation;
import java.io.IOException;
import nu.xom.Attribute;
import nu.xom.Document;
import nu.xom.Element;
import gigatools.extra.xom.XMLTools;
import java.io.File;

/**
 *
 * @deprecated In the future this will be removed. CompChem format will be used from now on
 * @author Weerapong Phadungsukanan
 */
public class WeerapongWriter extends ChemFileWriter {

    private Compound mDoc = null;
    private Document doc = null;
    private Element root = null;
    public static final String version = "0.1.0.3";

    @Override
    public void write(String file, Compound jDoc) throws IOException {
        this.mDoc = jDoc;
        root = new Element("jchem");
        //root.addAttribute(new Attribute("version", "0a"));
        //root.addAttribute(new Attribute("version", "0b"));
        //root.addAttribute(new Attribute("version", "0c"));
        //root.addAttribute(new Attribute("version", "0d"));
        // Version : major.minor[.maintenance[.build]]
        // maintenance :
        // 0 for alpha status
        // 1 for beta status
        // 2 for release candidate
        // 3 for public release
        root.addAttribute(new Attribute("version", version));
        doc = new Document(root);
        // Add smiles
        // Element elem = new Element("smiles");
        // elem.appendChild(jDoc.getSMILES());
        // root.appendChild(elem);
        // Add symmetry
        Element elem = new Element("n_symmetry");
        elem.appendChild(jDoc.getRotationalSymmetryNumber() + "");
        root.appendChild(elem);
        // Add charge
        elem = new Element("charge");
        elem.appendChild(jDoc.getCharge() + "");
        root.appendChild(elem);
        // Add spin multiplicity
        elem = new Element("spinMultiplicity");
        elem.appendChild(jDoc.getSpinMultiplicity() + "");
        root.appendChild(elem);
        // Add SCF Energy
        elem = new Element("energySCF");
        elem.addAttribute(new Attribute("type", jDoc.getMethod()));
        elem.addAttribute(new Attribute("unit", "Hartree"));
        elem.appendChild(jDoc.getFinalSCFEnergyInHartree() + "");
        root.appendChild(elem);
        // Add Enthalpy of formation
        EnthalpyOfFormation enf = jDoc.getHf();
        if (enf == null) {
            enf = new EnthalpyOfFormation(0d);
        }
        // Always write ethalpy of formation even if it doesn't exist.
        {
            elem = new Element("enthalpyOfFormation");
            elem.addAttribute(new Attribute("temperature", enf.getTemperature() + ""));
            elem.addAttribute(new Attribute("unit", "kcal/mol"));
            elem.appendChild(enf.getValueInkcal_Per_mol() + "");
            root.appendChild(elem);
        }
        // Add moment of inertia from gaussian
        PMOI pmoi = jDoc.getPrincipalMOI();
        if (pmoi != null) {
            elem = new Element("principalMOI");
            for (int i = 0; i < 3; i++) {
                Element selem = new Element("eigen");
                selem.addAttribute(new Attribute("value", pmoi.getMOIIn_amu_rbohr2(i) + ""));
                // Adding eigen vector
                Element sselem = new Element("ex");
                sselem.appendChild(pmoi.getPrincipalAxis(i)[0] + "");
                selem.appendChild(sselem);

                sselem = new Element("ey");
                sselem.appendChild(pmoi.getPrincipalAxis(i)[1] + "");
                selem.appendChild(sselem);

                sselem = new Element("ez");
                sselem.appendChild(pmoi.getPrincipalAxis(i)[2] + "");
                selem.appendChild(sselem);

                elem.appendChild(selem);
            }
            root.appendChild(elem);
        }
        // Add Rotational Constants from gaussian
        RotationalConstants rConst = jDoc.getRotationalConstants();
        if (rConst != null) {
            elem = new Element("rotationalConstants");
            for (int i = 0; i < 3; i++) {
                Element selem = new Element("rotVal");
                selem.addAttribute(new Attribute("value", rConst.getValueInGHz(i) + ""));
                elem.appendChild(selem);
            }
            root.appendChild(elem);
        }

        addAtomList();
        addBondList();
        addFrequencyData();

        XMLTools.writeXML(new File(file), doc);
    }

    private void addAtomList() {
        Element elem = new Element("atoms");

        for (int i = 0; i < mDoc.getAtomCount(); i++) {
            Element subelem = new Element("atom");
            Atom atom = mDoc.getAtom(i);
            subelem.addAttribute(new Attribute("symbol", atom.getElement().symbol));
            subelem.addAttribute(new Attribute("order", atom.getOrder() + ""));

            Element sselem = new Element("atomicNumber");
            sselem.appendChild(atom.getElement().atomicNumber + "");
            subelem.appendChild(sselem);

            sselem = new Element("atomicWeight");
            sselem.appendChild(atom.getElement().atomicWeight + "");
            subelem.appendChild(sselem);

            sselem = new Element("positionX");
            sselem.appendChild(atom.getXInA() + "");
            subelem.appendChild(sselem);

            sselem = new Element("positionY");
            sselem.appendChild(atom.getYInA() + "");
            subelem.appendChild(sselem);

            sselem = new Element("positionZ");
            sselem.appendChild(atom.getZInA() + "");
            subelem.appendChild(sselem);

            elem.appendChild(subelem);
        }
        root.appendChild(elem);
    }

    private void addBondList() {
        Element elem = new Element("bonds");

        for (int i = 0; i < mDoc.getBondCount(); i++) {
            Element subelem = new Element("bond");
            Bond bond = mDoc.getBond(i);
            subelem.addAttribute(new Attribute("type", bond.getBondType().name().toLowerCase() + ""));
            subelem.addAttribute(new Attribute("atomA", bond.getAtomA().getOrder() + ""));
            subelem.addAttribute(new Attribute("atomB", bond.getAtomB().getOrder() + ""));

            elem.appendChild(subelem);
        }
        root.appendChild(elem);
    }

    private void addFrequencyData() {
        Element elem = new Element("frequencies");

        for (int i = 0; i < mDoc.getVibrationCount(); i++) {
            Element subelem = new Element("frequencyData");
            Vibration f_data = mDoc.getVibration(i);

            Element sselem = new Element("frequency");
            sselem.appendChild(f_data.Frequency + "");
            subelem.appendChild(sselem);

            sselem = new Element("forceConstant");
            sselem.appendChild(f_data.ForceConstant + "");
            subelem.appendChild(sselem);

            sselem = new Element("reducedMass");
            sselem.appendChild(f_data.ReducedMass + "");
            subelem.appendChild(sselem);

            sselem = new Element("irIntensity");
            sselem.appendChild(f_data.IRIntensity + "");
            subelem.appendChild(sselem);

            if (f_data.getIRotorBondCount() > 0) {
                sselem = new Element("rotorBondAxis");
                for (int j = 0; j < f_data.getIRotorBondCount(); j++) {
                    Element s3elem = new Element("bond");
                    Bond bond = f_data.getIRotorBond(j);
                    s3elem.addAttribute(new Attribute("type", bond.getBondType().name().toLowerCase() + ""));
                    s3elem.addAttribute(new Attribute("atomA", bond.getAtomA().getOrder() + ""));
                    s3elem.addAttribute(new Attribute("atomB", bond.getAtomB().getOrder() + ""));
                    s3elem.addAttribute(new Attribute("torsionBarrier", bond.getTorsionBarrierInkcal_Per_mol() / PhysicalConstants.Cal / 1000 * PhysicalConstants.NA + ""));

                    sselem.appendChild(s3elem);
                }
                subelem.appendChild(sselem);
            }

            if (f_data.DisplacementList.size() > 0) {
                sselem = new Element("displacements");
                for (int j = 0; j < f_data.DisplacementList.size(); j++) {
                    Element s3elem = new Element("displacement");

                    Element s4elem = new Element("dx");
                    s4elem.appendChild(f_data.DisplacementList.get(j).dxInA() + "");
                    s3elem.appendChild(s4elem);

                    s4elem = new Element("dy");
                    s4elem.appendChild(f_data.DisplacementList.get(j).dyInA() + "");
                    s3elem.appendChild(s4elem);

                    s4elem = new Element("dz");
                    s4elem.appendChild(f_data.DisplacementList.get(j).dzInA() + "");
                    s3elem.appendChild(s4elem);

                    sselem.appendChild(s3elem);
                }
                sselem.addAttribute(new Attribute("unit", "A"));
                subelem.appendChild(sselem);
            }

            elem.appendChild(subelem);
        }
        if (mDoc.getVibrationCount() > 0) {
            root.appendChild(elem);
        }
    }
}
