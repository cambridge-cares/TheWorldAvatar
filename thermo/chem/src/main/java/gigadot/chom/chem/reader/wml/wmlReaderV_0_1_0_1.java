package gigadot.chom.chem.reader.wml;

import gigadot.chom.chem.property.Displacement;
import gigadot.chom.chem.property.Vibration;
import gigadot.chom.chem.structure.Bond;
import gigadot.chom.chem.structure.BondType;
import gigadot.chom.chem.structure.Compound;
import gigadot.chom.chem.structure.tool.StructureTools;
import gigadot.chom.model.cookie.EnthalpyOfFormation;
import gigadot.chom.compchem.property.PMOI;
import gigadot.chom.compchem.property.RotationalConstants;
import gigadot.chom.model.brownie.Atom;
import gigatools.lite.constant.PhysicalConstants;
import gigatools.lite.math.DoubleTool;
import java.io.IOException;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Elements;

/**
 *
 *
 * @author Weerapong Phadungsukanan
 */
public class wmlReaderV_0_1_0_1 extends wmlReaderBase {

    protected Compound compound = null;
    protected Element wmlRoot = null;

    @Override
    public void read(Document xml, Compound compound) throws IOException {
        compound.clear();
        this.compound = compound;
        wmlRoot = xml.getRootElement();

        readAtomList();
        readBondList();
        compound.recreateMoleculeList();

        readInChI();
        readSymmetryNumber();
        readSCFEnergy();
        readEnthalpyOfFormation();
        readPricipalMOI();
        readRotationalConstants();
        readFrequencyData();
        readCharge();
        readSpinMultiplicity();
    }

    protected void readAtomList() {
        Element elem = wmlRoot.getFirstChildElement("atoms");
        if (elem != null) {
            Elements elems = elem.getChildElements("atom");
            for (int i = 0; i < elems.size(); i++) {
                String symbol = elems.get(i).getAttributeValue("symbol");
                int order = Integer.parseInt(elems.get(i).getAttributeValue("order"));
                Atom atom = new Atom(symbol);
                compound.addAtom(atom);
                atom.setOrder(order);
                double posX = DoubleTool.parseDouble(elems.get(i).getFirstChildElement("positionX").getValue());
                double posY = DoubleTool.parseDouble(elems.get(i).getFirstChildElement("positionY").getValue());
                double posZ = DoubleTool.parseDouble(elems.get(i).getFirstChildElement("positionZ").getValue());
                atom.setCoordinateInA(posX, posY, posZ);
            }
        }
    }

    protected BondType readBondType(String stype) throws IOException {
        int type = Integer.parseInt(stype);
        return BondType.parseWMLBondType(type);
    }

    protected void readBondList() throws IOException {
        Element elem = wmlRoot.getFirstChildElement("bonds");
        if (elem != null) {
            Elements elems = elem.getChildElements("bond");
            for (int i = 0; i < elems.size(); i++) {
                int A = Integer.parseInt(elems.get(i).getAttributeValue("atomA"));
                int B = Integer.parseInt(elems.get(i).getAttributeValue("atomB"));
                // in this version bond type is labelled by integer.
                Bond.createBond(compound.getAtomByOrder(A), compound.getAtomByOrder(B), readBondType(elems.get(i).getAttributeValue("type")), compound);
            }
        }
    }

    protected void readEnthalpyOfFormation() {
        Element elem = wmlRoot.getFirstChildElement("enthalpyOfFormation");
        if (elem != null) {
            // Add Enthalpy of formation
            double T = (DoubleTool.parseDouble(elem.getAttributeValue("temperature")));
            double DHfInkcal_Per_mol = (DoubleTool.parseDouble(elem.getValue()));
            compound.setHf(EnthalpyOfFormation.from_kcal_Per_mol(T, DHfInkcal_Per_mol));
        }
    }

    protected void readFrequencyData() throws IOException {
        Element elem = wmlRoot.getFirstChildElement("frequencies");
        if (elem != null) {
            Elements elems = elem.getChildElements("frequencyData");
            for (int i = 0; i < elems.size(); i++) {
                Vibration f_data = new Vibration();
                f_data.Frequency = DoubleTool.parseDouble(elems.get(i).getFirstChildElement("frequency").getValue());

                Element subelem = elems.get(i).getFirstChildElement("forceConstant");
                if (subelem != null) {
                    f_data.ForceConstant = DoubleTool.parseDouble(subelem.getValue());
                }

                subelem = elems.get(i).getFirstChildElement("reducedMass");
                if (subelem != null) {
                    f_data.ReducedMass = DoubleTool.parseDouble(subelem.getValue());
                }

                subelem = elems.get(i).getFirstChildElement("irIntensity");
                if (subelem != null) {
                    f_data.IRIntensity = DoubleTool.parseDouble(subelem.getValue());
                }

                subelem = elems.get(i).getFirstChildElement("rotorBondAxis");
                if (subelem != null) {
                    Elements roelems = subelem.getChildElements("bond");
                    for (int j = 0; j < roelems.size(); j++) {
                        int A = Integer.parseInt(roelems.get(j).getAttributeValue("atomA"));
                        int B = Integer.parseInt(roelems.get(j).getAttributeValue("atomB"));
                        Bond robond = Bond.createBond(compound.getAtomByOrder(A), compound.getAtomByOrder(B), readBondType(roelems.get(j).getAttributeValue("type")), compound);
                        try {
                            double torsionBarrier = DoubleTool.parseDouble(roelems.get(j).getAttributeValue("torsionBarrier"));
                            robond.setTorsionBarrierInkcal_Per_mol(torsionBarrier * PhysicalConstants.Cal * 1000 / PhysicalConstants.NA);
                        } catch (NullPointerException ex) {
                            // No barrier;
                        }
                        f_data.addIRotorBond(robond);
                    }
                }

                subelem = elems.get(i).getFirstChildElement("displacements");
                if (subelem != null) {
                    Elements diselems = subelem.getChildElements("displacement");
                    for (int j = 0; j < diselems.size(); j++) {
                        double dx = DoubleTool.parseDouble(diselems.get(j).getFirstChildElement("dx").getValue());
                        double dy = DoubleTool.parseDouble(diselems.get(j).getFirstChildElement("dy").getValue());
                        double dz = DoubleTool.parseDouble(diselems.get(j).getFirstChildElement("dz").getValue());
                        Displacement dist = new Displacement();
                        dist.setdxInA(dx);
                        dist.setdyInA(dy);
                        dist.setdzInA(dz);
                        f_data.DisplacementList.add(dist);
                    }
                }

                compound.addVibration(f_data);
            }
        }
    }
    protected String key_principalMOI = "pricipalMOI";

    protected void readPricipalMOI() {
        Element elem = wmlRoot.getFirstChildElement(key_principalMOI);
        if (elem != null) {
            // Add PricipalMOI
            PMOI pmoi = new PMOI();
            Elements elems = elem.getChildElements("eigen");
            for (int i = 0; i < elems.size(); i++) {
                pmoi.setMOIIn_amu_rbohr2(i, DoubleTool.parseDouble(elems.get(i).getAttributeValue("value")));
                pmoi.setPrincipalAxis(i,
                        DoubleTool.parseDouble(elems.get(i).getFirstChildElement("ex").getValue()),
                        DoubleTool.parseDouble(elems.get(i).getFirstChildElement("ey").getValue()),
                        DoubleTool.parseDouble(elems.get(i).getFirstChildElement("ez").getValue()));
            }
            compound.setPricipleMOI(pmoi);
        } else {
            if (compound.getMoleculeCount() == 1) {
                compound.setPricipleMOI(StructureTools.calculatePrincipalMOI(compound.getMolecule(0)));
            } else {
                throw new RuntimeException("Zero or more than 1 molecule found in Compound.");
            }
        }
    }

    protected void readRotationalConstants() {
        Element elem = wmlRoot.getFirstChildElement("rotationalConstants");
        if (elem != null) {
            // Add PricipalMOI
            Elements elems = elem.getChildElements("rotVal");
            if (elems.size() == 3) {
                double[] rotcInGHz = new double[3];
                for (int i = 0; i < elems.size(); i++) {
                    rotcInGHz[i] = DoubleTool.parseDouble(elems.get(i).getAttributeValue("value"));
                }
                RotationalConstants rotConst = RotationalConstants.from_GHz(rotcInGHz);
                compound.setRotationalConstants(rotConst);
            }
        }
    }

    protected void readSCFEnergy() {
        Element elem = wmlRoot.getFirstChildElement("energySCF");
        if (elem != null) {
            compound.setFinalSCFEnergyInHartree(DoubleTool.parseDouble(elem.getValue()));
            String type = elem.getAttributeValue("type");
            if (type != null) {
                compound.setMethod(type);
            }
        }
    }

    protected void readInChI() {
        Element elem = wmlRoot.getFirstChildElement("inchi");
        if (elem != null) {
            compound.setInchi(elem.getValue());
        }
    }

    protected void readSymmetryNumber() {
        Element elem = wmlRoot.getFirstChildElement("n_symmetry");
        if (elem != null) {
            compound.setRotationalSymmetryNumber(Integer.parseInt(elem.getValue()));
        }
    }

    protected void readCharge() {
        Element elem = wmlRoot.getFirstChildElement("charge");
        if (elem != null) {
            compound.setCharge(Integer.parseInt(elem.getValue()));
        } else {
            compound.setCharge(0);
        }
    }

    protected void readSpinMultiplicity() {
        Element elem = wmlRoot.getFirstChildElement("spinMultiplicity");
        if (elem != null) {
            compound.setMultiplicity(Integer.parseInt(elem.getValue()));
        } else {
            compound.setMultiplicity(1);
            logger.warn("Spin multiplicity not found. A singlet state is set to molecule.");
        }
    }
}
