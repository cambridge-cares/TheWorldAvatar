/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * author nk510
 *  disabled import: uk.ac.cam.ceb.como.descriptor.molecular.CompoundDescriptionBuilderIntf;
 *  
 *  Comment: There is no CoMoMolecularDescriptors project in Bitbucket.
 *  
 *  
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation;

import java.util.ArrayList;
import java.util.Collection;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRUtils;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;
//import uk.ac.cam.ceb.como.descriptor.molecular.CompoundDescriptionBuilderIntf;

/**
 *
 * @author pb556
 */
public class TorsionalBondIdentifier {

    protected CMLMolecule mol = null;
    protected Top top = null;
    protected String topDescr = null;
    /**
     * @author nk510
     */
//    protected CompoundDescriptionBuilderIntf builder = null;
    
    private Logger logger = Logger.getLogger(getClass().getName());
    
    /**
     * @author nk510
     * @return
     */
/*    public TorsionalBondIdentifier(CompoundDescriptionBuilderIntf builder) {
        this.builder = builder;
    }
    
    public TorsionalBondIdentifier(CompoundDescriptionBuilderIntf builder, Top top) throws Exception {
        this.builder = builder;
        if (top != null && builder != null) {
            this.top = top;
            topDescr = builder.build(this.top.asCMLMolecule());
        }
    }
    
     public TorsionalBondIdentifier(CompoundDescriptionBuilderIntf builder, CMLMolecule mol, Top top) throws Exception {
        this.builder = builder;
        if (top != null && builder != null) {
            this.top = top;
            topDescr = builder.build(this.top.asCMLMolecule());
        }
        this.mol = mol;
    }
    
    public TorsionalBondIdentifier(CompoundDescriptionBuilderIntf builder, String topDescription) {
        this.builder = builder;
        topDescr = topDescription;
    }
    
    public TorsionalBondIdentifier(CompoundDescriptionBuilderIntf builder, CMLMolecule mol, String topDescription) {
        this.builder = builder;
        topDescr = topDescription;
        this.mol = mol;
    }*/
    
    public CMLMolecule getCMLMolecule() {
        return mol;
    }
    
    public Top getTop() {
        return top;
    }
    
    public String getTopDescription() {
        return topDescr;
    }
    
  /**
   * @author nk510  
   * @param mol
   */
  /*  public CompoundDescriptionBuilderIntf getCompoundDescriptionBuilder() {
        return builder;
    }*/
    
    public void setCMLMolecule(CMLMolecule mol) {
        this.mol = mol;
    }
    
    /**
     * @author nk510
     * @param topDescription
     */
  /*  public void setTop(Top top) throws Exception {
        if (top != null && builder != null) {
            this.top = top;
            topDescr = builder.build(this.top.asCMLMolecule());
        }
    }*/
    
    public void setTopDescription(String topDescription) {
        topDescr = topDescription;
    }


//    public void set(CMLMolecule mol, Top top) throws Exception {
//        setCMLMolecule(mol);
//        setTop(top);
//    }
    
//    public void setCompoundDescriptionBuilder(CompoundDescriptionBuilderIntf builder) throws Exception {
//        this.builder = builder;
//        if (top != null && builder != null) {
//            topDescr = builder.build(this.top.asCMLMolecule());
//        }
//    }
    
//    public Collection<CMLBond> identify() throws Exception {
//        return identify(getCMLMolecule(), getTop());
//    }
    
    public Collection<CMLBond> identify(CMLMolecule mol) throws Exception {
        return identify(mol, getTopDescription());
    }
    
//    public Collection<CMLBond> identify(Top top) throws Exception {
//        return identify(getCMLMolecule(), top);
//    }
    
    public Collection<CMLBond> identify(String topDescription) throws Exception {
        return identify(getCMLMolecule(), topDescription);
    }
    
//    public Collection<CMLBond> identify(CMLMolecule mol, Top top) throws Exception {
//        return identify(mol, getCompoundDescriptionBuilder().build(top.asCMLMolecule()));
//    }
    
    public Collection<CMLBond> identify(CMLMolecule mol, String topDescription) throws Exception {
        Collection<CMLBond> identifiedTorsBonds = new ArrayList<CMLBond>();
        String refTop = topDescription;
        Top[] tops;
        for (CMLBond b : mol.getBonds()) {
            try {
                if (b.getOrder().compareToIgnoreCase("s") != 0 && b.getOrder().compareToIgnoreCase("1") != 0 ) {
                    continue;
                }
                //System.out.println(b.getAtomId(0) + " " + b.getAtomId(1));
                tops = IRUtils.extractTop(mol, b);
                for (int i = 0; i < tops.length; i++) {
/**
 * @author nk510
 */
/*
 * 
 * old comments:
 * 
 *                     for (CMLAtom aTop : tops[i].getAtoms()) {
                        System.out.print(aTop.getId() + ", ");
                    }
*/                    System.out.println();
                    CMLMolecule cmlTop = tops[i].asCMLMolecule();
                    if (cmlTop == null) {
                        continue;
                    }
                    if (cmlTop.getBondArray().size() > 0 && cmlTop.getAtomArray().size() > 1) {
//                        String cmp = getCompoundDescriptionBuilder().build(cmlTop).trim();
                        //System.out.println(cmp);
//                        if (refTop.trim().equalsIgnoreCase(cmp)) {
//                            identifiedTorsBonds.add(b);
//                        }
                    }
                }
            } catch (Exception ex) {
                logger.error("Problems occurred during the identifiation of torsional bonds in molecule " + mol.getId(), ex);
            }
        }
        return identifiedTorsBonds;
    }
}
