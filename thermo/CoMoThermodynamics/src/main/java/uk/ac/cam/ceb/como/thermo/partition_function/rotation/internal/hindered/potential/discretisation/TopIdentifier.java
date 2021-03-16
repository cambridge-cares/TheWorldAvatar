/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation;

import java.util.ArrayList;
import java.util.Collection;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRUtils;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;

/**
 * @author nk510
 */
/* added as comment 04th-April-2018.
 * import uk.ac.cam.ceb.como.descriptor.molecular.CompoundDescriptionBuilderIntf; 
 */

/**
 *
 * @author pb556
 */
public class TopIdentifier {
    
    protected CMLMolecule mol = null;
    protected Top top = null;
    protected String topDescr = null;
 
    /**
     * @author nk510
     */
 /* comment added 04th-April-2018.
  *    protected CompoundDescriptionBuilderIntf builder = null;
  */
    
    private Logger logger = Logger.getLogger(getClass().getName());
    
    /**
     *@author nk510 
     */
 /* comment added 04th-April-2018. 
  *  public TopIdentifier(CompoundDescriptionBuilderIntf builder) {
        this.builder = builder;
    }
    
  
    public TopIdentifier(CompoundDescriptionBuilderIntf builder, Top top) throws Exception {
        this.builder = builder;
        if (top != null && builder != null) {
            this.top = top;
            topDescr = builder.build(this.top.asCMLMolecule());
        }
    }
    
    public TopIdentifier(CompoundDescriptionBuilderIntf builder, CMLMolecule mol, Top top) throws Exception {
        this.builder = builder;
        if (top != null && builder != null) {
            this.top = top;
            topDescr = builder.build(this.top.asCMLMolecule());
        }
        this.mol = mol;
    }
    
    public TopIdentifier(CompoundDescriptionBuilderIntf builder, String topDescription) {
        this.builder = builder;
        topDescr = topDescription;
    }
    
    public TopIdentifier(CompoundDescriptionBuilderIntf builder, CMLMolecule mol, String topDescription) {
        this.builder = builder;
        topDescr = topDescription;
        this.mol = mol;
    }
  */  
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
     * 
     * @author nk510
     */
 /* comment added: 04th-April-2018.
  * 
  *    public CompoundDescriptionBuilderIntf getCompoundDescriptionBuilder() {
        return builder;
    }
 */   
    public void setCMLMolecule(CMLMolecule mol) {
        this.mol = mol;
    }
    
/**
 * @author nk510    
 */
/* comment added: 04th-April-2018.
 *    public void setTop(Top top) throws Exception {
        if (top != null && builder != null) {
            this.top = top;
            topDescr = builder.build(this.top.asCMLMolecule());
        }
    }
*/    
    
    public void setTopDescription(String topDescription) {
        topDescr = topDescription;
    }

    /**
     * @author nk510
     * 
     * 
     */
/* comment added: 04th-April-2018. 
 *   public void set(CMLMolecule mol, Top top) throws Exception {
        setCMLMolecule(mol);
        setTop(top);
    }
    
    public void setCompoundDescriptionBuilder(CompoundDescriptionBuilderIntf builder) throws Exception {
        this.builder = builder;
        if (top != null && builder != null) {
            topDescr = builder.build(this.top.asCMLMolecule());
        }
    }
    
    public Collection<Top> identify() throws Exception {
        return identify(getCMLMolecule(), getTop());
    }
    
    public Collection<Top> identify(CMLMolecule mol) throws Exception {
        return identify(mol, getTopDescription());
    }
    
    public Collection<Top> identify(Top top) throws Exception {
        return identify(getCMLMolecule(), top);
    }
 */   
    public Collection<Top> identify(String topDescription) throws Exception {
        return identify(getCMLMolecule(), topDescription);
    }
   /**
    *@author nk510   
    */
 /*   public Collection<Top> identify(CMLMolecule mol, Top top) throws Exception {
        return identify(mol, getCompoundDescriptionBuilder().build(top.asCMLMolecule()));
    }
 */   
    public Collection<Top> identify(CMLMolecule mol, String topDescription) throws Exception {
        Collection<Top> identifiedTops = new ArrayList<Top>();
        String refTop = topDescription;
        Top[] tops;
        for (CMLBond b : mol.getBonds()) {
            try {
                if (b.getOrder().compareToIgnoreCase("s") != 0 && b.getOrder().compareToIgnoreCase("1") != 0 ) {
                    continue;
                }
                tops = IRUtils.extractTop(mol, b);
                for (int i = 0; i < tops.length; i++) {
                    CMLMolecule cmlTop = tops[i].asCMLMolecule();
                    if (cmlTop == null) {
                        continue;
                    }
                    if (cmlTop.getBondArray().size() > 0 && cmlTop.getAtomArray().size() > 1) {
                    	
                   /**
                    * @author nk510
                    */
                    	
                    /* added: 04th-April-2018  
                     *  String cmp = getCompoundDescriptionBuilder().build(cmlTop).trim();
                     */
                       
                    	//System.out.println(cmp);
                
                    	
                /**
                  * @author nk510       
                  */
                /* added: 04th-April-2018.
                 *    	if (refTop.trim().equalsIgnoreCase(cmp)) {
                            identifiedTops.add(tops[i]);
                        }
                 */
                    }
                }
            } catch (Exception ex) {
                logger.error("Problems occurred during the identifiation of torsional bonds in molecule " + mol.getId(), ex);
            }
        }
        return identifiedTops;
    }
}
