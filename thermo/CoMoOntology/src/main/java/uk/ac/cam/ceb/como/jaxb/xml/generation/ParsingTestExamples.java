/*
 * 
 */
package uk.ac.cam.ceb.como.jaxb.xml.generation;

import java.io.File;
import java.util.List;

import org.xmlcml.cml.base.CMLAttribute;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.base.CMLElements;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLAtomArray;
import org.xmlcml.cml.element.CMLAtomParity;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.element.CMLName;
import org.xmlcml.cml.element.CMLParameter;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLPropertyList;

import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.structure.Atom;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.compchem.property.RotationalConstants;
import uk.ac.cam.ceb.como.io.chem.file.parser.cml.CMLMoleculeParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemIOUtils;
import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.GaussianParser;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;

// TODO: Auto-generated Javadoc
/**
 * The Class ParsingTestExamples.
 */

public class ParsingTestExamples {

//  static CompChemPropertyParser ccpp = new CompChemPropertyParser();

/**
 * The main method.
 *
 * @param args the arguments
 * @throws Exception the exception
 */

public static void main(String[] args) throws Exception {

		 String path = "src/test/resources/g09/Cl2.g09";
//		 String path = "src/test/resources/g09/Cl2O6.g09";

		 File f = new File(path);
		
//		 GaussianParser parser  = new FrequencyParser();

//		 FrequencyParser parser = new FrequencyParser();
//		 GeometryParser parser = new GeometryParser();
//       GaussianHRParser parser = new GaussianHRParser();
		
//		 parser.set(f);
//		 parser.parse();
		
//		 parser.parseSection();
//		 CompChem cc = (CompChem) parser.get();
//		 CompChemWrapper ccw = new CompChemWrapper(cc);
//		 CompChemIOUtils.write(System.out, ccw.getCompchem());
//		 IRCompChemWrapper irccw = new IRCompChemWrapper(cc);
//		 CompChemIOUtils.write(System.out, irccw.getCompchem()); 	
//		 List<CMLProperty> prop = irccw.getProperties();//
		
//		 for (CMLProperty cmlp : prop) {
			
//		 System.out.println("+ " + cmlp.getLocalName() + "" + cmlp);
			
//		 List<CMLElement> child_elem = cmlp.getDescendants();
			
//		 for(CMLElement c_elem: child_elem ) {
			
//		 System.out.println("  :" + c_elem);
			
//		 	}
//		 }

//		 CompChemIOUtils.write(System.out, irccw.getCompchem());
		
//		 CMLMoleculeParser parser = new CMLMoleculeParser();	
		
//		 m.getConvention();
		
		 FrequencyParser parser = new FrequencyParser();
		 parser.set(f);
		 parser.parse();
		 
		 CompChem cc = (CompChem) parser.get();

		 CompChemParser ccp = new CompChemParser();
		 
		 ccp.parse(cc);
		 
//		 RotationalConstants rc = ccp.get().getRotationalConstants();
		 
//		 System.out.println("Rotational Constants: " + rc.getValue(2));
		 
         IRCompChemWrapper irccw = new IRCompChemWrapper(cc);
         
//       CompChemWrapper ccw = new CompChemWrapper(cc);
         
//       CompChemIOUtils.write(System.out, ccw.getCompchem());
         
		 List<CMLParameter> cmlP = irccw.getParameters();

		 for(CMLParameter p: cmlP) {
		 	
		    	/**
				 *
				 * Generate : basis set, level of theory,
				 *
				 */
			 
			 System.out.println("feature: " + p.getDictRef() + ",  value : " + p.getValue());
				
		}

//		 CMLMolecule m = irccw.getFinalMolecule();
		 
//		 System.out.println("m.getAtomCount(): " + m.getAtomCount());

//		 CMLAtomArray aa = m.getAtomArray();
//
//		 List<CMLAtom> atomList = aa.getAtoms();
//		 
//		 CMLMolecule cmlElm = aa.getMolecule();
//		 
//		 Element element = new Element();
//		 
//		 System.out.println("element.getAtomicWeight() : " + element.getAtomicWeight());
//		 
//		 for(CMLAtom a: atomList) {
//
//			 System.out.println(a.getAtomParityElements().getList());
//			 
//		 }

//			CompChemIOUtils.write(System.out, irccw.getCompchem());	
//			CMLMolecule m = irccw.getInitialMolecule();			
//			System.out.println("m.getAtomCount() : " + m.getAtomCount() + "" );
//			System.out.println(rc.toString());			
//		    CMLElements<CMLArray> n_name = m.getArrayElements();
//			CMLElements<CMLMolecule> n_name = m.getMoleculeElements();
//		    List<CMLArray> atom_array = n_name.getList();             
//			List<CMLMolecule> atom_array = n_name.getList();			
//			for(CMLMolecule cmlm: atom_array) {
			
//				CMLElements<CMLPropertyList> plist = cmlm.getPropertyListElements();
//				List<CMLPropertyList> iPList = plist.getList();
//				for(CMLPropertyList pl: iPList) {
//					System.out.println(pl.getDictRef());
//				}				
//			}			
//		for(CMLArray cml_aa: atom_array) {		
//		System.out.println(cml_aa.getArraySize());		
//		}
//		List<CMLAtom> atom = m.getAtoms();	
//		for(CMLAtom a: atom) {
//		System.out.println(a.getElementType() + " " + a.getX3() + " " );			
//		}
	}
}