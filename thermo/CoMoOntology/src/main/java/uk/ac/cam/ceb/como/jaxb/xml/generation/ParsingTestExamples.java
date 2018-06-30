/*
 * 
 */
package uk.ac.cam.ceb.como.jaxb.xml.generation;

import java.io.File;

import org.xmlcml.cml.element.CMLMolecule;

import uk.ac.cam.ceb.como.compchem.CompChem;

import uk.ac.cam.ceb.como.compchem.property.RotationalConstants;

import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
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

//		String path = "src/test/resources/g09/freq-hr-fine-species-0232-radical-0-restricted.g09";
//		String path = "src/test/resources/g09/Cl2O6.g09";

//		File f = new File(path);
		
//		GaussianParser parser  = new FrequencyParser();
//		FrequencyParser parser = new FrequencyParser();
//		GeometryParser parser = new GeometryParser();
//      GaussianHRParser parser = new GaussianHRParser();
//		parser.set(f);
//		parser.parse();
//		parser.parseSection();
//		CompChem cc = (CompChem) parser.get();
//		CompChemWrapper ccw = new CompChemWrapper(cc);
//		CompChemIOUtils.write(System.out, ccw.getCompchem());
//		IRCompChemWrapper irccw = new IRCompChemWrapper(cc);		
//		CompChemIOUtils.write(System.out, irccw.getCompchem());		
//		List<CMLProperty> prop = irccw.getProperties();//
//		for (CMLProperty cmlp : prop) {
//			System.out.println("+ " + cmlp.getLocalName());			
//			List<CMLElement> child_elem = cmlp.getDescendants();			
//			for(CMLElement c_elem: child_elem ) {				
//				System.out.println("  :" + c_elem);
//			}
//		}
		
//		CMLMoleculeParser parser = new CMLMoleculeParser();
		
//		
//		m.getConvention();
		
//		 FrequencyParser parser = new FrequencyParser();
//		 parser.set(f);
//		 parser.parse();
//		 CompChem cc = (CompChem) parser.get();
//		 CompChemParser ccp = new CompChemParser();
//		 ccp.parse(cc);
//		 RotationalConstants rc = ccp.get().getRotationalConstants();
//		
//		 
//		 System.out.println("Rotational Constants: " + rc.getValue(2));
//
//			IRCompChemWrapper irccw = new IRCompChemWrapper(cc);		
//			
//			CMLMolecule m = irccw.getFinalMolecule();
//			
//			System.out.println("m.getAtomCount() : " + m.getAtomCount());
//
//			System.out.println(rc.toString());

			
//		CMLElements<CMLArray> n_name = m.getArrayElements();
//
//		List<CMLArray> atom_array = n_name.getList();
//
//		for(CMLArray cml_aa: atom_array) {
		
//		System.out.println(cml_aa.getArraySize());
		
//		}
//		
//		List<CMLAtom> atom = m.getAtoms();
//		
//		for(CMLAtom a: atom) {
//			
//			System.out.println(a.getElementType() + " " + a.getX3());
//		}
	}
}