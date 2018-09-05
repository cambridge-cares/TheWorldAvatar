/*
 * 
 */
package uk.ac.cam.ceb.como.jaxb.xml.generation;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFParser;
import org.apache.jena.riot.RDFParserBuilder;
import org.apache.jena.util.FileManager;
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

import com.github.jsonldjava.core.JsonLdOptions;
import com.google.common.collect.Sets;

import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.structure.Atom;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.compchem.ontology.query.CompChemQuery;
import uk.ac.cam.ceb.como.compchem.property.RotationalConstants;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.AtomArray;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Formula;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Molecule;
import uk.ac.cam.ceb.como.io.chem.file.parser.cml.CMLMoleculeParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemIOUtils;
import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.formula.EmpiricalFormulaParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.GaussianParser;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;

// TODO: Auto-generated Javadoc
/**
 * The Class ParsingTestExamples.
 */

public class ParsingTestExamples {

    public static final String TBOX_SOURCE = "./src/test/resources/ontology/ontochem_abox/Cl.rdf";
	public static final String ABOX_SOURCE="./src/test/resources/ontology/ontochem_abox/";
	public static final String TRAGET_FOLDER="./src/test/resources/ontology/sparql_results/";
	public static final String SPARQL_FOLDER ="./src/test/resources/ontology/sparql_query/";
//  static CompChemPropertyParser ccpp = new CompChemPropertyParser();

/**
 * The main method.
 *
 * @param args the arguments
 * @throws Exception the exception
 */

public static void main(String[] args) throws Exception {	
	
	    String sparqlPath = "src/test/resources/ontology/sparql_query/query_all.sparql";
	    
	    File sparql = new File(sparqlPath);
	    
	    File abox =new File(TBOX_SOURCE);

	    OntModel model = CompChemQuery.getOntModel(TBOX_SOURCE, abox.getAbsolutePath());
		
		String q = FileUtils.readFileToString(sparql);
		
		CompChemQuery.performQuery(model, q, abox.getName(), TRAGET_FOLDER);
		
	    
	    
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
		
//		 FrequencyParser parser = new FrequencyParser();
//		 parser.set(f);
//		 parser.parse();
//		 
//		 CompChem cc = (CompChem) parser.get();
//
//		 CompChemParser ccp = new CompChemParser();
//		 
//		 ccp.parse(cc);
		 
//		 RotationalConstants rc = ccp.get().getRotationalConstants();
		 
//		 System.out.println("Rotational Constants: " + rc.getValue(2));
		 
//         IRCompChemWrapper irccw = new IRCompChemWrapper(cc);
         
//       CompChemWrapper ccw = new CompChemWrapper(cc);
         
//       CompChemIOUtils.write(System.out, ccw.getCompchem());
         
//		 List<CMLParameter> cmlP = irccw.getParameters();
//
//		 for(CMLParameter p: cmlP) {
//		 	
//		    	/**
//				 *
//				 * Generate : basis set, level of theory,
//				 *
//				 */
//			 
//			 System.out.println("feature: " + p.getDictRef() + ",  value : " + p.getValue());
//				
//		}

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
		 
//EmpiricalFormulaParser empiricalFormulaParser = new EmpiricalFormulaParser();
		
//System.out.println("formula: " + empiricalFormulaParser.parse("Fe22") + ", atom name: " + empiricalFormulaParser.getAtomName("Fe22") + " atom number: " + empiricalFormulaParser.getAtomSum("Fe22") );







}



}