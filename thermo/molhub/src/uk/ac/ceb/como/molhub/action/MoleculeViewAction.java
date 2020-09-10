package uk.ac.ceb.como.molhub.action;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.apache.struts2.ServletActionContext;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.ceb.como.molhub.bean.AtomicMass;
import uk.ac.ceb.como.molhub.bean.ElectronicEnergy;
import uk.ac.ceb.como.molhub.bean.FormalCharge;
import uk.ac.ceb.como.molhub.bean.Frequency;
import uk.ac.ceb.como.molhub.bean.MoleculeProperty;
import uk.ac.ceb.como.molhub.bean.RotationalConstant;
import uk.ac.ceb.como.molhub.model.FolderManager;
import uk.ac.ceb.como.molhub.model.PropertiesManager;
import uk.ac.ceb.como.molhub.model.QueryManager;

// TODO: Auto-generated Javadoc
/**
 * The Class MoleculeViewAction. 
 * Shows all molecules properties stored in RDF4J triple store. 
 */
public class MoleculeViewAction extends ActionSupport {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 62530332669165064L;

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(MoleculeViewAction.class.getName());

	/** The folder manager. */
	FolderManager folderManager = new FolderManager();
	
	/** The rotational symmetry number. */
	String rotationalSymmetryNumber;

	/** The spin multiplicity value. */
	String spinMultiplicityValue;
	
	/** The formal charge. */
	List<FormalCharge> formalChargeList;
	
	/** 
	 * Reads folders path from properties, where generated ontologies are defined, as well as folder where Gaussian, XML and png files are stored. 
	 */
	Properties molhubPropreties = PropertiesManager.loadProperties(UploadAction.class.getClassLoader().getResourceAsStream("molhub.management.properties"));
	
	private String dataFolderPath = molhubPropreties.getProperty("data.folder.path").toString();

	private String kbFolderPath = molhubPropreties.getProperty("kb.folder.path").toString();
	
	/**
	 * The uuid is used as unique identifier for query all properties of a digital
	 * entity and showing results on new page.
	 */
	private String uuid = ServletActionContext.getRequest().getParameter("uuidName");

	/** The gaussian file name. */
	private String gaussianFileName = folderManager.getFileName(getUuid(), kbFolderPath, dataFolderPath, ".g09");

	/** The xml file name. */
	private String xmlFileName = folderManager.getFileName(getUuid(), kbFolderPath,dataFolderPath, ".xml");

	/** The owl file name. */
	private String owlFileName = folderManager.getFileName(getUuid(), kbFolderPath,dataFolderPath, ".owl");

	/** The nasa file name. */
	private String nasaFileName = folderManager.getFileName(getUuid(), kbFolderPath,dataFolderPath, "_nasa.json");

	/** The molecule property list. */
	List<MoleculeProperty> moleculePropertyList = new ArrayList<MoleculeProperty>();

	/** The frequency list. */
	List<Frequency> frequencyList = new ArrayList<Frequency>();

	/** The atomic mass list. */
	List<AtomicMass> atomicMassList = new ArrayList<AtomicMass>();

	/** The rotational constant list. */
	List<RotationalConstant> rotationalConstantList = new ArrayList<RotationalConstant>();
	
	/**
	 * The scf electronic energy list.
	 */
	List<ElectronicEnergy> scfElectronicEnergyList = new ArrayList<ElectronicEnergy>();

	/**
	 * The zero-point electronic energy list. 
	 */
	List<ElectronicEnergy> zeroPointElectronicEnergyList = new ArrayList<ElectronicEnergy>();
	
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.opensymphony.xwork2.ActionSupport#execute()
	 * 
	 */
	@Override
	public String execute() {
		
		/**
		 * @author nk510 <p>SPARQL returns a list of frequencies for given uuid.</p>
		 */
//		The line below is commented because the method getAllFrequencies(getUuid() is also commented and not used in new version of the code. If you want to run it, uncomment first the method QueryManager.getAllFrequencies(getUuid()
//		frequencyList = QueryManager.getAllFrequencies(getUuid());

		/**
		 * @author nk510 <p>SPARQL returns an object that remembers uud, molecule name,
		 *         basis set value, level of theory, and geometry type value.</p>
		 */

//		moleculePropertyList = QueryManager.getAllNonCompositetMoleculeProperties(getUuid());

		/**
		 * @author nk510 <p>SPARQL returns String that remembers rotational symmetry number
		 *         for given uuid.</p>
		 */

//		rotationalSymmetryNumber = QueryManager.getAllRotationalSymmertyNumber(getUuid());

		/**
		 * @author nk510 <p>SPARQL returns String that remembers spin multiplicity value
		 *         for given uuid.</p>
		 */

//		spinMultiplicityValue = QueryManager.getAllSpinMultiplicity(getUuid());

		/**
		 * @author nk510 <p>Remembers atomic masses for each atom appearing in a molecule (species) based on uuid. It
		 *         includes atomic mass value, atomic mass unit, and atom name.</p>
		 */

//		atomicMassList = QueryManager.getAllAtomicMass(getUuid());
		
		/**
		 * @author nk510 
		 * <p> Remembers rotational constant in the following form (rotational constant size, rotational
	     *         constant value, rotational constant unit).</p> 
		 */

//		rotationalConstantList = QueryManager.getAllRotationalConstant(getUuid());

		/**
		 * @author nk510 <p>SPARQL returns List<FormalCharge> that remembers formal charge value and formal charge unit
		 *         for given uuid.</p>
		 */	

//		formalChargeList = QueryManager.getAllFormalCharge(getUuid());
		
		
//		scfElectronicEnergyList = QueryManager.getElectronicEnergy(getUuid(),"ScfEnergy");
		
//		zeroPointElectronicEnergyList = QueryManager.getElectronicEnergy(getUuid(),"ZeroPointEnergy");
		
		return SUCCESS;
	}

	/**
	 * Gets the uuid.
	 *
	 * @return the uuid
	 */
	public String getUuid() {
		return uuid;
	}

	/**
	 * Sets the uuid.
	 *
	 * @param uuid
	 *            the new uuid
	 */
	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	/**
	 * Gets the gaussian file name.
	 *
	 * @return the gaussian file name
	 */
	public String getGaussianFileName() {
		return gaussianFileName;
	}

	/**
	 * Sets the gaussian file name.
	 *
	 * @param gaussianFileName
	 *            the new gaussian file name
	 */
	public void setGaussianFileName(String gaussianFileName) {
		this.gaussianFileName = gaussianFileName;
	}

	/**
	 * Gets the xml file name.
	 *
	 * @return the xml file name
	 */
	public String getXmlFileName() {
		return xmlFileName;
	}

	/**
	 * Sets the xml file name.
	 *
	 * @param xmlFileName
	 *            the new xml file name
	 */
	public void setXmlFileName(String xmlFileName) {
		this.xmlFileName = xmlFileName;
	}

	/**
	 * Gets the owl file name.
	 *
	 * @return the owl file name
	 */
	public String getOwlFileName() {
		return owlFileName;
	}

	/**
	 * Sets the owl file name.
	 *
	 * @param owlFileName
	 *            the new owl file name
	 */
	public void setOwlFileName(String owlFileName) {
		this.owlFileName = owlFileName;
	}

	/**
	 * Gets the nasa file name.
	 *
	 * @return the nasa file name
	 */
	public String getNasaFileName() {
		return nasaFileName;
	}

	/**
	 * Sets the nasa file name.
	 *
	 * @param nasaFileName
	 *            the new nasa file name
	 */
	public void setNasaFileName(String nasaFileName) {
		this.nasaFileName = nasaFileName;
	}

	/**
	 * Gets the frequency list.
	 *
	 * @return the frequency list
	 */
	public List<Frequency> getFrequencyList() {
		return frequencyList;
	}

	/**
	 * Sets the frequency list.
	 *
	 * @param frequencyList
	 *            the new frequency list
	 */
	public void setFrequencyList(List<Frequency> frequencyList) {
		this.frequencyList = frequencyList;
	}

	/**
	 * Gets the molecule property list.
	 *
	 * @return the molecule property list
	 */
	public List<MoleculeProperty> getMoleculePropertyList() {
		return moleculePropertyList;
	}

	/**
	 * Sets the molecule property list.
	 *
	 * @param moleculePropertyList
	 *            the new molecule property list
	 */
	public void setMoleculePropertyList(List<MoleculeProperty> moleculePropertyList) {
		this.moleculePropertyList = moleculePropertyList;
	}
	
	/**
	 * Gets the rotational symmetry number.
	 *
	 * @return the rotational symmetry number
	 */
	public String getRotationalSymmetryNumber() {
		return rotationalSymmetryNumber;
	}

	/**
	 * Sets the rotational symmetry number.
	 *
	 * @param rotationalSymmetryNumber
	 *            the new rotational symmetry number
	 */
	public void setRotationalSymmetryNumber(String rotationalSymmetryNumber) {
		this.rotationalSymmetryNumber = rotationalSymmetryNumber;
	}

	/**
	 * Gets the spin multiplicity value.
	 *
	 * @return the spin multiplicity value
	 */
	public String getSpinMultiplicityValue() {
		return spinMultiplicityValue;
	}

	/**
	 * Sets the spin multiplicity value.
	 *
	 * @param spinMultiplicityValue
	 *            the new spin multiplicity value
	 */
	public void setSpinMultiplicityValue(String spinMultiplicityValue) {
		this.spinMultiplicityValue = spinMultiplicityValue;
	}

	/**
	 * Gets the atomic mass list.
	 *
	 * @return the atomic mass list
	 */
	public List<AtomicMass> getAtomicMassList() {
		return atomicMassList;
	}

	/**
	 * Sets the atomic mass list.
	 *
	 * @param atomicMassList
	 *            the new atomic mass list
	 */
	public void setAtomicMassList(List<AtomicMass> atomicMassList) {
		this.atomicMassList = atomicMassList;
	}

	/**
	 * Gets the rotational constant list.
	 *
	 * @return the rotational constant list
	 */
	public List<RotationalConstant> getRotationalConstantList() {
		return rotationalConstantList;
	}

	/**
	 * Sets the rotational constant list.
	 *
	 * @param rotationalConstantList
	 *            the new rotational constant list
	 */
	public void setRotationalConstantList(List<RotationalConstant> rotationalConstantList) {
		this.rotationalConstantList = rotationalConstantList;
	}

	public List<FormalCharge> getFormalChargeList() {
		return formalChargeList;
	}

	public void setFormalChargeList(List<FormalCharge> formalChargeList) {
		this.formalChargeList = formalChargeList;
	}

	public List<ElectronicEnergy> getScfElectronicEnergyList() {
		return scfElectronicEnergyList;
	}

	public void setScfElectronicEnergyList(List<ElectronicEnergy> scfElectronicEnergyList) {
		this.scfElectronicEnergyList = scfElectronicEnergyList;
	}

	public List<ElectronicEnergy> getZeroPointElectronicEnergyList() {
		return zeroPointElectronicEnergyList;
	}

	public void setZeroPointElectronicEnergyList(List<ElectronicEnergy> zeroPointElectronicEnergyList) {
		this.zeroPointElectronicEnergyList = zeroPointElectronicEnergyList;
	}

	
	
}