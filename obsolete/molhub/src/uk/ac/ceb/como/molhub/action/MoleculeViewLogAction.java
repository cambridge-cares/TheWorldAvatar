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
 * Shows in a browser all molecules properties stored in RDF4J triple store. 
 * 
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 */
public class MoleculeViewLogAction extends ActionSupport {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 62530332669165064L;

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(MoleculeViewAction.class.getName());

	/** The folder manager. It implements methods for getting OWL, JSON, NASA polynomial  file names. */
	FolderManager folderManager = new FolderManager();
	
	/** The rotational symmetry number. */
	String rotationalSymmetryNumber;

	/** The spin multiplicity value. */
	String spinMultiplicityValue;
	
	/** The formal charge. */
	List<FormalCharge> formalChargeList;
	
	/** 
	 * Reads folders path from properties, where generated ontologies are defined, as well as folder where Gaussian, JSON and PNG files are stored. 
	 */
	Properties molhubPropreties = PropertiesManager.loadProperties(UploadAction.class.getClassLoader().getResourceAsStream("molhub.management.properties"));
	
	private String dataFolderPath = molhubPropreties.getProperty("data.folder.path").toString();

	private String kbFolderPath = molhubPropreties.getProperty("kb.folder.path").toString();
	
	/**
	 * The UUID and uuidFile are used as unique identifiers for query all properties of a digital
	 * entity and showing results on new page. They are also used to create URLs to Log, OWL and JSON files. 
	 */
	private String uuid = ServletActionContext.getRequest().getParameter("uuidName");	
	
	private String uuidFile = ServletActionContext.getRequest().getParameter("uuidFileName");

	/** The Gaussian file name. */	
//	private String gaussianFileName = folderManager.getLogFileName(getUuid(), getUuidFile());
	
	/** The Gaussian file name. */
	private String gaussianFileName = folderManager.getGaussianFileName(getUuid(),getUuidFile(),dataFolderPath);
	
	/** The JSON file name*/
	private String gaussianJsonFileName = folderManager.getGaussianJsonFileName(getUuidFile());
	
	/** The OWL file name. */
	private String owlFileName = folderManager.getOwlFileName(getUuid(),getUuidFile(),kbFolderPath,".owl");

	/** The NASA JSON file name. */
	private String nasaFileName = folderManager.getJsonNasaFileName(getUuid(), getUuidFile(),dataFolderPath, "q_nasa.json");

	/** The molecule property list. */
	List<MoleculeProperty> moleculePropertyList = new ArrayList<MoleculeProperty>();

	/** The frequency list. */
	List<Frequency> frequencyList = new ArrayList<Frequency>();

	/** The atomic mass list. */
	List<AtomicMass> atomicMassList = new ArrayList<AtomicMass>();

	/** The rotational constant list. */
	List<RotationalConstant> rotationalConstantList = new ArrayList<RotationalConstant>();
	
	/**
	 * The SCF electronic energy list.
	 */
	List<ElectronicEnergy> scfElectronicEnergyList = new ArrayList<ElectronicEnergy>();

	/**
	 * The zero-point electronic energy list. 
	 */
	List<ElectronicEnergy> zeroPointElectronicEnergyList = new ArrayList<ElectronicEnergy>();
	/**
	 * The electronic and zero point energy list. 
	 */
	List<ElectronicEnergy> electronicAndZeroPointEnergyList = new ArrayList<ElectronicEnergy>();
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.opensymphony.xwork2.ActionSupport#execute()
	 * 
	 */
	@Override
	public String execute() {
		
		/**
		 * @author nk510 <p>SPARQL returns a list of frequencies for given UUID and uuidFile.</p>
		 */
		frequencyList = QueryManager.getAllFrequencies(getUuid(),getUuidFile());

		/**
		 * @author nk510 <p>SPARQL returns an object that remembers UUID, molecule name,
		 *         basis set value, level of theory, and geometry type value.</p>
		 */

		moleculePropertyList = QueryManager.getAllNonCompositetMoleculeProperties(getUuid(),getUuidFile());

		/**
		 * @author nk510 <p>SPARQL returns String that remembers rotational symmetry number
		 *         for given UUID and uuidFile.</p>
		 */

		rotationalSymmetryNumber = QueryManager.getAllRotationalSymmertyNumber(getUuid(),getUuidFile());

		/**
		 * @author nk510 <p>SPARQL returns String that remembers spin multiplicity value
		 *         for given UUID and uuidFile.</p>
		 */

		spinMultiplicityValue = QueryManager.getAllSpinMultiplicity(getUuid(),getUuidFile());

		/**
		 * @author nk510 <p>Remembers atomic masses for each atom appearing in a molecule (species) based on UUID and uuidFile. It
		 *         includes atomic mass value, atomic mass unit, and atom name.</p>
		 */

		atomicMassList = QueryManager.getAllAtomicMass(getUuid(),getUuidFile());
		
		/**
		 * @author nk510 
		 * <p> Remembers rotational constant in the following form (rotational constant size, rotational
	     *         constant value, rotational constant unit).</p> 
		 */

		rotationalConstantList = QueryManager.getAllRotationalConstant(getUuid(),getUuidFile());

		/**
		 * @author nk510 <p>SPARQL returns List<FormalCharge> that remembers formal charge value and formal charge unit
		 *         for given UUID and uuidFile.</p>
		 */	

		formalChargeList = QueryManager.getAllFormalCharge(getUuid(),getUuidFile());
		
		/**
		 * @author nk510 <p>SPARQL returns List<ElectronicEnergy> that remembers zero point electronic energy value and  unit
		 *         for given UUID and given uuidFile.</p>
		 */	
		
		zeroPointElectronicEnergyList = QueryManager.getElectronicEnergy(getUuid(),getUuidFile(),"ZeroPointEnergy");
		
		/**
		 * @author nk510 <p>SPARQL returns List<ElectronicEnergy> that remembers electronic and zero point energy value and  unit
		 *         for given UUID and given uuidFile.</p>
		 */	
		
		electronicAndZeroPointEnergyList = QueryManager.getElectronicEnergy(getUuid(),getUuidFile(),"ElectronicAndZPEEnergy");
		
		/**
		 * @author nk510 <p>SPARQL returns List<ElectronicEnergy> that remembers scf electronic energy value and  unit
		 *         for given UUID and given uuidFile.</p>
		 */	
		
		scfElectronicEnergyList = QueryManager.getElectronicEnergy(getUuid(),getUuidFile(),"ScfEnergy");
		
		return SUCCESS;
	}

	/**
	 * @return the uuidFile
	 */
	public String getUuidFile() {
		return uuidFile;
	}

	/**
	 * @param uuidFile unique name for generated owl file.
	 */
	public void setUuidFile(String uuidFile) {
		this.uuidFile = uuidFile;
	}
	
	/**
	 * Gets the UUID.
	 *
	 * @return the UUID
	 */
	public String getUuid() {
		return uuid;
	}	

	/**
	 * Sets the UUID.
	 *
	 * @param uuid
	 *            the new UUID
	 */
	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	/**
	 * Gets the Gaussian file name.
	 *
	 * @return the Gaussian file name
	 */
	public String getGaussianFileName() {
		return gaussianFileName;
	}

	/**
	 * Sets the Gaussian file name.
	 *
	 * @param gaussianFileName
	 *            the new Gaussian file name
	 */
	public void setGaussianFileName(String gaussianFileName) {
		this.gaussianFileName = gaussianFileName;
	}	
	/**
	 * Gets the OWL file name.
	 *
	 * @return the OWL file name
	 */
	public String getOwlFileName() {
		return owlFileName;
	}

	/**
	 * Sets the OWL file name.
	 *
	 * @param owlFileName
	 *            the new OWL file name
	 */
	public void setOwlFileName(String owlFileName) {
		this.owlFileName = owlFileName;
	}

	/**
	 * Gets the NASA file name.
	 *
	 * @return the NASA file name
	 */
	public String getNasaFileName() {
		return nasaFileName;
	}

	/**
	 * Sets the NASA file name.
	 *
	 * @param nasaFileName
	 *            the new NASA file name
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

	/**
	 * @return list of formal charge data
	 */
	public List<FormalCharge> getFormalChargeList() {
		return formalChargeList;
	}

	/**
	 * @param formalChargeList list of formal charge data.
	 */
	public void setFormalChargeList(List<FormalCharge> formalChargeList) {
		this.formalChargeList = formalChargeList;
	}

	/**
	 * @return SCF electronic energy
	 */
	public List<ElectronicEnergy> getScfElectronicEnergyList() {
		return scfElectronicEnergyList;
	}

	/**
	 * @param scfElectronicEnergyList the list of SCF electronic energy data
	 */
	public void setScfElectronicEnergyList(List<ElectronicEnergy> scfElectronicEnergyList) {
		this.scfElectronicEnergyList = scfElectronicEnergyList;
	}

	/**
	 * @return zero point energy electronic energy list
	 */
	public List<ElectronicEnergy> getZeroPointElectronicEnergyList() {
		return zeroPointElectronicEnergyList;
	}

	/**
	 * @param zero point electronic energy list
	 */
	public void setZeroPointElectronicEnergyList(List<ElectronicEnergy> zeroPointElectronicEnergyList) {
		this.zeroPointElectronicEnergyList = zeroPointElectronicEnergyList;
	}

	/**
	 * @return Gaussian JSON file name
	 */
	public String getGaussianJsonFileName() {
		return gaussianJsonFileName;
	}

	/**
	 * @param gaussianJsonFileName
	 */
	public void setGaussianJsonFileName(String gaussianJsonFileName) {
		this.gaussianJsonFileName = gaussianJsonFileName;
	}

	/**
	 * @return electronic and zero point energy list
	 */
	public List<ElectronicEnergy> getElectronicAndZeroPointEnergyList() {
		return electronicAndZeroPointEnergyList;
	}

	/**
	 * @param electronic and zero point energy list
	 */
	public void setElectronicAndZeroPointEnergyList(List<ElectronicEnergy> electronicAndZeroPointEnergyList) {
		this.electronicAndZeroPointEnergyList = electronicAndZeroPointEnergyList;
	}
	
}