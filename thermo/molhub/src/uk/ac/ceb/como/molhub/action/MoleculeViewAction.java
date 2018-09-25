package uk.ac.ceb.como.molhub.action;

import java.util.List;
import java.util.ArrayList;

import org.apache.struts2.ServletActionContext;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.ceb.como.molhub.bean.AtomicMass;
import uk.ac.ceb.como.molhub.bean.Frequency;
import uk.ac.ceb.como.molhub.bean.MoleculeProperty;
import uk.ac.ceb.como.molhub.bean.RotationalConstant;
import uk.ac.ceb.como.molhub.model.FolderManager;
import uk.ac.ceb.como.molhub.model.QueryManager;

import org.apache.log4j.Logger;

public class MoleculeViewAction extends ActionSupport {
	
	private static final long serialVersionUID = 62530332669165064L;
	
	final static Logger logger = Logger.getLogger(MoleculeViewAction.class.getName());
	
	FolderManager folderManager = new FolderManager();
	
	String rotationalSymmetryNumber;
	
	String spinMultiplicityValue;
	
	private String catalinaFolderPath = System.getProperty("catalina.home");
	
	private String uuid =  ServletActionContext.getRequest().getParameter("uuidName");
	
	private String gaussianFileName=folderManager.getFileName(getUuid(), catalinaFolderPath,".g09");
	
	private String xmlFileName=folderManager.getFileName(getUuid(), catalinaFolderPath,".xml");
	
	private String owlFileName=folderManager.getFileName(getUuid(), catalinaFolderPath,".owl");
	
	private String nasaFileName=folderManager.getFileName(getUuid(), catalinaFolderPath,"nasa.json");

	List<MoleculeProperty> moleculePropertyList = new ArrayList<MoleculeProperty>();
	
	List<Frequency> frequencyList = new ArrayList<Frequency>();
	
	List<AtomicMass> atomicMassList = new ArrayList<AtomicMass>();
	
	List<RotationalConstant> rotationalConstantList = new ArrayList<RotationalConstant>();
	
	public String execute() {
		
		/**
		 * @author nk510
		 * Returns a list of frequencies for each digital entity.
		 */
		frequencyList = QueryManager.getAllFrequencies(getUuid());
		
		moleculePropertyList = QueryManager.getAllNonCompositetMoleculeProperties(getUuid());
		
		rotationalSymmetryNumber = QueryManager.getAllRotationalSymmertyNumber(getUuid());
		
		spinMultiplicityValue = QueryManager.getAllSpinMultiplicity(getUuid());
		
		atomicMassList=QueryManager.getAllAtomicMass(getUuid());
		
		rotationalConstantList=QueryManager.getAllRotationalConstant(getUuid());
		
		return SUCCESS;
	}	

	public String getUuid() {
		return uuid;
	}
	
	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	public String getGaussianFileName() {
		return gaussianFileName;
	}

	public void setGaussianFileName(String gaussianFileName) {
		this.gaussianFileName = gaussianFileName;
	}

	public String getXmlFileName() {
		return xmlFileName;
	}

	public void setXmlFileName(String xmlFileName) {
		this.xmlFileName = xmlFileName;
	}

	public String getOwlFileName() {
		return owlFileName;
	}

	public void setOwlFileName(String owlFileName) {
		this.owlFileName = owlFileName;
	}

	public String getNasaFileName() {
		return nasaFileName;
	}

	public void setNasaFileName(String nasaFileName) {
		this.nasaFileName = nasaFileName;
	}
	
	public List<Frequency> getFrequencyList() {
		return frequencyList;
	}

	public void setFrequencyList(List<Frequency> frequencyList) {
		this.frequencyList = frequencyList;
	}

	public List<MoleculeProperty> getMoleculePropertyList() {
		return moleculePropertyList;
	}

	public void setMoleculePropertyList(List<MoleculeProperty> moleculePropertyList) {
		this.moleculePropertyList = moleculePropertyList;
	}

	public String getRotationalSymmetryNumber() {
		return rotationalSymmetryNumber;
	}

	public void setRotationalSymmetryNumber(String rotationalSymmetryNumber) {
		this.rotationalSymmetryNumber = rotationalSymmetryNumber;
	}

	public String getSpinMultiplicityValue() {
		return spinMultiplicityValue;
	}

	public void setSpinMultiplicityValue(String spinMultiplicityValue) {
		this.spinMultiplicityValue = spinMultiplicityValue;
	}

	public List<AtomicMass> getAtomicMassList() {
		return atomicMassList;
	}

	public void setAtomicMassList(List<AtomicMass> atomicMassList) {
		this.atomicMassList = atomicMassList;
	}

	public List<RotationalConstant> getRotationalConstantList() {
		return rotationalConstantList;
	}

	public void setRotationalConstantList(List<RotationalConstant> rotationalConstantList) {
		this.rotationalConstantList = rotationalConstantList;
	}
}