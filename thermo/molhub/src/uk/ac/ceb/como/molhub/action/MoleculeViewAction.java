package uk.ac.ceb.como.molhub.action;

import org.apache.struts2.ServletActionContext;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.ceb.como.molhub.model.FolderManager;

import org.apache.log4j.Logger;

public class MoleculeViewAction extends ActionSupport {
	
	private static final long serialVersionUID = 62530332669165064L;
	
	final static Logger logger = Logger.getLogger(MoleculeViewAction.class.getName());
	
	FolderManager folderManager = new FolderManager();
	
	private String catalinaFolderPath = System.getProperty("catalina.home");
	
	private String uuid =  ServletActionContext.getRequest().getParameter("uuidName");
	
	private String gaussianFileName=folderManager.getFileName(getUuid(), catalinaFolderPath,".g09");
	
	private String xmlFileName=folderManager.getFileName(getUuid(), catalinaFolderPath,".xml");
	
	private String owlFileName=folderManager.getFileName(getUuid(), catalinaFolderPath,".owl");
	
	private String nasaFileName=folderManager.getFileName(getUuid(), catalinaFolderPath,"nasa.json");

	public String execute() {	
				
		logger.info("getUuid() : " + getUuid() + ", getGaussianFileName(): " + getGaussianFileName());
		
		
		
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

	
}