package com.cmclinnovations.jps.agent.file_management.mods;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.cmclinnovations.jps.agent.file_management.mods.algorithms.AlgorithmS;
import com.cmclinnovations.jps.agent.file_management.mods.cases.CaseS;
import com.cmclinnovations.jps.agent.file_management.mods.files.FileS;
import com.cmclinnovations.jps.agent.file_management.mods.models.ModelS;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.ParameterS;

@XmlRootElement(name = "mods")
@XmlType(propOrder = { "algorithmS", "modelS", "caseS", "fileS", "parameterS" })
@XmlAccessorType(XmlAccessType.FIELD)
public class MoDS {
	@XmlAttribute(name = "xmlns")
	private String xmlns;

	public String getXmlns() {
		return xmlns;
	}

	public void setXmlns(String xmlns) {
		this.xmlns = xmlns;
	}

	@XmlAttribute(name = "xmlns:xsi")
	private String xmlnsXsi;

	public String getXmlnsXsi() {
		return xmlnsXsi;
	}

	public void setXmlnsXsi(String xmlnsXsi) {
		this.xmlnsXsi = xmlnsXsi;
	}

	@XmlAttribute(name = "xsi:schemaLocation")
	private String xsiSchemaLocation;

	public String getXsiSchemaLocation() {
		return xsiSchemaLocation;
	}

	public void setXsiSchemaLocation(String xsiSchemaLocation) {
		this.xsiSchemaLocation = xsiSchemaLocation;
	}

	@XmlElement(name = "algorithms")
	private AlgorithmS algorithmS;

	public AlgorithmS getAlgorithmS() {
		return algorithmS;
	}

	public void setAlgorithmS(AlgorithmS algorithmS) {
		this.algorithmS = algorithmS;
	}

	@XmlElement(name = "models")
	private ModelS modelS;

	public ModelS getModelS() {
		return modelS;
	}

	public void setModelS(ModelS modelS) {
		this.modelS = modelS;
	}

	@XmlElement(name = "cases")
	private CaseS caseS;

	public CaseS getCaseS() {
		return caseS;
	}

	public void setCaseS(CaseS caseS) {
		this.caseS = caseS;
	}

	@XmlElement(name = "files")
	private FileS fileS;

	public FileS getFileS() {
		return fileS;
	}

	public void setFileS(FileS fileS) {
		this.fileS = fileS;
	}

	@XmlElement(name = "parameters")
	private ParameterS parameterS;

	public ParameterS getParameterS() {
		return parameterS;
	}

	public void setParameterS(ParameterS parameterS) {
		this.parameterS = parameterS;
	}
}
