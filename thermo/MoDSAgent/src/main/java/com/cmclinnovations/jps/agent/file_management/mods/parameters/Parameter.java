package com.cmclinnovations.jps.agent.file_management.mods.parameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.cmclinnovations.jps.agent.file_management.mods.cases.CaseS;
import com.cmclinnovations.jps.agent.file_management.mods.files.FileS;
import com.cmclinnovations.jps.agent.file_management.mods.models.ModelS;

@XmlType(propOrder = { "scaling", "paramterCaseS", "parameterModelS", "parameterFileS" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Parameter {
	@XmlAttribute
	private String type;

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	@XmlAttribute
	private String subtype;

	public String getSubtype() {
		return subtype;
	}

	public void setSubtype(String subtype) {
		this.subtype = subtype;
	}
	
	@XmlAttribute(name="case_detail_sep")
	private String caseDetailSep;

	public String getCaseDetailSep() {
		return caseDetailSep;
	}

	public void setCaseDetailSep(String caseDetailSep) {
		this.caseDetailSep = caseDetailSep;
	}
	
	@XmlAttribute(name="detail_sep")
	private String detailSep;

	public String getDetailSep() {
		return detailSep;
	}

	public void setDetailSep(String detailSep) {
		this.detailSep = detailSep;
	}
	
	@XmlAttribute(name="n_params_per_case")
	private String nParamsPerCase;

	public String getNParamsPerCase() {
		return nParamsPerCase;
	}

	public void setNParamsPerCase(String nParamsPerCase) {
		this.nParamsPerCase = nParamsPerCase;
	}

	@XmlAttribute
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	@XmlAttribute(name="name_suffix_detail")
	private String nameSuffixDetail;

	public String getNameSufficDetail() {
		return nameSuffixDetail;
	}

	public void setNameSufficDetail(String nameSuffixDetail) {
		this.nameSuffixDetail = nameSuffixDetail;
	}

	@XmlAttribute(name="perspective_white_space")
	private String perspectiveWhiteSpace;

	public String getPerspectiveWhiteSpace() {
		return perspectiveWhiteSpace;
	}

	public void setPerspectiveWhiteSpace(String perspectiveWhiteSpace) {
		this.perspectiveWhiteSpace = perspectiveWhiteSpace;
	}

	@XmlElement(name = "scaling")
	private String scaling;

	public String getScaling() {
		return scaling;
	}

	public void setScaling(String scaling) {
		this.scaling = scaling;
	}

	@XmlElement(name = "cases")
	private CaseS parameterCaseS;

	public CaseS getCases() {
		return parameterCaseS;
	}

	public void setCases(CaseS parameterCaseS) {
		this.parameterCaseS = parameterCaseS;
	}

	@XmlElement(name = "models")
	private ModelS parameterModelS;

	public ModelS getModels() {
		return parameterModelS;
	}

	public void setModels(ModelS parameterModelS) {
		this.parameterModelS = parameterModelS;
	}

	@XmlElement(name = "files")
	private FileS parameterFileS;

	public FileS getFiles() {
		return parameterFileS;
	}

	public void setFiles(FileS parameterFileS) {
		this.parameterFileS = parameterFileS;
	}
}
