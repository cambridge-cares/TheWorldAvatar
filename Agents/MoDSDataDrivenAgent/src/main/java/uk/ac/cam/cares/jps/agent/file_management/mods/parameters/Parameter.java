package uk.ac.cam.cares.jps.agent.file_management.mods.parameters;

import java.util.LinkedHashMap;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import uk.ac.cam.cares.jps.agent.file_management.mods.cases.CaseS;
import uk.ac.cam.cares.jps.agent.file_management.mods.cases.SimplifiedCaseS;
import uk.ac.cam.cares.jps.agent.file_management.mods.files.FileS;
import uk.ac.cam.cares.jps.agent.file_management.mods.models.ModelS;
import uk.ac.cam.cares.jps.agent.file_management.mods.models.SimplifiedModelS;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder = { "scaling", "parameterCaseS", "parameterModelS", "parameterFileS" })
public class Parameter {
	@JsonProperty("type")
	@XmlAttribute
	private String type;

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}
	
	@JsonProperty("subtype")
	@XmlAttribute
	private String subtype;

	public String getSubtype() {
		return subtype;
	}

	public void setSubtype(String subtype) {
		this.subtype = subtype;
	}
	
	@JsonProperty("caseDetailSep")
	@XmlAttribute(name="case_detail_sep")
	private String caseDetailSep;

	public String getCaseDetailSep() {
		return caseDetailSep;
	}

	public void setCaseDetailSep(String caseDetailSep) {
		this.caseDetailSep = caseDetailSep;
	}
	
	@JsonProperty("detailSep")
	@XmlAttribute(name="detail_sep")
	private String detailSep;

	public String getDetailSep() {
		return detailSep;
	}

	public void setDetailSep(String detailSep) {
		this.detailSep = detailSep;
	}
	
	@JsonProperty("nParamsPerCase")
	@XmlAttribute(name="n_params_per_case")
	private String nParamsPerCase;

	public String getNParamsPerCase() {
		return nParamsPerCase;
	}

	public void setNParamsPerCase(String nParamsPerCase) {
		this.nParamsPerCase = nParamsPerCase;
	}
	
	@JsonProperty("name")
	@XmlAttribute
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	@JsonProperty("nameSuffixDetail")
	@XmlAttribute(name="name_suffix_detail")
	private String nameSuffixDetail;

	public String getNameSufficDetail() {
		return nameSuffixDetail;
	}

	public void setNameSufficDetail(String nameSuffixDetail) {
		this.nameSuffixDetail = nameSuffixDetail;
	}
	
	@JsonProperty("preserveWhiteSpace")
	@XmlAttribute(name="preserve_white_space")
	private String preserveWhiteSpace;

	public String getPreserveWhiteSpace() {
		return preserveWhiteSpace;
	}

	public void setPreserveWhiteSpace(String preserveWhiteSpace) {
		this.preserveWhiteSpace = preserveWhiteSpace;
	}
	
	@JsonProperty("scaling")
	@XmlElement(name = "scaling")
	private String scaling;

	public String getScaling() {
		return scaling;
	}

	public void setScaling(String scaling) {
		this.scaling = scaling;
	}
	
	@JsonProperty("cases")
	@XmlElement(name = "cases")
	private SimplifiedCaseS parameterCaseS;

	public SimplifiedCaseS getCases() {
		return parameterCaseS;
	}

	public void setCases(SimplifiedCaseS parameterCaseS) {
		this.parameterCaseS = parameterCaseS;
	}
	
	@JsonProperty("models")
	@XmlElement(name = "models")
	private SimplifiedModelS parameterModelS;

	public SimplifiedModelS getModels() {
		return parameterModelS;
	}

	public void setModels(SimplifiedModelS parameterModelS) {
		this.parameterModelS = parameterModelS;
	}
	
	@JsonProperty("files")
	@XmlElement(name = "files")
	private FileS parameterFileS;

	public FileS getFiles() {
		return parameterFileS;
	}

	public void setFiles(FileS parameterFileS) {
		this.parameterFileS = parameterFileS;
	}
	
	
	@JsonIgnore
	@XmlTransient
	private String activeParameterNo;
	
	@JsonIgnore
	@XmlTransient
	private String activeParameter;
	
	@JsonIgnore
	@XmlTransient
	private String passiveParameter;
	
	
	
	

	@JsonIgnore
	@XmlTransient
	private List<String> caseNamesList;
	
	@JsonIgnore
	@XmlTransient
	private List<String> modelList;
	
	@JsonIgnore
	@XmlTransient
	private LinkedHashMap<String, LinkedHashMap<String, String>> fileHash;

	public String getActiveParameterNo() {
		return activeParameterNo;
	}

	public void setActiveParameterNo(String activeParameterNo) {
		this.activeParameterNo = activeParameterNo;
	}

	public String getActiveParameter() {
		return activeParameter;
	}

	public void setActiveParameter(String activeParameter) {
		this.activeParameter = activeParameter;
	}

	public List<String> getCaseNamesList() {
		return caseNamesList;
	}
	
	public String getPassiveParameter() {
		return passiveParameter;
	}

	public void setPassiveParameter(String passiveParameter) {
		this.passiveParameter = passiveParameter;
	}

	public void setCaseNamesList(List<String> caseNamesList) {
		this.caseNamesList = caseNamesList;
	}

	public List<String> getModelList() {
		return modelList;
	}

	public void setModelList(List<String> modelList) {
		this.modelList = modelList;
	}
	
	public LinkedHashMap<String, LinkedHashMap<String, String>> getFileHash() {
		return fileHash;
	}

	public void setFileHash(LinkedHashMap<String, LinkedHashMap<String, String>> fileHash) {
		this.fileHash = fileHash;
	}
}
