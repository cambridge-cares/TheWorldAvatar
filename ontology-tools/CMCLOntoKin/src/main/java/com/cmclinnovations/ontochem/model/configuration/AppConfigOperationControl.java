package com.cmclinnovations.ontochem.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all operation control properties provided
 * in the operation.control.properties file.</br>
 * 
 * @author msff2
 *
 */
@Configuration
@PropertySource("classpath:operation.control.properties")
public class AppConfigOperationControl {
	@Value("${read.ctml}")
	private String opReadCtml;

	@Value("${write.ctml}")
	private String opWriteCtml;

	@Value("${read.owl}")
	private String opReadOwl;

	@Value("${write.owl}")
	private String opWriteOwl;
	
	@Value("${read.compchem}")
	private String opReadCompChem;	

	@Value("${mechanism.input.file.name}")
	private String mechanismFileName;
	
	@Value("${owl.file.extension}")
	private String owlFileExtension;
	
	@Value("${ctml.file.extension}")
	private String ctmlFileExtension;

	@Value("${compchem.file.extension}")
	private String compChemFileExtension;
	
	@Value("${ctml.message.species.coverage}")
	private String ctmlMessageSpeciesCoverage;

	@Value("${ctml.message.species.order}")
	private String ctmlMessageSpeciesOrder;

	@Value("${tbox.file.name}")
	private String tBoxFileName;

	@Value("${tbox.csv.file.datatype.key.column.index}")
	private int dataTypeKeyColumnIndex;

	@Value("${tbox.csv.file.datatype.value.column.index}")
	private int dataTypeValueColumnIndex;

	@Value("${tbox.csv.file.source.type.column.index}")
	private int sourceTypeColumnIndex;

	@Value("${tbox.csv.file.source.type.data.property}")
	private String sourceTypeDataPropertyName;
	
	/**
	 * @return a value to indicate that it's a CTML read operation.
	 * @see String
	 */
	public String getOpReadCtml() {
		return opReadCtml;
	}
	
	/**
	 * @param opReadCtml a CTML read operation to set.
	 */
	public void setOpReadCtml(String opReadCtml) {
		this.opReadCtml = opReadCtml;
	}

	/**
	 * @return a value to indicate that it's a CTML write operation.
	 * @see String
	 */
	public String getOpWriteCtml() {
		return opWriteCtml;
	}

	/**
	 * @param opWriteCtml a CTML write operation to set.
	 */
	public void setOpWriteCtml(String opWriteCtml) {
		this.opWriteCtml = opWriteCtml;
	}

	/**
	 * 
	 * @return "3" to indicate that it's an OWL read operation.
	 * @see String
	 */
	public String getOpReadOwl() {
		return opReadOwl;
	}

	/**
	 * @param opReadOwl an OWL read operation to set.
	 */
	public void setOpReadOwl(String opReadOwl) {
		this.opReadOwl = opReadOwl;
	}

	/**
	 * 
	 * @return "2" to indicate that it's an OWL write operation.
	 * @see String
	 */
	public String getOpWriteOwl() {
		return opWriteOwl;
	}

	/**
	 * @param opWriteOwl an OWL write operation to set.
	 */
	public void setOpWriteOwl(String opWriteOwl) {
		this.opWriteOwl = opWriteOwl;
	}

	/** 
	 * @return the name of a mechanism file.
	 * @see String
	 */
	public String getMechanismFileName() {
		return mechanismFileName;
	}
	
	/**
	 * @param mechanismFileName the name of a mechanism file to set.  
	 */
	public void setMechanismFileName(String mechanismFileName) {
		this.mechanismFileName = mechanismFileName;
	}
	/**
	 * @return the name of the OWL file extension.
	 * @see String
	 */
	public String getOwlFileExtension() {
		return owlFileExtension;
	}

	/**
	 * @param owlFileExtension the name of the OWL file extension to set.
	 */
	public void setOwlFileExtension(String owlFileExtension) {
		this.owlFileExtension = owlFileExtension;
	}

	/**
	 * @return the name of the CTML file extension.
	 * @see String
	 */
	public String getCtmlFileExtension() {
		return ctmlFileExtension;
	}
	
	/**
	 * @param xmlFileExtension the name of the CTML file extension to set.
	 */
	public void setCtmlFileExtension(String ctmlFileExtension) {
		this.ctmlFileExtension = ctmlFileExtension;
	}

	public String getCtmlMessageSpeciesCoverage() {
		return ctmlMessageSpeciesCoverage;
	}

	public void setCtmlMessageSpeciesCoverage(String ctmlMessageSpeciesCoverage) {
		this.ctmlMessageSpeciesCoverage = ctmlMessageSpeciesCoverage;
	}

	public String getCtmlMessageSpeciesOrder() {
		return ctmlMessageSpeciesOrder;
	}

	public void setCtmlMessageSpeciesOrder(String ctmlMessageSpeciesOrder) {
		this.ctmlMessageSpeciesOrder = ctmlMessageSpeciesOrder;
	}

	public String gettBoxFileName() {
		return tBoxFileName;
	}

	public void settBoxFileName(String tBoxFileName) {
		this.tBoxFileName = tBoxFileName;
	}

	public int getDataTypeKeyColumnIndex() {
		return dataTypeKeyColumnIndex;
	}

	public void setDataTypeKeyColumnIndex(int dataTypeKeyColumnIndex) {
		this.dataTypeKeyColumnIndex = dataTypeKeyColumnIndex;
	}

	public int getDataTypeValueColumnIndex() {
		return dataTypeValueColumnIndex;
	}

	public void setDataTypeValueColumnIndex(int dataTypeValueColumnIndex) {
		this.dataTypeValueColumnIndex = dataTypeValueColumnIndex;
	}

	public int getSourceTypeColumnIndex() {
		return sourceTypeColumnIndex;
	}

	public void setSourceTypeColumnIndex(int sourceTypeColumnIndex) {
		this.sourceTypeColumnIndex = sourceTypeColumnIndex;
	}

	public String getSourceTypeDataPropertyName() {
		return sourceTypeDataPropertyName;
	}

	public void setSourceTypeDataPropertyName(String sourceTypeDataPropertyName) {
		this.sourceTypeDataPropertyName = sourceTypeDataPropertyName;
	}

	public String getOpReadCompChem() {
		return opReadCompChem;
	}

	public void setOpReadCompChem(String opReadCompChem) {
		this.opReadCompChem = opReadCompChem;
	}

	public String getCompChemFileExtension() {
		return compChemFileExtension;
	}

	public void setCompChemFileExtension(String compChemFileExtension) {
		this.compChemFileExtension = compChemFileExtension;
	}
}
