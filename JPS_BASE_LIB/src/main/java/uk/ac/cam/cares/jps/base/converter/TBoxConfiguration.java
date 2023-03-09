package uk.ac.cam.cares.jps.base.converter;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all TBox properties provided in the jps.properties file.
 * 
 * @author msff2
 *
 */
@Configuration
@PropertySource("classpath:jps.properties")
public class TBoxConfiguration {
	
	private String tBoxName;
	
	private String tBoxIri;
	
	private String tBoxVersion;	
	
	private String tBoxComment;

	@Value("${tbox.compchem.git.commit.hash}")
	private String compChemGitCommitHash;
	
	@Value("${tbox.git.commit.hash.value}")
	private String gitCommitHashValue;
	
	@Value("${tbox.date.of.publication}")
	private String tBoxCreationDate;
	
	private String tBoxImport;
	
	@Value("${tbox.is-a.relation}")
	private String isARelation;

	@Value("${tbox.equivalent-to.relation}")
	private String equivalentToRelation;
	
	@Value("${tbox.inverse-of.relation}")
	private String inverseOfRelation;
	
	@Value("${tbox.data.of.publication}")
	private String annotationPropertyDate;
	
	public String gettBoxName() {
		return tBoxName;
	}

	public void settBoxName(String tBoxName) {
		this.tBoxName = tBoxName;
	}

	public String gettBoxIri() {
		return tBoxIri;
	}

	public void settBoxIri(String tBoxIri) {
		this.tBoxIri = tBoxIri;
	}

	public String gettBoxVersion() {
		return tBoxVersion;
	}

	public void settBoxVersion(String tBoxVersion) {
		this.tBoxVersion = tBoxVersion;
	}

	public String gettBoxComment() {
		return tBoxComment;
	}

	public void settBoxComment(String tBoxComment) {
		this.tBoxComment = tBoxComment;
	}

	public String getGitCommitHashValue() {
		return gitCommitHashValue;
	}

	public void setGitCommitHashValue(String gitCommitHashValue) {
		this.gitCommitHashValue = gitCommitHashValue;
	}

	public String gettBoxCreationDate() {
		return tBoxCreationDate;
	}

	public void settBoxCreationDate(String tBoxCreationDate) {
		this.tBoxCreationDate = tBoxCreationDate;
	}

	public String gettBoxImport() {
		return tBoxImport;
	}

	public void settBoxImport(String tBoxImport) {
		this.tBoxImport = tBoxImport;
	}
	
	public String getCompChemGitCommitHash() {
		return compChemGitCommitHash;
	}

	public void setCompChemGitCommitHash(String compChemGitCommitHash) {
		this.compChemGitCommitHash = compChemGitCommitHash;
	}

	public String getIsARelation() {
		return isARelation;
	}

	public void setIsARelation(String isARelation) {
		this.isARelation = isARelation;
	}

	public String getEquivalentToRelation() {
		return equivalentToRelation;
	}

	public void setEquivalentToRelation(String equivalentToRelation) {
		this.equivalentToRelation = equivalentToRelation;
	}
	
	public String getInverseOfRelation() {
		return inverseOfRelation;
	}

	public void setInverseOfRelation(String inverseOfRelation) {
		this.inverseOfRelation = inverseOfRelation;
	}

	public String getAnnotationPropertyDate() {
		return annotationPropertyDate;
	}

	public void setAnnotationPropertyDate(String annotationPropertyDate) {
		this.annotationPropertyDate = annotationPropertyDate;
	}

}
