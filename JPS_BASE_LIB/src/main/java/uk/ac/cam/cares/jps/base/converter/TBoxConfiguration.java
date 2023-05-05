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
		
	@Value("${tbox.element.type.class}")
	private String elementTypeClass;

	@Value("${tbox.element.type.object.property}")
	private String elementTypeObjectProperty;

	@Value("${tbox.index.of.source.column}")
	private int indexOfSourceColumn;

	@Value("${tbox.index.of.type.column}")
	private int indexOfTypeColumn;

	@Value("${tbox.index.of.target.column}")
	private int indexOfTargetColumn;
	
	@Value("${tbox.index.of.relation.column}")
	private int indexOfRelationColumn;

	@Value("${tbox.index.of.domain.column}")
	private int indexOfDomainColumn;

	@Value("${tbox.index.of.range.column}")
	private int indexOfRangeColumn;

	@Value("${tbox.index.of.quantifier.column}")
	private int indexOfQuantifierColumn;

	@Value("${tbox.index.of.comment.column}")
	private int indexOfCommentColumn;

	@Value("${tbox.index.of.defined.by.column}")
	private int indexOfDefinedByColumn;

	@Value("${tbox.index.of.label.column}")
	private int indexOfLabelColumn;
	
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
	
	public String getElementTypeClass() {
		return elementTypeClass;
	}

	public void setElementTypeClass(String elementTypeClass) {
		this.elementTypeClass = elementTypeClass;
	}

	public String getElementTypeObjectProperty() {
		return elementTypeObjectProperty;
	}

	public void setElementTypeObjectProperty(String elementTypeObjectProperty) {
		this.elementTypeObjectProperty = elementTypeObjectProperty;
	}

	public int getIndexOfSourceColumn() {
		return indexOfSourceColumn;
	}

	public void setIndexOfSourceColumn(int indexOfSourceColumn) {
		this.indexOfSourceColumn = indexOfSourceColumn;
	}

	public int getIndexOfTypeColumn() {
		return indexOfTypeColumn;
	}

	public void setIndexOfTypeColumn(int indexOfTypeColumn) {
		this.indexOfTypeColumn = indexOfTypeColumn;
	}
	
	public int getIndexOfTargetColumn() {
		return indexOfTargetColumn;
	}

	public void setIndexOfTargetColumn(int indexOfTargetColumn) {
		this.indexOfTargetColumn = indexOfTargetColumn;
	}

	public int getIndexOfRelationColumn() {
		return indexOfRelationColumn;
	}

	public void setIndexOfRelationColumn(int indexOfRelationColumn) {
		this.indexOfRelationColumn = indexOfRelationColumn;
	}

	public int getIndexOfDomainColumn() {
		return indexOfDomainColumn;
	}

	public void setIndexOfDomainColumn(int indexOfDomainColumn) {
		this.indexOfDomainColumn = indexOfDomainColumn;
	}

	public int getIndexOfRangeColumn() {
		return indexOfRangeColumn;
	}

	public void setIndexOfRangeColumn(int indexOfRangeColumn) {
		this.indexOfRangeColumn = indexOfRangeColumn;
	}

	public int getIndexOfQuantifierColumn() {
		return indexOfQuantifierColumn;
	}

	public void setIndexOfQuantifierColumn(int indexOfQuantifierColumn) {
		this.indexOfQuantifierColumn = indexOfQuantifierColumn;
	}

	public int getIndexOfCommentColumn() {
		return indexOfCommentColumn;
	}

	public void setIndexOfCommentColumn(int indexOfCommentColumn) {
		this.indexOfCommentColumn = indexOfCommentColumn;
	}

	public int getIndexOfDefinedByColumn() {
		return indexOfDefinedByColumn;
	}

	public void setIndexOfDefinedByColumn(int indexOfDefinedByColumn) {
		this.indexOfDefinedByColumn = indexOfDefinedByColumn;
	}

	public int getIndexOfLabelColumn() {
		return indexOfLabelColumn;
	}

	public void setIndexOfLabelColumn(int indexOfLabelColumn) {
		this.indexOfLabelColumn = indexOfLabelColumn;
	}

	public String getAnnotationPropertyDate() {
		return annotationPropertyDate;
	}

	public void setAnnotationPropertyDate(String annotationPropertyDate) {
		this.annotationPropertyDate = annotationPropertyDate;
	}

}
