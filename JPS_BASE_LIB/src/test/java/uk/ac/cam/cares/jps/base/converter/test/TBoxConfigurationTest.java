package uk.ac.cam.cares.jps.base.converter.test;

import uk.ac.cam.cares.jps.base.converter.TBoxConfiguration;
import org.junit.Test;
import static org.junit.Assert.*;

/*
* Testing for getter and setter in TBoxConfiguration
* @Author: ShinZert
 */


public class TBoxConfigurationTest {
    TBoxConfiguration test = new TBoxConfiguration();

    @Test
    public void gettBoxNameTest(){
        //Default value
        assertNull(test.gettBoxName());

        //Set specific value and check whether it is set correctly
        test.settBoxName("user");

        assertEquals("user",test.gettBoxName());
    }

    //* Repeat for the rest of the SettersNGetters

    @Test
    public void gettBoxIriTest(){
        assertNull(test.gettBoxIri());
        test.settBoxIri("user");
        assertEquals("user",test.gettBoxIri());
    }

    @Test
    public void gettBoxVersionTest(){
        assertNull(test.gettBoxVersion());
        test.settBoxVersion("user");
        assertEquals("user",test.gettBoxVersion());
    }

    @Test
    public void gettBoxCommentTest(){
        assertNull(test.gettBoxComment());
        test.settBoxComment("user");
        assertEquals("user",test.gettBoxComment());
    }

    @Test
    public void getGitCommitHashValueTest(){
        assertNull(test.getGitCommitHashValue());
        test.setGitCommitHashValue("user");
        assertEquals("user",test.getGitCommitHashValue());
    }

    @Test
    public void gettBoxCreationDateTest(){
        assertNull(test.gettBoxCreationDate());
        test.settBoxCreationDate("user");
        assertEquals("user",test.gettBoxCreationDate());
    }

    @Test
    public void gettBoxImportTest(){
        assertNull(test.gettBoxImport());
        test.settBoxImport("user");
        assertEquals("user",test.gettBoxImport());
    }

    @Test
    public void getCompChemGitCommitHashTest(){
        assertNull(test.getCompChemGitCommitHash());
        test.setCompChemGitCommitHash("user");
        assertEquals("user",test.getCompChemGitCommitHash());
    }

    @Test
    public void getIsARelationTest(){
        assertNull(test.getIsARelation());
        test.setIsARelation("user");
        assertEquals("user",test.getIsARelation());
    }

    @Test
    public void getEquivalentToRelationTest(){
        assertNull(test.getEquivalentToRelation());
        test.setEquivalentToRelation("user");
        assertEquals("user",test.getEquivalentToRelation());
    }

    @Test
    public void getInverseOfRelationTest(){
        assertNull(test.getInverseOfRelation());
        test.setInverseOfRelation("user");
        assertEquals("user",test.getInverseOfRelation());
    }

    @Test
    public void getAnnotationPropertyDateTest(){
        assertNull(test.getAnnotationPropertyDate());
        test.setAnnotationPropertyDate("user");
        assertEquals("user",test.getAnnotationPropertyDate());
    }

}
