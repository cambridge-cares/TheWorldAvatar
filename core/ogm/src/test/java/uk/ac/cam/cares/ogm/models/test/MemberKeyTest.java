package uk.ac.cam.cares.ogm.models.test;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.MemberKey;
import uk.ac.cam.cares.ogm.models.Model;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.*;

public class MemberKeyTest {

    @Test
    public void testNewMemberKey(){

        assertEquals(2, MemberKey.class.getDeclaredFields().length);
        assertEquals(2, MemberKey.class.getDeclaredMethods().length);
    }

    @Test
    public void testhashCode(){

        assertEquals(Objects.hash(TestModel.class, "http://testiri.com/test"), new MemberKey(TestModel.class, "http://testiri.com/test").hashCode());
        //same class and same iri
        assertEquals(new MemberKey(TestModel.class, "http://testiri.com/test").hashCode(), new MemberKey(TestModel.class, "http://testiri.com/test").hashCode());
        //same class and different iri
        assertNotEquals(new MemberKey(TestModel.class, "http://testiri.com/test").hashCode(), new MemberKey(TestModel.class, "http://testiri.com/test1").hashCode());
        //different class and same iri
        assertNotEquals(new MemberKey(Model.class, "http://testiri.com/test").hashCode(), new MemberKey(TestModel.class, "http://testiri.com/test").hashCode());
    }

    @Test
    public void testequals(){

        //same class and same iri
        assertTrue(new MemberKey(TestModel.class, "http://testiri.com/test").equals(new MemberKey(TestModel.class, "http://testiri.com/test")));
        //same class and different iri
        assertFalse(new MemberKey(TestModel.class, "http://testiri.com/test").equals(new MemberKey(TestModel.class, "http://testiri.com/test1")));
        //different class and same iri
        assertFalse(new MemberKey(Model.class, "http://testiri.com/test").equals(new MemberKey(TestModel.class, "http://testiri.com/test")));
    }
}
