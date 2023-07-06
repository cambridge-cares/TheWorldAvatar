package test.java.uk.ac.cam.cares.jps.agent.opf;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import main.java.uk.ac.cam.cares.jps.agent.opf.utils.IriMapper;
import main.java.uk.ac.cam.cares.jps.agent.opf.utils.IriMapper.IriMapping;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

public class IriMapperTest {
	
	@Test
	public void testAdd() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		String iri = "iri_number";
		String id = "id_number";
		String type =  "type_number";
			
		IriMapper newinst = new IriMapper();
		newinst.add(iri, id, type);
		assertNotNull(newinst.getClass().getDeclaredField("list"));
		Field field = newinst.getClass().getDeclaredField("list");
		field.setAccessible(true);
		List<IriMapper.IriMapping> list = (List<IriMapping>) field.get(newinst);
		
		assertEquals(list.get(0).iri,iri);
		assertEquals(list.get(0).id,id);
		assertEquals(list.get(0).type,type);	
	}
		
	@Test 
	public void testGetIri() {
		String iri1 = "iri_number_1";
		String id1 = "id_number_1";
		String type1 =  "type_number_1";
		String iri2 = "iri_number_2";
		String id2 = "id_number_2";
		String type2 =  "type_number_2";
		
		IriMapper newinst = new IriMapper();	
		String test_id = "test_string";
		String test_type = "test_type";
		String resultiri1 = newinst.getIri(test_id, test_type);
		assertNull(resultiri1);
		
		newinst.add(iri1, id1, type1);
		newinst.add(iri2, id2, type2);
	
		String resultiri2 = newinst.getIri(test_id,test_id);
		String resultiri3 = newinst.getIri(test_id,type1);
		String resultiri4 = newinst.getIri(test_id,type2);
		String resultiri5 = newinst.getIri(id1, test_type);
		String resultiri6 = newinst.getIri(id2, test_type);
		String resultiri7 = newinst.getIri(id1, type1);
	    String resultiri8 = newinst.getIri(id2, type2);
	    
		assertNull(resultiri2);
		assertNull(resultiri3);
		assertNull(resultiri4);
		assertNull(resultiri5);
		assertNull(resultiri6);
		assertSame(resultiri7,iri1);
		assertSame(resultiri8,iri2); 
	}
	
	@Test
	public void testGetIDFromMap() {
		String iri1 = "iri_number_1";
		String id1 = "id_number_1";
		String type1 =  "type_number_1";
		String iri2 = "iri_number_2";
		String id2 = "id_number_2";
		String type2 =  "type_number_2";
		
		List<IriMapping> listinput = new ArrayList<IriMapping>();
		IriMapper newinst = new IriMapper();
		
		IriMapper.IriMapping newinst_irimapping1 = newinst.new IriMapping();
		newinst_irimapping1.iri = iri1;
		newinst_irimapping1.id = id1;
		newinst_irimapping1.type = type1;
		listinput.add(newinst_irimapping1);	
		
		IriMapper.IriMapping newinst_irimapping2 = newinst.new IriMapping();
		newinst_irimapping2.iri = iri2;
		newinst_irimapping2.id = id2;
		newinst_irimapping2.type = type2;
		listinput.add(newinst_irimapping2);	
		
		String test_iri = "test_iri";
		String mappedori1 = newinst.getIDFromMap(listinput, test_iri);
		assertNull(mappedori1);
		
		String mappedori2 = newinst.getIDFromMap(listinput, iri1);
		assertSame(mappedori2,id1);
		
		String mappedori3 = newinst.getIDFromMap(listinput, iri2);
		assertSame(mappedori3,id2);
	}

	@Test
	public void testSerialize() {
		String iri = "iri_number";
		String id = "id_number";
		String type =  "type_number";
		IriMapper newinst = new IriMapper();
		newinst.add(iri, id, type);
		String csv = String.join(",", new String[]{iri,id,type});
		String serialize = newinst.serialize().trim();
		assertEquals(csv,serialize);
	}
	
	@Rule
    public TemporaryFolder tempFolder = new TemporaryFolder();
	
	@Test 
	public void testDeserialize2() throws IOException {
		String iri = "iri_number";
		String id = "id_number";
		String type =  "type_number";
	
		IriMapper newinst1 = new IriMapper();
		newinst1.add(iri,id,type);
		
        File testFile = tempFolder.newFile("test.txt");
        FileWriter writer = new FileWriter(testFile);
        writer.write("iri_number,id_number,type_number");
        writer.close();

		String filePath = tempFolder.getRoot().getAbsolutePath() + "/test.txt";
		List<IriMapping> list = newinst1.deserialize2(filePath);
		
		List<IriMapping> list_test = new ArrayList<IriMapping>();
		IriMapper newinst2 = new IriMapper();
		IriMapper.IriMapping newinst_irimapping = newinst2.new IriMapping();
		newinst_irimapping.iri = iri;
		newinst_irimapping.id = id;
		newinst_irimapping.type = type;
		list_test.add(newinst_irimapping);
		
		assertEquals(list.get(0).iri,list_test.get(0).iri);
		assertEquals(list.get(0).id,list_test.get(0).id);
		assertEquals(list.get(0).type,list_test.get(0).type);
	}

    @Test(expected = JPSRuntimeException.class)
	public void testDeserialize2FileNotFound() throws IOException {
		IriMapper mapper = new IriMapper();
		mapper.deserialize2(tempFolder.getRoot().getAbsolutePath());
	}
}
