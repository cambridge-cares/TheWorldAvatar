package main.java.uk.ac.cam.cares.jps.agent.opf.utils;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

import com.opencsv.CSVReader;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

public class IriMapper {
	
	private List<IriMapping> list = new ArrayList<IriMapping>();

	public class IriMapping {
		public String iri;
		public String id;
		public String type;
	}
	
	public void add(String iri, String id, String type) {
		IriMapping newinst = new IriMapping();
		newinst.iri = iri;
		newinst.id = id;
		newinst.type = type;
		list.add(newinst);	
	}
	
	public String getIri(String id, String type) {
		int size = list.size();
		String resultiri = null;
		for (int r = 0; r < size; r++) {
			if(id.contentEquals(list.get(r).id) && type.contentEquals(list.get(r).type)) {
				resultiri = list.get(r).iri;
			}
		}
		return resultiri;
	}

	public String getIDFromMap(List<IriMapping> list, String iri) {
		String mappedori = null;
		int an = 0;
		while(an < list.size()) {
			if(list.get(an).iri.contentEquals(iri)) {
				mappedori = list.get(an).id;
			}
			an++;	
		}
		return mappedori;
	}	
	
	/**
	 * Create csv without header, column 1= iri; column 2= id; column3=type
	 * @return
	 */
	public String serialize() {
		ArrayList<String[]> stringArray = new ArrayList<String[]>();
		
		int size = list.size();
		for(int a = 0; a < size; a++) {
			String[]line = new String[3];
			line[0] = list.get(a).iri;
			line[1] = list.get(a).id;
			line[2] = list.get(a).type;	
			stringArray.add(line);
		}
		return MatrixConverter.fromArraytoCsv(stringArray);
	}
	
	public List<IriMapping> deserialize2(String filePath) throws IOException {
		if (!new File(filePath).isFile()) {
			throw new JPSRuntimeException("File not exist at " + filePath + " .\n");
		}
	    Reader readpath = new FileReader(filePath);
	    CSVReader csvReader = new CSVReader(readpath);
	    List<String[]> listofcontent = new ArrayList<>();
	    listofcontent = csvReader.readAll();
	    readpath.close();
	    csvReader.close();
	    int size = listofcontent.size();
	    for(int b = 0; b < size; b++) {
	    	IriMapping a = new IriMapping ();
	    	a.iri = listofcontent.get(b)[0];
	    	a.id = listofcontent.get(b)[1];
	    	a.type = listofcontent.get(b)[2];
	    	list.add(a);
	    }
	    return list;
		
	}
	
}
