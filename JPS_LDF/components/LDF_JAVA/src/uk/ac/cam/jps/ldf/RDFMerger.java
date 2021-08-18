package uk.ac.cam.jps.ldf;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;

import org.apache.jena.riot.Lang;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;

public class RDFMerger {

	public static void main(String[] args) throws FileNotFoundException, IOException {
		
		System.out.println("Working Directory = " + System.getProperty("user.dir"));

		// To merge RDF files into one single file (n3/ttl) so that the files can be later converted to HDT files
		
		
			// read the files in the folder, select the owl files 
		
			File folder = new File("D:\\ontocompchem");
			File[] listOfFiles = folder.listFiles();
			ArrayList<String> filenames = new ArrayList<String>();
			
			// ======================= filter out all the owl files ============================
			for (File file : listOfFiles) {
			    if (file.isFile()) {
			    	if (file.getName().endsWith(".owl")) {
			    		System.out.println(file.getName());
			    		filenames.add(file.getName()); // make a list of all filenames
			    	}
			    }
			}
			
			int counter = 1; // use the counter to merge every 10 files 
			
			int block_counter = 0; // use the counter as the name for the block 
			
			System.out.println("============================== Start merging ==============================");
			Model blockModel = ModelFactory.createDefaultModel();
			for (String filename : filenames) {				
				// ========================= read the owl file ==============================
				InputStream inputstream = new FileInputStream("D:\\ontocompchem\\" + filename);
			 	final Model rdffile = ModelFactory.createDefaultModel();
				rdffile.read(inputstream, null, null) ;				
				// ==========================================================================

				System.out.println("Processing " + filename);
				blockModel = blockModel.add(rdffile); // put the new file into the graph ... 
				counter ++ ;
				if ((counter == filenames.size() + 1) || (counter% 100 == 0)) {
					block_counter++;
					System.out.println("The last file of this block " + filename); // the last file of a block, make the merge and serialize the file ...  
					FileOutputStream blockfile = new FileOutputStream("D:\\ontocompchem\\merged_"+ block_counter +".ttl"); // write the new block file ... 
					blockModel.write(blockfile, "Turtle");
					
					blockModel = ModelFactory.createDefaultModel(); // empty the block model 

				}
				}

 
		
		
		
	}

}
