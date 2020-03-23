package uk.ac.cam.cares.ebr.repository;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.zip.*;

public class FolderManager {
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param folderPath Folder path that contains folders and files that will be zipped.
	 * @throws IOException
	 */
	public static void getZipFile(String folderPath) throws IOException {
		
		Path sourceDirectory = Paths.get(folderPath);		
		
		String zipFileName = folderPath.concat(".zip");

		try {
			
		ZipOutputStream zipOutputStream = new ZipOutputStream (new FileOutputStream(zipFileName));
		
		Files.walkFileTree(sourceDirectory, new SimpleFileVisitor<Path>() {

			@Override
			public FileVisitResult visitFile(Path arg0, BasicFileAttributes arg1) throws IOException {
				
				try {
				Path destinationFile = sourceDirectory.relativize(arg0);
				
				zipOutputStream.putNextEntry(new ZipEntry(destinationFile.toString()));
				
				byte[] bytes = Files.readAllBytes(arg0);
				
				zipOutputStream.write(bytes, 0, bytes.length);
				
				zipOutputStream.closeEntry();
				
				}catch(IOException e) {
					
					e.printStackTrace();
				}
				
				// TODO Auto-generated method stub
				//return super.visitFile(arg0, arg1);
				
				return FileVisitResult.CONTINUE;
				
			}
		});
		
		zipOutputStream.close();
		
		}catch(IOException e) {
			
			e.printStackTrace();
		}
		
	}
	
}
