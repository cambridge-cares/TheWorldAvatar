package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import java.io.File;
/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * This code is borrowed from the web.
 *
 */
public class FileComparison {

	public static void main(String[] args) {
		
		//Provide full path for directory(change accordingly)
        //String maindirpath = "C:\\Users\\Gaurav Miglani\\Desktop\\Test";
		
        //String maindirpath = "C:\\Users\\NK\\git\\thermochemistry\\CoMoEnthalpyEstimationPaper\\test_data\\test_results\\ti_isg\\04391b0e-b1bc-3cc3-be1c-733b18219e36";
        String maindirpath = "C:\\Users\\NK\\git\\thermochemistry\\CoMoEnthalpyEstimationPaper\\test_data\\test_results\\ti_isg\\valid-test-results";
                  
        //File object
        File maindir = new File(maindirpath);
        
        if(maindir.exists() && maindir.isDirectory()){ 
        	
            // array for files and sub-directories
            // of directory pointed by maindir
            File arr[] = maindir.listFiles();
              
            System.out.println("**********************************************");
            System.out.println("Files from main directory : " + maindir);
            System.out.println("**********************************************");
              
            // Calling recursive method
            RecursivePrint(arr,0,0);
       
       }
    }
	
	static void RecursivePrint(File[] arr,int index,int level) {
		
        // terminate condition
        if(index == arr.length) {return;}
        
        // tabs for internal levels
        for (int i = 0; i < level; i++)System.out.print("\t");
       
        // for files
        if(arr[index].isFile()){
        
        System.out.println(arr[index].getName());
        
        // for sub-directories
        }else if(arr[index].isDirectory()){
        	
            System.out.println("[" + arr[index].getName() + "]");
              
            // recursion for sub-directories
            RecursivePrint(arr[index].listFiles(), 0, level + 1);
        }
        
        // recursion for main directory
        RecursivePrint(arr,++index, level);
   }
}