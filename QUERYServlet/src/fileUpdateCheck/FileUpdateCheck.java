package fileUpdateCheck;

import java.io.File;
import java.io.FileNotFoundException;

public class FileUpdateCheck {
	/*	
	public static void main(String[] s) throws FileNotFoundException
    {
		System.out.println("test!");
        CheckFile1 check = new CheckFile1("D:\\JPS_Code_Test\\GIS.CSV");
        check.start();
        System.out.println("test1!");
    }
	*/
	
}
/* 
class CheckFile1 extends Thread {
    private File file = null;
    private long lastModifieTime = 0L;
    public CheckFile1(String fileName) throws FileNotFoundException {
        file = new File(fileName);
        if (!file.exists()) {
            throw new FileNotFoundException();
        }
        lastModifieTime = file.lastModified();
        System.out.println(lastModifieTime);
    }
     
    @Override
    public void run() {
        long newTime = 0L;
      
        while(true) {
            newTime = file.lastModified();
          
            if (newTime != lastModifieTime) {
                lastModifieTime = newTime;
                System.out.println("File has been modified.:(");
            }
             
       
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

}
*/