import java.io.IOException;

public class remote_exec {
	
	public static void main(String[] args) {
	    Runtime rs = Runtime.getRuntime();
	    try {
			try {
				rs.exec("/Users/gourab/Matlab/call_matlab.sh").waitFor();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			System.out.printf("executed ");
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	  
	}

}
