package uk.ac.cam.cares.ebr.cross.validation.servlet;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.schmizz.sshj.SSHClient;
import net.schmizz.sshj.common.IOUtils;
import net.schmizz.sshj.connection.channel.direct.Session;
import net.schmizz.sshj.transport.verification.PromiscuousVerifier;

/**
 * 
 * Servlet implementation class CrossValidation
 * 
 */
@WebServlet("/")
public class CrossValidation extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
	/**
     * @see HttpServlet#HttpServlet()
     */
    public CrossValidation() {
       
    	super();
       
    }

	/**
	 * 
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 * 
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		SSHClient ssh = new SSHClient();
	
        try {
        	
		ssh.addHostKeyVerifier(new PromiscuousVerifier());
		
		ssh.loadKnownHosts();
		
		ssh.connect("172.25.186.150");
		
		ssh.authPassword("", "");
		
		final Session session = ssh.startSession();
		
		try {
			
			final Session.Command cmd_ssh = session.exec("ssh cn01; java -jar /home/nkrd01/cv_isg_15000_r5_v1.jar" );
			
			System.out.println(IOUtils.readFully(cmd_ssh.getInputStream()).toString());
			
			response.getWriter().append("exit status: " + cmd_ssh.getExitStatus() + "\n");
			
			response.getWriter().append("Served at: ").append(request.getContextPath() + ".");
			
			cmd_ssh.join(10, TimeUnit.SECONDS);
		
		}finally {
			
		session.close();
		
		}

        } finally {
        
        ssh.disconnect();
    	ssh.close();
    	
        }		
	}

	/**
	 * 
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 * 
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

}
