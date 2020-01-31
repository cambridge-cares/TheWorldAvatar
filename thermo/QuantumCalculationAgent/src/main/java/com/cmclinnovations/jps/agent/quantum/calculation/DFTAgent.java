package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashSet;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;
import org.springframework.stereotype.Controller;

import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.SftpException;

import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.StreamGobbler;
import ch.ethz.ssh2.Session;

/**
 * Quantum Calculation Agent developed for setting-up and running quantum
 * jobs at increasing levels of theory.   
 * 
 * @author msff2
 *
 */
@Controller
public class DFTAgent extends HttpServlet{
	private Logger logger = LoggerFactory.getLogger(DFTAgent.class);	
	String server = "login-skylake.hpc.cam.ac.uk";
	String username = "msff2";
	String password = "Abcdl955_l7_l7_l7_aB";
	String command = "ls";
	Connection connection;
	Session session;
	boolean isAuthenticated;
	
	static HashSet<String> jobPool = new HashSet<>();
	
	/**
	 * Initialises property values.
	 * 
	 * @throws QuantumCalculationException
	 */
	public void init() throws ServletException{
        logger.info("----------");
        logger.info("---------- Quantum Calculation Agent has started ----------");
        logger.info("----------");
        	runQuantumJobs();
        	System.out.println("The jobs are over.");
	}
	
	private String runQuantumJobs(){
		try {
			setupJob();
			String slurmScriptName = "G09Slurm_darwin_S1.sh";
			String inputFileName = "water_ex.com";
			uploadFile("C:/Users/msff2/Documents/HPC/KnowledgeCapturedFromAngiras/".concat(inputFileName),
					"rds/hpc-work/gaussian");
			uploadFile("C:/Users/msff2/Documents/HPC/KnowledgeCapturedFromAngiras/".concat(slurmScriptName),
					"rds/hpc-work/gaussian");
			runQuantumJob(slurmScriptName, inputFileName);
		} catch (JSchException e) {
			logger.error(e.getMessage());
		} catch (SftpException e) {
			logger.error(e.getMessage());
		} catch (InterruptedException e) {
			logger.error(e.getMessage());
		} catch (IOException e) {
			logger.error(e.getMessage());
		}
		return null;
	}
	
	/**
	 * The method decides the time interval between the current and next</br>
	 * status check operations.   
	 * 
	 * @param count
	 * @throws InterruptedException
	 */
	private void waitBeforeStatusCheck(int count) throws InterruptedException{
		if(count <= 3){
			Thread.sleep(10000);
		}else if(count <= 6){
			Thread.sleep(20000);
		}else{
			Thread.sleep(60000*2);
		}
	}
	
	private boolean isJobRunning(String jobId) throws IOException, InterruptedException{
		System.out.println("Closed the connection.");
		String command = "squeue -j " + jobId + "--start";
		System.out.println("Running the following command:"+command);
		ArrayList<String> outputs = executeCommand(command);
		System.out.println("Command executed.");
		boolean jobRunning = isJobRunning(outputs);
		System.out.println("Job running status:"+jobRunning);
		return jobRunning;
	}
	
	private boolean isJobRunning(ArrayList<String> outputs){
		if(outputs!=null && outputs.size()==1){
			return false;
		}
		return true;
	}
	
	private String getJobId(ArrayList<String> outputs){
		for(String output: outputs){
			if(output.contains("Submitted batch job")){
				String tokens[] = output.split(" ");
				if(tokens.length>=4){
					return tokens[3].trim();
				}
			}
		}
		return null;
	}
	
	private void setupJob(){
		
	}
	
	/**
	 * Runs a quantum job.
	 * 
	 * @param command
	 * @throws IOException
	 */
	private String runQuantumJob(String scriptName, String inputFileName) throws IOException, InterruptedException{
		String command = "cd rds/hpc-work/gaussian && sbatch ".concat(scriptName);
		ArrayList<String> outputs = executeCommand(command);
		if (outputs == null) {
			return null;
		}
		String jobId = getJobId(outputs);
		System.out.println("Job id:" + jobId);
		jobPool.add(jobId);
		boolean isJobRunning = isJobRunning(jobId);
		int count = 0;
		System.out.println("Is the job running?" + isJobRunning);
		while (isJobRunning) {
			count++;
			waitBeforeStatusCheck(count);
			isJobRunning = isJobRunning(jobId);
		}
		try {
			downloadFile("rds/hpc-work/gaussian/".concat(inputFileName.replace(".com", ".log")),
					"C:/Users/msff2/Documents/HPC/KnowledgeCapturedFromAngiras");
		} catch (JSchException e) {
			logger.error(e.getMessage());
		} catch (SftpException e) {
			logger.error(e.getMessage());
		}
		return jobId;
	}
	
	/**
	 * Connects the client to the server via server IP address or name. 
	 * 
	 * @param server IP address or name
	 * @throws IOException
	 */
	public void connect(String server) throws IOException{
        if(connection == null){
        	connection = new Connection(server);
            connection.connect();
    		authenticate(username, getPassword(password));
    		openSession();
        }
	}
	
	/**
	 * Authenticate the user with the password.
	 * 
	 * @param username the name of user
	 * @param password the password of user
	 * @throws IOException
	 */
	public void authenticate(String username, String password) throws IOException{
        if(connection == null)
        	throw new IOException("Not connected to the server.");
        if (!connection.isAuthenticationComplete())
        	isAuthenticated = connection.authenticateWithPassword(username, password);
        if (isAuthenticated == false)
            throw new IOException("Authentication failed.");        		
	}
	
	/**
	 * Open a session in order to allow to execute commands.
	 * 
	 * @throws IOException
	 */
	public void openSession() throws IOException{
		if(connection == null)
        	throw new IOException("Not connected to the server.");
		if (!connection.isAuthenticationComplete())
			throw new IOException("Authentication is not complete.");
		session = connection.openSession();
	}
	
	/**
	 * Executes a command within a session.
	 * 
	 * @param command
	 * @throws IOException
	 */
	public void executeCommand1(String command) throws IOException{
		if(session == null)
			throw new IOException("No session is open.");
		session.execCommand(command);
	}
	
	/**
	 * Create the reader to read the outputs of the most recently executed</br>
	 * command.
	 * 
	 * @return
	 * @throws IOException
	 */
	public BufferedReader getReader() throws IOException{
        if(session == null)
        	throw new IOException("No session is open.");
        return new BufferedReader(new InputStreamReader(new StreamGobbler(session.getStdout())));
	}
	
	/**
	 * Read the output of the most recently executed command.
	 * 
	 * @param br
	 * @return
	 * @throws IOException
	 */
	public ArrayList<String> readCommandOutput(BufferedReader br) throws IOException{
		if(br == null)
			throw new IOException("The reader is not initialised.");
		String line;
		ArrayList<String> outputs = new ArrayList<>();
		while((line=br.readLine())!=null){
			outputs.add(line);
		}
		return outputs;
	}
	
	/**
	 * Close the currently open session and connection. 
	 * 
	 * @throws IOException
	 */
	public void closeSessionAndConnection() throws IOException{
        if(session == null)
        	throw new IOException("No session is open.");
		session.close();
        if(connection == null)
        	throw new IOException("No connection is open.");
		connection.close();
		System.out.println("Connection closed test:"+(session==null)+" session is null:"+session.getState());
	}
	
	/**
	 * Display every item in a list of string array.
	 * 
	 * @param list
	 */
	public void displayArray(ArrayList<String> list){
		for(String item:list){
			System.out.println(item);
		}
	}
	
	public void uploadFile(String src, String dest) throws JSchException, SftpException{
		JSch jsch = new JSch();
		com.jcraft.jsch.Session session = jsch.getSession(username, server);
		String pwd = getPassword(password);
		session.setPassword(pwd);
        session.setConfig("StrictHostKeyChecking", "no");
        System.out.println("Establishing Connection to transfer "+src+" to "+dest);
        session.connect();
		ChannelSftp sftpChannel = (ChannelSftp) session.openChannel("sftp");
		sftpChannel.connect();

		sftpChannel.put(src, dest);
		sftpChannel.disconnect();
		System.out.println("Closing the connection.");
	}

	public void downloadFile(String src, String dest) throws JSchException, SftpException{
		JSch jsch = new JSch();
		com.jcraft.jsch.Session session = jsch.getSession(username, server);
		String pwd = getPassword(password);
		session.setPassword(pwd);
        session.setConfig("StrictHostKeyChecking", "no");
        System.out.println("Establishing Connection to transfer "+src+" to "+dest);
        session.connect();
		ChannelSftp sftpChannel = (ChannelSftp) session.openChannel("sftp");
		sftpChannel.connect();

		sftpChannel.get(src, dest);
		sftpChannel.disconnect();
		System.out.println("Closing the connection.");
	}


	public ArrayList<String> executeCommand(String Command){
		ArrayList<String> outputs = null;
		try{JSch jsch = new JSch();
		com.jcraft.jsch.Session session = jsch.getSession(username, server);
		String pwd = getPassword(password);
		session.setPassword(pwd);
        session.setConfig("StrictHostKeyChecking", "no");
        System.out.println("Establishing a connection to perform the following command:"+Command);
        session.connect();
        Channel channel=session.openChannel("exec");
        ((ChannelExec)channel).setCommand(Command);
        channel.setInputStream(null);
        ((ChannelExec)channel).setErrStream(System.err);
        
//        InputStream in=channel.getInputStream();
        /////////////////////////
        BufferedReader stdInput = new BufferedReader(new
				InputStreamReader(channel.getInputStream()));		
        ////////////////////////////
        channel.connect();
		outputs = readCommandOutput(stdInput);
//        byte[] tmp=new byte[1024];
//        while(true){
//          while(in.available()>0){
//        	  int i=in.read(tmp, 0, 1024);
//            if(i<0)break;
//            System.out.print(new String(tmp, 0, i));
//          }
//          if(channel.isClosed()){
//            System.out.println("exit-status: "+channel.getExitStatus());
//            break;
//          }
//          try{Thread.sleep(1000);}catch(Exception ee){}
//        }
        channel.disconnect();
        session.disconnect();
        System.out.println("DONE");
    }catch(JSchException e){
    	e.printStackTrace();
	}catch(Exception e){
    	e.printStackTrace();
    }
		return outputs;
	}

	
	public void SSHClient(String serverIp,String command, String username,String password) throws IOException{
        System.out.println("inside the ssh function");
        try
        {
            Connection conn = new Connection(serverIp);
            conn.connect();
            boolean isAuthenticated = conn.authenticateWithPassword(username, password);
            if (isAuthenticated == false)
                throw new IOException("Authentication failed.");        
            ch.ethz.ssh2.Session sess = conn.openSession();
            sess.execCommand(command);  
            InputStream stdout = new StreamGobbler(sess.getStdout());
            BufferedReader br = new BufferedReader(new InputStreamReader(stdout));
            System.out.println("the output of the command is");
            while (true)
            {
                String line = br.readLine();
                if (line == null)
                    break;
                System.out.println(line);
            }
            System.out.println("ExitCode: " + sess.getExitStatus());
            sess.close();
            conn.close();
        }
        catch (IOException e)
        {
            e.printStackTrace(System.err);

        }
    }
	
	private String getPassword(String password){
		return password.replace("l", "1").replace("_", "").replace("7", "3").replace("3", "4");
	}
}
