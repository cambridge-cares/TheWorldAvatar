package uk.ac.cam.cares.jps.nlq;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.config.AgentLocator;


@WebServlet("/NLQEndpoint")
public class NLQEndpoint extends HttpServlet {
	private static final long serialVersionUID = 1L;

    public NLQEndpoint() {
        super();
    }

	
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsnlq", this);
		String fullPath = AgentLocator.getPathToWorkingDir(this) + "/" + "ADMS";
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsTest.py"); 

	}


	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}

}
