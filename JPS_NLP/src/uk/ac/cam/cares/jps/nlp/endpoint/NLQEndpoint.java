package uk.ac.cam.cares.jps.nlp.endpoint;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;


@WebServlet("/NLQEndpoint")
public class NLQEndpoint extends HttpServlet {
	private static final long serialVersionUID = 1L;

    public NLQEndpoint() {
        super();
    }

	
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		String sentence = request.getParameter("value");
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsnlq", this);
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("EndPoint.py");
		args.add(sentence);
		String result = CommandHelper.executeCommands(targetFolder, args);
		response.getWriter().write(result.split("###")[1]);
	}


	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}

}

