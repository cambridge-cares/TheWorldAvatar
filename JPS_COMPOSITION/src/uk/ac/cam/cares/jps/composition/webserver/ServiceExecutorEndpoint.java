package uk.ac.cam.cares.jps.composition.webserver;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.composition.executor.ExecutionLayer;
import uk.ac.cam.cares.jps.composition.executor.ExecutorNew;
import uk.ac.cam.cares.jps.composition.executor.ExecutorProcessor;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;
import uk.ac.cam.cares.jps.composition.util.JSONReader;

@WebServlet("/ServiceExecutorEndpoint")
public class ServiceExecutorEndpoint extends HttpServlet {
	private static final long serialVersionUID = 1L;

	public ServiceExecutorEndpoint() {
		super();
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			ExecutorProcessor executorprocessor = new ExecutorProcessor(JSONReader.readJSONFromRequest(request));
			ArrayList<ExecutionLayer> executionChain = executorprocessor.generateExecutionChain();
			ExecutorNew executor = new ExecutorNew(executionChain);
			response.getWriter().write(FormatTranslator.convertExectorToJSON(executor).toString());

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
