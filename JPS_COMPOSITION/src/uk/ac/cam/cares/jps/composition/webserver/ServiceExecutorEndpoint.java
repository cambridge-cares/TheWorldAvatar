package uk.ac.cam.cares.jps.composition.webserver;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.composition.enginemodel.Graph;
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
			
			System.out.println("=================== Received ================");
			String graphInString = JSONReader.readJSONFromRequest(request).toString();
			System.out.println(graphInString);
			System.out.println("=============================================");
			Graph graph = FormatTranslator.convertGraphJSONTOJavaClass(graphInString);
			
			
			ExecutorProcessor processor = new ExecutorProcessor(FormatTranslator.convertGraphJavaClassTOJSON(graph));
			ArrayList<ExecutionLayer> executionChain = processor.generateExecutionChain();
			ExecutorNew executor = new ExecutorNew(executionChain);
			System.out.println("=================== Execution Chain ==================");
			System.out.println(FormatTranslator.convertExectorToJSON(executor).toString());
			System.out.println("======================================================");
			
			response.getWriter().write(FormatTranslator.convertExectorToJSON(executor).toString());

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
