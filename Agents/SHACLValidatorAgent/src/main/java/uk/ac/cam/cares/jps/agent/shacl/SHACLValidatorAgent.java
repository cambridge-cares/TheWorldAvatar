package uk.ac.cam.cares.jps.agent.shacl;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.shacl.Shapes;
import org.apache.jena.shacl.ValidationReport;
import org.apache.jena.shacl.ShaclValidator;

import static spark.Spark.*;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

public class SHACLValidatorAgent {

    public static void main(String[] args) {
        // Create an instance of the validation agent
        SHACLValidatorAgent agent = new SHACLValidatorAgent();

        agent.runSparkServer(3737);

        // Define the endpoint to handle validation
        post("/validate", (req, res) -> {
            // Check if request is multipart
            if (!ServletFileUpload.isMultipartContent(req.raw())) {
                res.status(400); // Bad Request
                return "Request must be multipart with two files: dataGraph and shaclGraph";
            }

            // Create file upload handler
            ServletFileUpload upload = new ServletFileUpload(new DiskFileItemFactory());

            // Temporary file storage
            File dataGraphFile = null;
            File shaclGraphFile = null;

            try {
                // Parse the request
                List<FileItem> items = upload.parseRequest(req.raw());

                // Iterate over form items
                for (FileItem item : items) {
                    if (!item.isFormField()) {

                        String endsWith = agent.getFileExtension(item);
                        if (endsWith == "") {
                            // Add handling for other formats as needed
                            throw new IllegalArgumentException("Unknown file format");
                        }

                        if (item.getFieldName().equals("dataGraph")) {
                            dataGraphFile = File.createTempFile("dataGraph", "." + endsWith);
                            item.write(dataGraphFile);
                        } else if (item.getFieldName().equals("shaclGraph")) {
                            shaclGraphFile = File.createTempFile("shaclGraph", "." + endsWith);
                            item.write(shaclGraphFile);
                        }
                    }
                }

                // Ensure both files are uploaded
                if (dataGraphFile == null || shaclGraphFile == null) {
                    res.status(400); // Bad Request
                    return "Both dataGraph and shaclGraph files must be provided.";
                }

                // Validate the knowledge graph against SHACL constraints
                ValidationReport report = agent.validate(dataGraphFile.getAbsolutePath(),
                        shaclGraphFile.getAbsolutePath());

                // Prepare the response
                res.type("text/turtle");
                return agent.reportToString(report, RDFFormat.TURTLE);

            } catch (Exception e) {
                res.status(500); // Internal Server Error
                return "An error occurred: " + e.getMessage();
            } finally {
                // Clean up temporary files
                if (dataGraphFile != null)
                    dataGraphFile.delete();
                if (shaclGraphFile != null)
                    shaclGraphFile.delete();
            }
        });

        // Health check endpoint
        get("/health", (req, res) -> "SHACL Validation Agent is running!");
    }

    public void runSparkServer(int portnumber) {
        // Start the Spark server
        port(portnumber);
    }

    public String getFileExtension(FileItem item) {
        String fileName = item.getName();
        if (fileName != null && !fileName.isEmpty()) {
            fileName = fileName.substring(fileName.lastIndexOf("/") + 1).substring(fileName.lastIndexOf("\\") + 1);

            int dotIndex = fileName.lastIndexOf('.');
            if (dotIndex > 0 && dotIndex < fileName.length() - 1) {
                return fileName.substring(dotIndex + 1);
            }
        }
        return ""; // No extension found
    }

    // Method to perform SHACL validation
    public ValidationReport validate(String dataGraphPath, String shaclGraphPath) throws Exception {
        // Load data graph and SHACL shapes graph
        Model dataModel = loadModel(dataGraphPath);
        Model shaclModel = loadModel(shaclGraphPath);

        // Parse SHACL shapes
        Shapes shapes = Shapes.parse(shaclModel.getGraph());

        // Perform SHACL validation
        return ShaclValidator.get().validate(shapes, dataModel.getGraph());
    }

    // Helper method to load an RDF model from a file
    private Model loadModel(String filePath) throws FileNotFoundException {
        Model model = ModelFactory.createDefaultModel();
        System.out.println(filePath);
        File file = new File(filePath);
        if (!file.exists()) {
            throw new FileNotFoundException("File not found: " + file.getAbsolutePath());
        }

        try (FileInputStream inputStream = new FileInputStream(file)) {
            // Detect format based on file extension
            String fileExtension = file.getName().toLowerCase();

            if (fileExtension.endsWith(".ttl")) {
                RDFDataMgr.read(model, inputStream, RDFLanguages.TURTLE);
            } else if (fileExtension.endsWith(".rdf")) {
                RDFDataMgr.read(model, inputStream, RDFLanguages.RDFXML);
            } else if (fileExtension.endsWith(".jsonld")) {
                RDFDataMgr.read(model, inputStream, RDFLanguages.JSONLD);
            } else {
                // Add handling for other formats as needed
                throw new IllegalArgumentException("Unsupported RDF file format");
            }
        } catch (IOException e) {
            e.printStackTrace();
            throw new FileNotFoundException("Error reading the file: " + file.getAbsolutePath());
        }

        return model;
    }

    // Helper to convert a validation report to a string in a specific RDF format
    private String reportToString(ValidationReport report, RDFFormat format) {
        try {
            java.io.ByteArrayOutputStream out = new java.io.ByteArrayOutputStream();
            RDFDataMgr.write(out, report.getModel(), format);
            return out.toString();
        } catch (Exception e) {
            return "Error converting report to string: " + e.getMessage();
        }
    }
}
