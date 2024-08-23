package uk.ac.cam.cares.jps.agent.bmsupdate.helper;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.FileEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.junit.rules.TemporaryFolder;

import java.io.*;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class BlazegraphHelper {
    private static List<String> readNamespacePropertiesTemplateFile(String namespace) {
        String path = "src/test/resources/";

        File file = new File(path + "newNamespacePropertiesFileTemplate.xml");
        String templatePath = file.getAbsolutePath();

        List<String> results = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(templatePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.contains("[NAMESPACE]")) {
                    line = line.replace("[NAMESPACE]", namespace);
                }
                results.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return results;
    }

    private static void writeToTempFolder(String filepath, List<String> properties) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : properties) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }

    private static String prepareBlazegraphPropertiesFile(String namespace, TemporaryFolder folder) throws IOException {
        List<String> labPropertiesFile = readNamespacePropertiesTemplateFile(namespace);
        String blazegraphProperty = Paths.get(folder.getRoot().toString(), namespace + "namespace_properties.xml").toString();
        writeToTempFolder(blazegraphProperty, labPropertiesFile);
        return blazegraphProperty;
    }

    public static void createNewNameSpace(String namespace, TemporaryFolder folder, CloseableHttpClient httpClient, String blazegraphEndpoint) throws IOException {
        String testPropertyFile = prepareBlazegraphPropertiesFile(namespace, folder);

        HttpPost httpPostLab = new HttpPost(blazegraphEndpoint + "/namespace");
        httpPostLab.setEntity(new FileEntity(new File(testPropertyFile), ContentType.APPLICATION_XML));
        httpPostLab.setHeader("Content-Type", "application/xml");

        try (CloseableHttpResponse ignored = httpClient.execute(httpPostLab)) {
        }
    }

    public static void createNewData(String fileName, String namespaceUrl, CloseableHttpClient httpClient) throws IOException {
        String path = "src/test/resources/";
        File file = new File(path + fileName);
        String filePath = file.getAbsolutePath();

        HttpPost addData = new HttpPost(namespaceUrl);
        addData.setEntity(new FileEntity(new File(filePath), ContentType.create("application/rdf+xml")));
        addData.setHeader("Content-Type", "application/rdf+xml");
        try (CloseableHttpResponse ignored = httpClient.execute(addData)) {
        }
    }

    public static void clearBlazegraphData(String namespaceUrl, CloseableHttpClient httpClient) throws IOException {
        HttpDelete deleteNamespace = new HttpDelete(namespaceUrl);
        deleteNamespace.setHeader("Accept", "application/xml");
        System.out.println(deleteNamespace);
        try (CloseableHttpResponse ignored = httpClient.execute(deleteNamespace)) {
        }
    }
}
