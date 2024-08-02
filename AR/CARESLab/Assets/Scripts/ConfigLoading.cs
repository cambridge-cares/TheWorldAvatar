using UnityEngine;
using System.IO;


public class ConfigLoading : MonoBehaviour
{

    private void LoadConfigFromFile()
    {
        string configFilePath = Path.Combine(Application.streamingAssetsPath, "endpoints.properties");
        Debug.Log(configFilePath);
        StreamReader reader = new(configFilePath);

        try
        {
            while (!reader.EndOfStream)
            {
                string line = reader.ReadLine();
                processPropertyLine(line);
            }
        }
        catch
        {

        }
        finally
        {
            Debug.Log("Config loaded");
            reader.Close();
        }

    }

    private void processPropertyLine(string line)
    {
        string attrName = line.Split("=")[0];
        string attrVal = line.Replace(attrName + "=", "").Trim();

        if (attrVal[0] == '\"' || attrVal[0] == '\'') attrVal = attrVal.Substring(1);
        if (attrVal[attrVal.Length - 1] == '\"' || attrVal[0] == '\'') attrVal = attrVal.Substring(0, attrVal.Length - 2);

        Debug.Log("attrName: " + attrName + " attrVal: " + attrVal);

        switch (attrName)
        {
            case "FIAGetUrl":
                Config.FIAGetUrl = attrVal;
                break;
            case "RfidQueryAgentUrl":
                Config.RfidQueryAgentUrl = attrVal;
                break;
            case "FumehoodDashboardUrl":
                Config.FumehoodDashboardUrl = attrVal;
                break;
            case "CanopyhoodDashboardUrl":
                Config.CanopyhoodDashboardUrl = attrVal;
                break;
            case "BmsUpdateAgentUrl":
                Config.BmsUpdateAgentUrl = attrVal;
                break;
            case "CanopyhoodAirflowIri":
                Config.CanopyhoodAirflowIri = attrVal;
                break;
            case "CanopyhoodControlModeIri":
                Config.CanopyhoodControlModeIri = attrVal;
                break;
            case "FhSashAndOccupancyAgentUrl":
                Config.FhSashAndOccupancyAgentUrl = attrVal;
                break;
            default:
                break;
        }
    }

    // Start is called before the first frame update
    void Start()
    {
        //StartCoroutine(LoadFileCoroutine());

        LoadConfigFromFile();
    }

    // Update is called once per frame
    void Update()
    {
        
    }
}
