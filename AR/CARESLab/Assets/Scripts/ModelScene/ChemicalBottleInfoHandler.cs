using Newtonsoft.Json.Linq;
using System.Collections;
using UnityEngine;
using UnityEngine.Networking;

/// <summary>
/// Script to show chemical bottle information
/// </summary>
public class ChemicalBottleInfoHandler : MonoBehaviour
{
    [SerializeField]
    private string chemicalContainerIRI;

    [SerializeField]
    private GameObject infoDisplayPanelPrefab;

    [SerializeField]
    private Vector3 panelOffset;

    private GameObject panelInstance;

    private InfoDisplayPanel infoDisplayPanel;

    /// <summary>
    /// Retrieve chemical bottle information from RFID query agent and display it on an <see cref="InfoDisplayPanel"/>
    /// </summary>
    public void ShowChemicalInfo()
    {
        StartCoroutine(RetrieveChemicalInfo());
    }

    private IEnumerator RetrieveChemicalInfo()
    {
        if (panelInstance == null)
        {
            InitPanelInstance();
        }
        panelInstance.SetActive(true);
        infoDisplayPanel.SetTitleText(gameObject.name);
        infoDisplayPanel.Spinner.SetActive(true);
        infoDisplayPanel.BodyText.SetActive(false);

        if (Config.RfidQueryAgentUrl == null)
        {
            Debug.LogError("Config RfidQueryAgentUrl not init. Please check endpoints.properties file and start from Home scene.");
            yield break;
        }

        string url = Config.RfidQueryAgentUrl;
        WWWForm form = new();
        form.AddField("timeSeriesClientProperties", "CLIENT_PROPERTIES");
        form.AddField("taggedObjectIRI", chemicalContainerIRI);
        form.AddField("speciesProperties", "SPECIES_PROPERTIES");

        using UnityWebRequest www = UnityWebRequest.Post(url, form);
        Debug.Log("Request sent to " + url);
        yield return www.SendWebRequest();

        if (www.result == UnityWebRequest.Result.ConnectionError || www.result == UnityWebRequest.Result.ProtocolError)
        {
            Debug.LogError("Error: " + www.error);
        }
        else
        {
            Debug.Log("Processing json response");
            string jsonResponse = www.downloadHandler.text;

            // Parse the JSON response
            HandleJSONResponse(jsonResponse);
        }
    }

    private void HandleJSONResponse(string jsonResponse)
    {
        JObject jObject = JObject.Parse(jsonResponse);
        string result = JObjectToString(jObject);

        Debug.Log(result);
        infoDisplayPanel.SetBodyText(result);
        infoDisplayPanel.Spinner.SetActive(false);
        infoDisplayPanel.BodyText.SetActive(true);
    }

    private string JObjectToString(JObject jObject, string prefix = "")
    {
        string result = "";
        foreach (JProperty property in jObject.Properties())
        {
            if (property.Value is JObject)
            {
                result += JObjectToString((JObject)property.Value, prefix + "    ");
                continue;
            }
            result += prefix + property.Name + ": " + property.Value + "\n";
        }
        return result;
    }

    private void InitPanelInstance()
    {
        panelInstance = Instantiate(infoDisplayPanelPrefab);
        panelInstance.transform.position = panelOffset + transform.position;
        panelInstance.transform.localScale = new Vector3(0.01f, 0.01f, 0.01f);
        FollowBottle followBottle = panelInstance.AddComponent<FollowBottle>();
        followBottle.Target = gameObject;

        infoDisplayPanel = panelInstance.GetComponent<InfoDisplayPanel>();
    }
}