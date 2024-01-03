using Newtonsoft.Json.Linq;
using System.Collections;
using UnityEngine;
using UnityEngine.Networking;

/// <summary>
/// Room information data class
/// </summary>
public class RoomInfo
{
    public string RoomLable { set; get; }
    public double RoomTemperature { set; get; }
    public double RoomHumidity { set; get; }
    public string RetrivedAt { set; get; }
}

/// <summary>
/// Handle the retrieval and display of the room information.
/// </summary>
public class RoomInfoHandler : MonoBehaviour
{
    /// <summary>
    /// Room iri
    /// </summary>
    public string Iri;

    [SerializeField]
    private GameObject InfoDisplayPanelPrefab;

    [SerializeField]
    private GameObject Canvas;

    [SerializeField]
    private Vector3 panelPosition;

    private GameObject panelInstance;
    private InfoDisplayPanel infoDisplayPanel;

    /// <summary>
    /// Callback for button click event. Should attached to <see href="https://learn.microsoft.com/en-us/dotnet/api/microsoft.mixedreality.toolkit.ui.pressablebutton?view=mixed-reality-toolkit-unity-2020-dotnet-2.8.0" langword="Pressable Button"/>
    /// Send request to feature-info-agent and display the information on <see cref="InfoDisplayPanel"/>.
    /// </summary>
    public void OnClickListener()
    {
        Debug.Log("Iri: " + Iri);
        Debug.Log(Iri != null);
        if (Iri != null)
        {
            StartCoroutine(SendRequest());
        }

    }

    IEnumerator SendRequest()
    {
        // show info display pannel
        if (panelInstance == null)
        {
            InitPanelInstance();
        }
        panelInstance.SetActive(true);
        infoDisplayPanel.SetTitleText(gameObject.name);
        infoDisplayPanel.Spinner.SetActive(true);
        infoDisplayPanel.BodyText.SetActive(false);

        string url = "https://www.theworldavatar.com:1010/careslab/feature-info-agent/get?iri=" + Iri;
        using UnityWebRequest www = UnityWebRequest.Get(url);
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

    void HandleJSONResponse(string jsonResponse)
    {
        JObject jObject = JObject.Parse(jsonResponse);

        RoomInfo roomInfo = new RoomInfo();
        roomInfo.RoomLable = (string)jObject["meta"].First["Room label"];
        Debug.Log("Room label: " + roomInfo.RoomLable);

        if (!jObject.ContainsKey("time"))
        {
            infoDisplayPanel.SetBodyText("No data retrieved.");
            return;
        }

        foreach (JObject data in jObject["time"])
        {
            Debug.Log(data["data"]);
            if (string.Equals(data["data"].First.ToString(), "Ambient relative humidity"))
            {
                roomInfo.RoomHumidity = System.Math.Round(double.Parse(data["values"].Last.Last.ToString()), 1);
                Debug.Log("Humidity" + roomInfo.RoomHumidity);
            }
            else if (string.Equals(data["data"].First.ToString(), "Ambient temperature"))
            {
                roomInfo.RoomTemperature = System.Math.Round(double.Parse(data["values"].Last.Last.ToString()), 1);
                Debug.Log("Temperature" + roomInfo.RoomTemperature);
            }
        }
        roomInfo.RetrivedAt = jObject["time"].First["time"].Last.ToString();

        infoDisplayPanel.Spinner.SetActive(false);
        infoDisplayPanel.BodyText.SetActive(true);
        infoDisplayPanel.SetBodyText($@"
Temperature: {roomInfo.RoomTemperature} °C
Humidity: {roomInfo.RoomHumidity} %
Retrieved at: {roomInfo.RetrivedAt}");
    }

    private void InitPanelInstance()
    {
        panelInstance = Instantiate(InfoDisplayPanelPrefab);
        panelInstance.transform.SetParent(Canvas.transform, false);
        panelInstance.transform.localPosition = panelPosition;
        panelInstance.transform.localScale = new Vector3(0.5f, 0.5f, 1f);

        infoDisplayPanel = panelInstance.GetComponent<InfoDisplayPanel>();
    }
}