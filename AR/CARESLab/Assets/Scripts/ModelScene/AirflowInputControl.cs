using MixedReality.Toolkit.UX.Experimental;
using System.Collections;
using TMPro;
using UnityEngine;
using UnityEngine.Networking;

/// <summary>
/// Script used to update airflow of canopy hood.
/// </summary>
public class AirflowInputControl : MonoBehaviour
{
    [SerializeField]
    private TMP_Text titleTMP;

    [SerializeField]
    private NonNativeKeyboard NonNativeKeyboard;

    // NOTICE: Change the following endpoint and iris to valid string for this function to work properly.
    private string bmsUpdateAgentUrl = Config.BmsUpdateAgentUrl;
    private string canopyhoodAirflowIri = Config.CanopyhoodAirflowIri;
    private string canopyhoodControlModeIri = Config.CanopyhoodControlModeIri;

    /// <summary>
    /// Open keyboard to symbol layout.
    /// </summary>
    public void SetupKeyboard()
    {
        NonNativeKeyboard.Open(NonNativeKeyboard.LayoutType.Symbol);
        titleTMP.SetText("Please enter the new airflow setpoint (between 400-1100)");
        titleTMP.color = new Color(255, 255, 255, 255);
    }

    /// <summary>
    /// Check the entered setpoint and send to the bms update agent
    /// </summary>
    public void SubmitNewSetPoint()
    {
        // NOTICE: This text is not shown because the keyboard is closed.
        if (!double.TryParse(NonNativeKeyboard.Text, out double result))
        {
            titleTMP.SetText("Please enter a valid number.");
            titleTMP.color = new Color(255, 0, 0, 255);
            return;
        }

        if (result < 400 || result > 1100)
        {
            titleTMP.SetText("Please enter a value between 400-1100 m3/h.");
            titleTMP.color = new Color(255, 0, 0, 255);
            return;
        }
        StartCoroutine(SendSetPointRequest());
    }

    private IEnumerator SendSetPointRequest()
    {
        string url = bmsUpdateAgentUrl;
        WWWForm form = new();
        form.AddField("dataIRI", canopyhoodAirflowIri);
        form.AddField("value", NonNativeKeyboard.Text);
        form.AddField("clientProperties", "WRITE_CLIENT_PROPERTIES");

        using UnityWebRequest www = UnityWebRequest.Post(url, form);
        Debug.Log("Update airflow setpoint to " + NonNativeKeyboard.Text);
        yield return www.SendWebRequest();

        if (www.result != UnityWebRequest.Result.Success)
        {
            Debug.Log(www.error);
        }
        else
        {
            Debug.Log("Airflow setpoint updated, updating change mode now...");
            StartCoroutine(SendChangeModeRequest());
        }
    }

    private IEnumerator SendChangeModeRequest()
    {
        string url = bmsUpdateAgentUrl;
        WWWForm form = new();
        form.AddField("dataIRI", canopyhoodControlModeIri);
        form.AddField("value", "1.0");
        form.AddField("clientProperties", "WRITE_CLIENT_PROPERTIES");

        using UnityWebRequest www = UnityWebRequest.Post(url, form);
        Debug.Log("Update control mode to CARES mode");
        yield return www.SendWebRequest();

        if (www.result != UnityWebRequest.Result.Success)
        {
            Debug.Log(www.error);
        }
        else
        {
            Debug.Log("Change mode updated.");
        }
    }
}
