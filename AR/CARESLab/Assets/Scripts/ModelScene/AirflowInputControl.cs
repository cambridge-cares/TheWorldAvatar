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

    public void SetupKeyboard()
    {
        SetupKeyboard("Please enter the new airflow setpoint (between 400-1100)", new Color(255, 255, 255, 255));
    }

    /// <summary>
    /// Open keyboard to symbol layout.
    /// </summary>
    private void SetupKeyboard(string text, Color color)
    {
        NonNativeKeyboard.Open(NonNativeKeyboard.LayoutType.Symbol);
        titleTMP.SetText(text);
        titleTMP.color = color;
    }

    /// <summary>
    /// Check the entered setpoint and send to the bms update agent
    /// </summary>
    public void SubmitNewSetPoint()
    {
        if (!double.TryParse(NonNativeKeyboard.Text, out double result))
        {
            Debug.LogError("Invalid number, cannot be parsed to double.");
            StartCoroutine(ReopenKeyboardDueToInvalidInput("Please enter a valid number."));
            return;
        }

        if (result < 400 || result > 1100)
        {
            Debug.LogError("Value out of range.");
            StartCoroutine(ReopenKeyboardDueToInvalidInput("Please enter a value between 400-1100 m3/h."));
            return;
        }

        if (Config.BmsUpdateAgentUrl == null || Config.CanopyhoodAirflowIri == null || Config.CanopyhoodControlModeIri == null)
        {
            Debug.LogError("Config BmsUpdateAgentUrl or CanopyhoodAirflowIri or CanopyhoodControlModeIri not init. Please check endpoints.properties file and start from Home scene.");
            return;
        }
        StartCoroutine(SendSetPointRequest());
    }

    private IEnumerator ReopenKeyboardDueToInvalidInput(string text)
    {
        if (NonNativeKeyboard.isActiveAndEnabled)
        {
            yield return null;
        }
        SetupKeyboard(text, new Color(255, 0, 0, 255));
    }

    private IEnumerator SendSetPointRequest()
    {
        string url = Config.BmsUpdateAgentUrl;
        WWWForm form = new();
        form.AddField("dataIRI", Config.CanopyhoodAirflowIri);
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
        string url = Config.BmsUpdateAgentUrl;
        WWWForm form = new();
        form.AddField("dataIRI", Config.CanopyhoodControlModeIri);
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
