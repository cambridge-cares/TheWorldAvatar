using UnityEngine;

/// <summary>
/// A script used in model scene to control the buttons and information shown on the information display pannel.
/// </summary>
/// <remarks>
/// The default information display pannel is able to show the name of the current model, close model view and open dashboard in browser.
/// PIPS robot has an additional function: enter new setpoint of the canopyhood on top of it.
/// Cabinet removes the dashboard option.
/// </remarks>
public class ControlInfoSetup : MonoBehaviour
{
    [SerializeField]
    private SelectedModel selectedModel;

    [SerializeField]
    private GameObject controlCanvas;

    private GameObject infoDisplayPanelObject;

    // Start is called before the first frame update
    void Start()
    {
        infoDisplayPanelObject = controlCanvas.transform.Find("Container/InfoDisplayPanel").gameObject;
        infoDisplayPanelObject.GetComponent<InfoDisplayPanel>().SetTitleText(selectedModel.ModelName);

        if (selectedModel.ModelName.Contains("PIPS"))
        {
            controlCanvas.transform.Find("Container/EnterNewSetPoint").gameObject.SetActive(true);
        }
        if (selectedModel.ModelName.Contains("Cabinet"))
        {
            controlCanvas.transform.Find("Container/OpenDashboard").gameObject.SetActive(false);
        }
    }

}
