using UnityEngine;

/// <summary>
/// Store model related information and prepare SelectedModel for scene navigation.
/// </summary>
public class FloorMapModelController : MonoBehaviour
{
    [SerializeField]
    private SelectedModel selectedModel;

    [SerializeField]
    private string iri = "";

    [SerializeField]
    private GameObject realSizeModel;

    [SerializeField]
    private SceneTransitionScript sceneTransition;

    /// <summary>
    /// Set selected model and navigate to model scene.
    /// </summary>
    public void TransitToModelScene()
    {
        selectedModel.ModelName = gameObject.name;
        selectedModel.Iri = iri;
        selectedModel.DashboardUri = GetDashboardUri();
        selectedModel.SelectedModelTransform = gameObject.transform;
        selectedModel.Position = gameObject.transform.position;
        selectedModel.Rotation = gameObject.transform.rotation;
        selectedModel.Scale = gameObject.transform.lossyScale;
        selectedModel.Model = realSizeModel;

        if (!sceneTransition.isActiveAndEnabled)
        {
            sceneTransition.enabled = true;
            sceneTransition.gameObject.SetActive(true);
        }
        sceneTransition.NavigateToScene("ModelScene");
    }

    private string GetDashboardUri()
    {
        if (gameObject.name.Contains("Fumehood"))
        {
            try
            {
                return Config.FumehoodDashboardUrl.ToString() + "&var-fumehood=" + gameObject.name.Replace("Fumehood", "FH-");
            } catch
            {
                Debug.LogError("Config FumehoodDashboardUrl not init. Please check endpoints.properties file and start from Home scene.");
                return "";
            }
            
        } else if (gameObject.name.Contains("PIPS Robot"))
        {
            try
            {
                return Config.CanopyhoodDashboardUrl.ToString() + "&var-canopyhood=" + "CH-7-7-CAV_E7_07";
            }
            catch
            {
                Debug.LogError("Config CanopyhoodDashboardUrl not init. Please check endpoints.properties file and start from Home scene.");
                return "";
            }
        } else
        {
            return "";
        }
    }
}
