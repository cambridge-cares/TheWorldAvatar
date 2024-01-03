using UnityEngine;

/// <summary>
/// Store model related information and prepare SelectedModel for scene navigation.
/// </summary>
public class FloorMapModelController : MonoBehaviour
{
    [SerializeField]
    private SelectedModel selectedModel;

    [SerializeField]
    private string dashboardUri = "";

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
        selectedModel.DashboardUri = dashboardUri;
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


}
