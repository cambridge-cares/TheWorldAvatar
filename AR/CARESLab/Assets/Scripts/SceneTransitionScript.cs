using System.Collections;
using UnityEngine;
using UnityEngine.SceneManagement;

/// <summary>
/// Show scene loading animation and navigate to the specified scene
/// </summary>
public class SceneTransitionScript : MonoBehaviour
{
    [SerializeField]
    private GameObject loadingScreenInstance;

    [SerializeField]
    private float MinLoadTime = 2f;

    private void Start()
    {
        DontDestroyOnLoad(gameObject);

        loadingScreenInstance.SetActive(false);
    }

    /// <summary>
    /// Navigate to the specified scene
    /// </summary>
    /// <param name="sceneName">The name of the new scene</param>
    public void NavigateToScene(string sceneName)
    {
        StartCoroutine(LoadNewScene(sceneName));
    }

    IEnumerator LoadNewScene(string SceneName)
    {
        loadingScreenInstance.SetActive(true);
        AsyncOperation operation = SceneManager.LoadSceneAsync(SceneName);
        float elapsedTime = 0f;

        while (!operation.isDone)
        {
            elapsedTime += Time.deltaTime;
            yield return null;
        }

        Debug.Log(elapsedTime + " finsih loading");
        while (elapsedTime <= MinLoadTime)
        {
            elapsedTime += Time.deltaTime;
            yield return null;
        }

        GetComponent<Animator>().SetTrigger("isSceneLoaded");
    }

    /// <summary>
    /// Disable the scene loading animation
    /// </summary>
    public void DisableLoadScreen()
    {
        Debug.Log("distroy event called");
        Destroy(gameObject);
    }

}
