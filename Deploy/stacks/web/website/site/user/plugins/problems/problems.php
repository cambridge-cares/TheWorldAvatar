<?php
namespace Grav\Plugin;

use Composer\Autoload\ClassLoader;
use Grav\Common\Plugin;
use Grav\Common\Uri;
use Grav\Plugin\Problems\Base\ProblemChecker;
use RocketTheme\Toolbox\Event\Event;

class ProblemsPlugin extends Plugin
{
    protected $checker;
    protected $problems = [];

    /**
     * @return array
     */
    public static function getSubscribedEvents()
    {
        return [
            'onPluginsInitialized' => [
                ['autoload', 100002],
                ['onPluginsInitialized', 100001]
            ],
            'onFatalException' => ['onFatalException', 0],
            'onAdminGenerateReports' => ['onAdminGenerateReports', 0],
        ];
    }

    /**
     * [onPluginsInitialized:100000] Composer autoload.
     *
     * @return ClassLoader
     */
    public function autoload()
    {
        return require __DIR__ . '/vendor/autoload.php';
    }

    public function onFatalException()
    {
        if (\defined('GRAV_CLI') || $this->isAdmin()) {
            return;
        }

        // Run through potential issues
        if ($this->problemsFound()) {
            $this->renderProblems();
        }
    }

    public function onPluginsInitialized()
    {
        if (\defined('GRAV_CLI') || $this->isAdmin()) {
            return;
        }

        $this->checker = new ProblemChecker();

        if (!$this->checker->statusFileExists()) {
            // If no issues remain, save a state file in the cache
            if (!$this->problemsFound()) {
                // delete any existing validated files
                /** @var \SplFileInfo $fileInfo */
                foreach (new \GlobIterator(CACHE_DIR . ProblemChecker::PROBLEMS_PREFIX . '*') as $fileInfo) {
                    @unlink($fileInfo->getPathname());
                }
                // create a file in the cache dir so it only runs on cache changes
                $this->checker->storeStatusFile();
            } else {
                $this->renderProblems();
            }
        }
    }

    private function renderProblems()
    {
        /** @var Uri $uri */
        $uri = $this->grav['uri'];

        /** @var \Twig_Environment $twig */
        $twig = $this->getTwig();

        $data = [
            'problems' => $this->problems,
            'base_url' => $baseUrlRelative = $uri->rootUrl(false),
            'problems_url' => $baseUrlRelative . '/user/plugins/problems',
        ];

        echo $twig->render('problems.html.twig', $data);
        http_response_code(500);
        exit();
    }

    public function onAdminGenerateReports(Event $e)
    {
        $reports = $e['reports'];

        $this->checker = new ProblemChecker();

        // Check for problems
        $this->problemsFound();

        /** @var Uri $uri */
        $uri = $this->grav['uri'];

        /** @var \Twig_Environment $twig */
        $twig = $this->getTwig();

        $data = [
            'problems' => $this->problems,
            'base_url' => $baseUrlRelative = $uri->rootUrl(false),
            'problems_url' => $baseUrlRelative . '/user/plugins/problems',
        ];

        $reports['Grav Potential Problems'] = $twig->render('reports/problems-report.html.twig', $data);

        $this->grav['assets']->addCss('plugins://problems/css/admin.css');
        $this->grav['assets']->addCss('plugins://problems/css/spectre-icons.css');
    }

    private function problemsFound()
    {
        if (null === $this->checker) {
            $this->checker = new ProblemChecker();
        }

        $status = $this->checker->check(__DIR__ . '/classes/Problems');
        $this->problems = $this->checker->getProblems();
        
        return $status;
    }

    private function getTwig()
    {
        $loader = new \Twig_Loader_Filesystem(__DIR__ . '/templates');
        $twig = new \Twig_Environment($loader, ['debug' => true]);
        $twig->addExtension(New \Twig_Extension_Debug());

        return $twig;
    }
}
