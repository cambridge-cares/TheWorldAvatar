<?php

namespace RocketTheme\Toolbox\ResourceLocator;

/**
 * Implements Uniform Resource Location.
 *
 * @package RocketTheme\Toolbox\ResourceLocator
 * @author RocketTheme
 * @license MIT
 *
 * @link http://webmozarts.com/2013/06/19/the-power-of-uniform-resource-location-in-php/
 */
class UniformResourceLocator implements ResourceLocatorInterface
{
    /** @var string  Base URL for all the streams. */
    public $base;

    /** @var array[] */
    protected $schemes = [];

    /** @var array */
    protected $cache = [];

    /**
     * UniformResourceLocator constructor.
     *
     * @param string|null $base
     */
    public function __construct($base = null)
    {
        // Normalize base path.
        $this->base = rtrim(str_replace('\\', '/', $base ?: getcwd() ?: ''), '/');
    }

    /**
     * Return iterator for the resource URI.
     *
     * @param string $uri
     * @param int|null $flags See constants from FilesystemIterator class.
     * @return UniformResourceIterator
     */
    public function getIterator($uri, $flags = null)
    {
        return new UniformResourceIterator($uri, $flags, $this);
    }

    /**
     * Return recursive iterator for the resource URI.
     *
     * @param string $uri
     * @param int|null $flags    See constants from FilesystemIterator class.
     * @return RecursiveUniformResourceIterator
     */
    public function getRecursiveIterator($uri, $flags = null)
    {
        return new RecursiveUniformResourceIterator($uri, $flags, $this);
    }

    /**
     * Reset locator by removing all the schemes.
     *
     * @return $this
     */
    public function reset()
    {
        $this->schemes = [];
        $this->cache = [];

        return $this;
    }

    /**
     * Reset a locator scheme
     *
     * @param string $scheme The scheme to reset
     * @return $this
     */
    public function resetScheme($scheme)
    {
        $this->schemes[$scheme] = [];
        $this->cache = [];

        return $this;
    }

    /**
     * Add new paths to the scheme.
     *
     * @param string $scheme
     * @param string $prefix
     * @param string|array $paths
     * @param bool|string|string[]  $override  True to add path as override, string
     * @param bool $force     True to add paths even if them do not exist.
     * @return void
     * @throws \BadMethodCallException
     */
    public function addPath($scheme, $prefix, $paths, $override = false, $force = false)
    {
        $list = [];

        /** @var array<int,string|array> $paths */
        $paths = (array)$paths;
        foreach ($paths as $path) {
            if (\is_array($path)) {
                // Support stream lookup in ['theme', 'path/to'] format.
                if (\count($path) !== 2 || !\is_string($path[0]) || !\is_string($path[1])) {
                    throw new \BadMethodCallException('Invalid stream path given.');
                }
                $list[] = $path;
            } elseif (false !== strpos($path, '://')) {
                // Support stream lookup in 'theme://path/to' format.
                $stream = explode('://', $path, 2);
                $stream[1] = trim($stream[1], '/');

                $list[] = $stream;
            } else {
                // Normalize path.
                $path = rtrim(str_replace('\\', '/', $path), '/');
                if ($force || @file_exists("{$this->base}/{$path}") || @file_exists($path)) {
                    // Support for absolute and relative paths.
                    $list[] = $path;
                }
            }
        }

        if (isset($this->schemes[$scheme][$prefix])) {
            $paths = $this->schemes[$scheme][$prefix];
            if (!$override || $override == 1) {
                $list = $override ? array_merge($paths, $list) : array_merge($list, $paths);
            } else {
                if (is_string($override) && false !== strpos($override, '://')) {
                    // Support stream lookup in 'theme://path/to' format.
                    $override = explode('://', $override, 2);
                    $override[1] = trim($override[1], '/');
                }
                $location = (int)array_search($override, $paths, true) ?: \count($paths);
                array_splice($paths, $location, 0, $list);
                $list = $paths;
            }
        }

        $this->schemes[$scheme][$prefix] = $list;

        // Sort in reverse order to get longer prefixes to be matched first.
        krsort($this->schemes[$scheme]);

        $this->cache = [];
    }

    /**
     * Return base directory.
     *
     * @return string
     */
    public function getBase()
    {
        return $this->base;
    }


    /**
     * Return true if scheme has been defined.
     *
     * @param string $name
     * @return bool
     */
    public function schemeExists($name)
    {
        return isset($this->schemes[$name]);
    }

    /**
     * Return defined schemes.
     *
     * @return array
     */
    public function getSchemes()
    {
        return array_keys($this->schemes);
    }

    /**
     * Return all scheme lookup paths.
     *
     * @param string $scheme
     * @return array
     */
    public function getPaths($scheme = null)
    {
        if (null !== $scheme) {
            return isset($this->schemes[$scheme]) ? $this->schemes[$scheme] : [];
        }

        return $this->schemes;
    }

    /**
     * @param string $uri
     * @return string|false
     * @throws \BadMethodCallException
     */
    public function __invoke($uri)
    {
        if (!\is_string($uri)) {
            throw new \BadMethodCallException('Invalid parameter $uri.');
        }

        /** @var string|false $cached */
        $cached = $this->findCached($uri, false, true, false);

        return $cached;
    }

    /**
     * Returns true if uri is resolvable by using locator.
     *
     * @param string $uri
     * @return bool
     */
    public function isStream($uri)
    {
        try {
            list ($scheme,) = $this->normalize($uri, true, true);
        } catch (\Exception $e) {
            return false;
        }

        return $this->schemeExists($scheme);
    }

    /**
     * Returns the canonicalized URI on success. The resulting path will have no '/./' or '/../' components.
     * Trailing delimiter `/` is kept.
     *
     * By default (if $throwException parameter is not set to true) returns false on failure.
     *
     * @param string $uri
     * @param bool $throwException
     * @param bool $splitStream
     * @return string|array|false
     * @throws \BadMethodCallException
     */
    public function normalize($uri, $throwException = false, $splitStream = false)
    {
        if (!\is_string($uri)) {
            if ($throwException) {
                throw new \BadMethodCallException('Invalid parameter $uri.');
            }

            return false;
        }

        $uri = (string)preg_replace('|\\\|u', '/', $uri);
        $segments = explode('://', $uri, 2);
        $path = array_pop($segments);
        $scheme = array_pop($segments) ?: 'file';

        if ($path) {
            $path = (string)preg_replace('|\\\|u', '/', $path);
            $parts = explode('/', $path);

            $list = [];
            foreach ($parts as $i => $part) {
                if ($part === '..') {
                    $part = array_pop($list);
                    if ($part === null || $part === '' || (!$list && strpos($part, ':'))) {
                        if ($throwException) {
                            throw new \BadMethodCallException('Invalid parameter $uri.');
                        }

                        return false;
                    }
                } elseif (($i && $part === '') || $part === '.') {
                    continue;
                } else {
                    $list[] = $part;
                }
            }

            if (($l = end($parts)) === '' || $l === '.' || $l === '..') {
                $list[] = '';
            }

            $path = implode('/', $list);
        }

        if ($splitStream) {
            return [$scheme, $path];
        }

        return $scheme !== 'file' ? "{$scheme}://{$path}" : $path;
    }

    /**
     * Get resource path. If resource does not exist, return path with highest priority.
     *
     * @param string $uri Input URI to be searched.
     * @param bool $absolute Whether to return absolute path.
     * @return string
     * @throws \BadMethodCallException
     */
    public function getResource($uri, $absolute = true)
    {
        $path = $this->findResource($uri, $absolute);
        if ($path === false) {
            $path = $this->findResource($uri, $absolute, true);
            if ($path === false) {
                $path = '';
            }
        }

        return $path;
    }

    /**
     * Find highest priority instance from a resource.
     *
     * @param string $uri Input URI to be searched.
     * @param bool $absolute Whether to return absolute path.
     * @param bool $first Whether to return first path even if it doesn't exist.
     * @return string|false
     * @throws \BadMethodCallException
     */
    public function findResource($uri, $absolute = true, $first = false)
    {
        if (!\is_string($uri)) {
            throw new \BadMethodCallException('Invalid parameter $uri.');
        }

        /** @var string|false $cached */
        $cached = $this->findCached($uri, false, $absolute, $first);

        return $cached;
    }

    /**
     * Find all instances from a resource.
     *
     * @param string $uri Input URI to be searched.
     * @param bool $absolute Whether to return absolute path.
     * @param bool $all Whether to return all paths even if they don't exist.
     * @return array
     * @throws \BadMethodCallException
     */
    public function findResources($uri, $absolute = true, $all = false)
    {
        if (!\is_string($uri)) {
            throw new \BadMethodCallException('Invalid parameter $uri.');
        }

        /** @var array $cached */
        $cached = $this->findCached($uri, true, $absolute, $all);

        return $cached;
    }

    /**
     * Find all instances from a list of resources.
     *
     * @param array $uris Input URIs to be searched.
     * @param bool $absolute Whether to return absolute path.
     * @param bool $all Whether to return all paths even if they don't exist.
     * @return array
     * @throws \BadMethodCallException
     */
    public function mergeResources(array $uris, $absolute = true, $all = false)
    {
        $uris = array_unique($uris);

        $lists = [[]];
        foreach ($uris as $uri) {
            $lists[] = $this->findResources($uri, $absolute, $all);
        }

        return array_merge(...$lists);
    }

    /**
     * Pre-fill cache by a stream.
     *
     * @param string $uri
     * @return $this
     */
    public function fillCache($uri)
    {
        $cacheKey = $uri . '@cache';

        if (!isset($this->cache[$cacheKey])) {
            $this->cache[$cacheKey] = true;

            $iterator = new \RecursiveIteratorIterator($this->getRecursiveIterator($uri), \RecursiveIteratorIterator::SELF_FIRST);

            /** @var UniformResourceIterator $item */
            foreach ($iterator as $item) {
                $key = $item->getUrl() . '@010';
                $this->cache[$key] = $item->getPathname();
            }
        }

        return $this;
    }

    /**
     * Reset locator cache.
     *
     * @param string $uri
     * @return $this
     */
    public function clearCache($uri = null)
    {
        if ($uri) {
            $this->clearCached($uri, true, true, true);
            $this->clearCached($uri, true, true, false);
            $this->clearCached($uri, true, false, true);
            $this->clearCached($uri, true, false, false);
            $this->clearCached($uri, false, true, true);
            $this->clearCached($uri, false, true, false);
            $this->clearCached($uri, false, false, true);
            $this->clearCached($uri, false, false, false);
        } else {
            $this->cache = [];
        }

        return $this;
    }

    /**
     * @param string $uri
     * @param bool $array
     * @param bool $absolute
     * @param bool $all
     * @return array|string|false
     * @throws \BadMethodCallException
     */
    protected function findCached($uri, $array, $absolute, $all)
    {
        // Local caching: make sure that the function gets only called at once for each file.
        $key = $uri .'@'. (int) $array . (int) $absolute . (int) $all;

        if (!isset($this->cache[$key])) {
            try {
                list ($scheme, $file) = $this->normalize($uri, true, true);

                if (!$file && $scheme === 'file') {
                    $file = $this->base;
                }

                $this->cache[$key] = $this->find($scheme, $file, $array, $absolute, $all);

            } catch (\BadMethodCallException $e) {
                $this->cache[$key] =  $array ? [] : false;
            }
        }

        return $this->cache[$key];
    }

    /**
     * @param string $uri
     * @param bool $array
     * @param bool $absolute
     * @param bool $all
     * @return void
     */
    protected function clearCached($uri, $array, $absolute, $all)
    {
        // Local caching: make sure that the function gets only called at once for each file.
        $key = $uri .'@'. (int) $array . (int) $absolute . (int) $all;

        unset($this->cache[$key]);
    }

    /**
     * @param string $scheme
     * @param string $file
     * @param bool $array
     * @param bool $absolute
     * @param bool $all
     * @return array|string|false
     * @throws \InvalidArgumentException
     * @internal
     */
    protected function find($scheme, $file, $array, $absolute, $all)
    {
        if (!isset($this->schemes[$scheme])) {
            throw new \InvalidArgumentException("Invalid resource {$scheme}://");
        }

        $results = $array ? [] : false;
        foreach ($this->schemes[$scheme] as $prefix => $paths) {
            if ($prefix && strpos($file, $prefix) !== 0) {
                continue;
            }

            // Remove prefix from filename.
            $filename = '/' . trim(substr($file, \strlen($prefix)), '\/');

            foreach ($paths as $path) {
                if (\is_array($path)) {
                    // Handle scheme lookup.
                    $relPath = trim($path[1] . $filename, '/');
                    $found = $this->find($path[0], $relPath, $array, $absolute, $all);
                    if ($found) {
                        if (!\is_array($found)) {
                            return $found;
                        }
                        $results = array_merge($results, $found);
                    }
                } else {
                    // TODO: We could provide some extra information about the path to remove preg_match().
                    // Check absolute paths for both unix and windows
                    if (!$path || !preg_match('`^/|\w+:`', $path)) {
                        // Handle relative path lookup.
                        $relPath = trim($path . $filename, '/');
                        $fullPath = $this->base . '/' . $relPath;
                    } else {
                        // Handle absolute path lookup.
                        $relPath = null;
                        $fullPath = rtrim($path . $filename, '/');
                    }

                    if ($all || file_exists($fullPath)) {
                        if ($absolute) {
                            $current = $fullPath;
                        } elseif (null === $relPath) {
                            throw new \RuntimeException("UniformResourceLocator: Absolute stream path with relative lookup not allowed ({$prefix})", 500);
                        } else {
                            $current = $relPath;
                        }

                        if (!$array) {
                            return $current;
                        }
                        $results[] = $current;
                    }
                }
            }
        }

        return $results;
    }
}
