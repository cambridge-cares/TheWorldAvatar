<?php

namespace RocketTheme\Toolbox\StreamWrapper;

use RocketTheme\Toolbox\ResourceLocator\ResourceLocatorInterface;
use RocketTheme\Toolbox\ResourceLocator\UniformResourceLocator;

/**
 * Implements Read/Write Streams.
 *
 * @package RocketTheme\Toolbox\StreamWrapper
 * @author RocketTheme
 * @license MIT
 */
class Stream implements StreamInterface
{
    /** @var string */
    protected $uri;
    /** @var Resource A generic resource handle. */
    protected $handle;
    /** @var ResourceLocatorInterface|UniformResourceLocator|null */
    protected static $locator;

    /**
     * @param ResourceLocatorInterface $locator
     * @return void
     */
    public static function setLocator(ResourceLocatorInterface $locator)
    {
        static::$locator = $locator;
    }

    /**
     * @param string $uri
     * @param string $mode
     * @param int $options
     * @param string $opened_url
     * @return bool
     */
    public function stream_open($uri, $mode, $options, &$opened_url)
    {
        $path = $this->getPath($uri, $mode);

        if (!$path) {
            if ($options & STREAM_REPORT_ERRORS) {
                trigger_error(sprintf('stream_open(): path for %s does not exist', $uri), E_USER_WARNING);
            }

            return false;
        }

        $this->uri = $uri;

        $handle = ($options & STREAM_REPORT_ERRORS) ? fopen($path, $mode) : @fopen($path, $mode);
        if ($handle) {
            $this->handle = $handle;

            if (static::$locator instanceof UniformResourceLocator && !\in_array($mode, ['r', 'rb', 'rt'], true)) {
                static::$locator->clearCache($this->uri);
            }

            return true;
        }

        return false;
    }

    /**
     * @return bool
     */
    public function stream_close()
    {
        return fclose($this->handle);
    }

    /**
     * @param int $operation
     * @return bool
     */
    public function stream_lock($operation)
    {
        if (\in_array($operation, [LOCK_SH, LOCK_EX, LOCK_UN, LOCK_NB], true)) {
            return flock($this->handle, $operation);
        }

        return false;
    }

    /**
     * @param string $uri
     * @param int $option
     * @param mixed $value
     * @return bool
     */
    public function stream_metadata($uri, $option, $value)
    {
        $path = $this->findPath($uri);
        if ($path) {
            switch ($option) {
                case STREAM_META_TOUCH:
                    list($time, $atime) = $value;
                    return touch($path, $time, $atime);

                case STREAM_META_OWNER_NAME:
                case STREAM_META_OWNER:
                    return chown($path, $value);

                case STREAM_META_GROUP_NAME:
                case STREAM_META_GROUP:
                    return chgrp($path, $value);

                case STREAM_META_ACCESS:
                    return chmod($path, $value);
            }
        }

        return false;
    }

    /**
     * @param int $count
     * @return string|false
     */
    public function stream_read($count)
    {
        return fread($this->handle, $count);
    }

    /**
     * @param string $data
     * @return int|false
     */
    public function stream_write($data)
    {
        return fwrite($this->handle, $data);
    }

    /**
     * @return bool
     */
    public function stream_eof()
    {
        return feof($this->handle);
    }

    /**
     * @param int $offset
     * @param int $whence
     * @return bool
     */
    public function stream_seek($offset, $whence)
    {
        // fseek returns 0 on success and -1 on a failure.
        return !fseek($this->handle, $offset, $whence);
    }

    /**
     * @return bool
     */
    public function stream_flush()
    {
        return fflush($this->handle);
    }

    /**
     * @return int|false
     */
    public function stream_tell()
    {
        return ftell($this->handle);
    }

    /**
     * @return array
     */
    public function stream_stat()
    {
        return fstat($this->handle) ?: [];
    }

    /**
     * @param int $option
     * @param int $arg1
     * @param int $arg2
     * @return bool|int
     */
    public function stream_set_option($option, $arg1, $arg2)
    {
        switch ($option) {
            case STREAM_OPTION_BLOCKING:
                return stream_set_blocking($this->handle, (bool)$arg1);
            case STREAM_OPTION_READ_TIMEOUT:
                return stream_set_timeout($this->handle, $arg1, $arg2);
            case STREAM_OPTION_WRITE_BUFFER:
                return stream_set_write_buffer($this->handle, $arg2);
            default:
                return false;
        }
    }

    /**
     * @param string $uri
     * @return bool
     */
    public function unlink($uri)
    {
        $path = $this->getPath($uri);

        if (!$path) {
            return false;
        }

        return unlink($path);
    }

    /**
     * @param string $fromUri
     * @param string $toUri
     * @return bool
     */
    public function rename($fromUri, $toUri)
    {
        $fromPath = $this->getPath($fromUri);
        $toPath = $this->getPath($toUri, 'w');

        if (!$fromPath || !$toPath) {
            return false;
        }

        if (static::$locator instanceof UniformResourceLocator) {
            static::$locator->clearCache($fromUri);
            static::$locator->clearCache($toUri);
        }

        return rename($fromPath, $toPath);
    }

    /**
     * @param string $uri
     * @param int $mode
     * @param int $options
     * @return bool
     */
    public function mkdir($uri, $mode, $options)
    {
        $recursive = (bool) ($options & STREAM_MKDIR_RECURSIVE);
        $path = $this->getPath($uri, $recursive ? 'd' : 'w');

        if (!$path) {
            if ($options & STREAM_REPORT_ERRORS) {
                trigger_error(sprintf('mkdir(): Could not create directory for %s', $uri), E_USER_WARNING);
            }

            return false;
        }

        if (static::$locator instanceof UniformResourceLocator) {
            static::$locator->clearCache($uri);
        }

        return ($options & STREAM_REPORT_ERRORS) ? mkdir($path, $mode, $recursive) : @mkdir($path, $mode, $recursive);
    }

    /**
     * @param string $uri
     * @param int $options
     * @return bool
     */
    public function rmdir($uri, $options)
    {
        $path = $this->getPath($uri);

        if (!$path) {
            if ($options & STREAM_REPORT_ERRORS) {
                trigger_error(sprintf('rmdir(): Directory not found for %s', $uri), E_USER_WARNING);
            }

            return false;
        }

        if (static::$locator instanceof UniformResourceLocator) {
            static::$locator->clearCache($uri);
        }

        return ($options & STREAM_REPORT_ERRORS) ? rmdir($path) : @rmdir($path);
    }

    /**
     * @param string $uri
     * @param int $flags
     * @return array|false
     */
    public function url_stat($uri, $flags)
    {
        $path = $this->getPath($uri);

        if (!$path) {
            return false;
        }

        // Suppress warnings if requested or if the file or directory does not
        // exist. This is consistent with PHPs plain filesystem stream wrapper.
        return ($flags & STREAM_URL_STAT_QUIET || !file_exists($path)) ? @stat($path) : stat($path);
    }

    /**
     * @param string $uri
     * @param int $options
     * @return bool
     */
    public function dir_opendir($uri, $options)
    {
        $path = $this->getPath($uri);

        if ($path === false) {
            return false;
        }

        $this->uri = $uri;

        $handle = opendir($path);
        if ($handle) {
            $this->handle = $handle;

            return true;
        }

        return false;
    }

    /**
     * @return string|false
     */
    public function dir_readdir()
    {
        return readdir($this->handle);
    }

    /**
     * @return bool
     */
    public function dir_rewinddir()
    {
        rewinddir($this->handle);

        return true;
    }

    /**
     * @return bool
     */
    public function dir_closedir()
    {
        closedir($this->handle);

        return true;
    }

    /**
     * @param string $uri
     * @param string|null $mode
     * @return string|false
     */
    protected function getPath($uri, $mode = null)
    {
        if ($mode === null) {
            $mode = 'r';
        }

        $path = $this->findPath($uri);

        if ($path && file_exists($path)) {
            return $path;
        }

        if (strpos($mode[0], 'r') === 0) {
            return false;
        }

        // We are either opening a file or creating directory.
        list($scheme, $target) = explode('://', $uri, 2);

        if ($target === '') {
            return false;
        }
        $target = explode('/', $target);
        $filename = [];

        do {
            $filename[] = array_pop($target);

            $path = $this->findPath($scheme . '://' . implode('/', $target));
        } while ($target && !$path);

        if (!$path) {
            return false;
        }

        return $path . '/' .  implode('/', array_reverse($filename));
    }

    /**
     * @param string $uri
     * @return string|false
     */
    protected function findPath($uri)
    {
        return static::$locator && static::$locator->isStream($uri) ? static::$locator->findResource($uri) : false;
    }
}
