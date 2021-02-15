<?php

namespace RocketTheme\Toolbox\StreamWrapper;

use RocketTheme\Toolbox\ResourceLocator\ResourceLocatorInterface;

/**
 * Implements Read Only Streams.
 *
 * @package RocketTheme\Toolbox\StreamWrapper
 * @author RocketTheme
 * @license MIT
 */
class ReadOnlyStream extends Stream
{
    /** @var ResourceLocatorInterface */
    protected static $locator;

    /**
     * @param string $uri
     * @param string $mode
     * @param int $options
     * @param string $opened_url
     * @return bool
     */
    public function stream_open($uri, $mode, $options, &$opened_url)
    {
        if (!\in_array($mode, ['r', 'rb', 'rt'], true)) {
            if ($options & STREAM_REPORT_ERRORS) {
                trigger_error(sprintf('stream_open() write modes not allowed for %s', $uri), E_USER_WARNING);
            }

            return false;
        }

        $path = $this->getPath($uri);

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

            return true;
        }

        return false;
    }

    /**
     * @param int $operation
     * @return bool
     */
    public function stream_lock($operation)
    {
        // Disallow exclusive lock or non-blocking lock requests
        if (!\in_array($operation, [LOCK_SH, LOCK_UN, LOCK_SH | LOCK_NB], true)) {
            trigger_error(
                sprintf('stream_lock() exclusive lock operations not allowed for %s', $this->uri),
                E_USER_WARNING
            );

            return false;
        }

        return flock($this->handle, $operation);
    }

    /**
     * @param string $uri
     * @param int $option
     * @param mixed $value
     * @return bool
     */
    public function stream_metadata($uri, $option, $value)
    {
        if ($option !== STREAM_META_TOUCH) {
            throw new \BadMethodCallException(sprintf('stream_metadata() not allowed for %s', $uri));
        }

        return parent::stream_metadata($uri, $option, $value);
    }

    /**
     * @param string $data
     * @return int|false
     */
    public function stream_write($data)
    {
        throw new \BadMethodCallException(sprintf('stream_write() not allowed for %s', $this->uri));
    }

    /**
     * @param string $uri
     * @return bool
     */
    public function unlink($uri)
    {
        throw new \BadMethodCallException(sprintf('unlink() not allowed for %s', $uri));
    }

    /**
     * @param string $from_uri
     * @param string $to_uri
     * @return bool
     */
    public function rename($from_uri, $to_uri)
    {
        throw new \BadMethodCallException(sprintf('rename() not allowed for %s', $from_uri));
    }

    /**
     * @param string $uri
     * @param int $mode
     * @param int $options
     * @return bool
     */
    public function mkdir($uri, $mode, $options)
    {
        throw new \BadMethodCallException(sprintf('mkdir() not allowed for %s', $uri));
    }

    /**
     * @param string $uri
     * @param int $options
     * @return bool
     */
    public function rmdir($uri, $options)
    {
        throw new \BadMethodCallException(sprintf('rmdir() not allowed for %s', $uri));
    }
}
