<?php

namespace RocketTheme\Toolbox\File;

/**
 * Implements Universal File Reader.
 *
 * @package RocketTheme\Toolbox\File
 * @author RocketTheme
 * @license MIT
 */
abstract class AbstractFile implements FileInterface
{
    /** @var string */
    protected $filename;
    /** @var resource|null */
    protected $handle;
    /** @var bool|null */
    protected $locked;
    /** @var string */
    protected $extension;
    /** @var string|null  Raw file contents. */
    protected $raw;
    /** @var array|string|null  Parsed file contents. */
    protected $content;
    /** @var array */
    protected $settings = [];

    /** @var static[] */
    static protected $instances = [];

    /**
     * Get file instance.
     *
     * @param string $filename
     * @return static
     */
    public static function instance($filename)
    {
        if (!\is_string($filename) || $filename === '') {
            user_error(__METHOD__ . '() should not be called with empty filename, this will stop working in the future!', E_USER_DEPRECATED);

            // TODO: fail in the future (and also remove $this->filename === null checks).
            //throw new \InvalidArgumentException('Filename should be non-empty string');
            return new static();
        }

        if (!isset(static::$instances[$filename])) {
            static::$instances[$filename] = new static();
            static::$instances[$filename]->init($filename);
        }

        return static::$instances[$filename];
    }

    /**
     * Set/get settings.
     *
     * @param array $settings
     * @return array
     */
    public function settings(array $settings = null)
    {
        if ($settings !== null) {
            $this->settings = $settings;
        }

        return $this->settings;
    }

    /**
     * Get setting.
     *
     * @param string $setting
     * @param mixed $default
     * @return mixed
     */
    public function setting($setting, $default = null)
    {
        return isset($this->settings[$setting]) ? $this->settings[$setting] : $default;
    }

    /**
     * Prevent constructor from being used.
     */
    protected function __construct()
    {
    }

    /**
     * Prevent cloning.
     */
    protected function __clone()
    {
        //Me not like clones! Me smash clones!
    }

    /**
     * Set filename.
     *
     * @param string $filename
     * @return void
     */
    protected function init($filename)
    {
        $this->filename = $filename;
    }

    /**
     * Free the file instance.
     *
     * @return void
     */
    public function free()
    {
        if ($this->locked) {
            $this->unlock();
        }
        $this->content = null;
        $this->raw = null;

        if (null !== $this->filename) {
            unset(static::$instances[$this->filename]);
        }
    }

    /**
     * Get/set the file location.
     *
     * @param  string $var
     * @return string
     */
    public function filename($var = null)
    {
        if ($var !== null) {
            $this->filename = $var;
        }

        return $this->filename;
    }

    /**
     * Return basename of the file.
     *
     * @return string
     */
    public function basename()
    {
        return null !== $this->filename ? basename($this->filename, $this->extension) : '';
    }

    /**
     * Check if file exits.
     *
     * @return bool
     */
    public function exists()
    {
        return null !== $this->filename && is_file($this->filename);
    }

    /**
     * Return file modification time.
     *
     * @return int|false Timestamp or false if file doesn't exist.
     */
    public function modified()
    {
        return null !== $this->filename && $this->exists() ? filemtime($this->filename) : false;
    }

    /**
     * Lock file for writing. You need to manually unlock().
     *
     * @param bool $block  For non-blocking lock, set the parameter to false.
     * @return bool
     * @throws \RuntimeException
     */
    public function lock($block = true)
    {
        if (null === $this->filename) {
            throw new \RuntimeException('Opening file for writing failed because of it has no filename');
        }

        if (!$this->handle) {
            if (!$this->mkdir(\dirname($this->filename))) {
                throw new \RuntimeException('Creating directory failed for ' . $this->filename);
            }

            $handle = @fopen($this->filename, 'cb+');
            if (!$handle) {
                $error = error_get_last() ?: ['message' => 'Unknown error'];

                throw new \RuntimeException("Opening file for writing failed on error {$error['message']}");
            }
            $this->handle = $handle;
        }
        $lock = $block ? LOCK_EX : LOCK_EX | LOCK_NB;

        // Some filesystems do not support file locks, only fail if another process holds the lock.
        $this->locked = flock($this->handle, $lock, $wouldblock) || !$wouldblock;

        return $this->locked;
    }

    /**
     * Returns true if file has been locked for writing.
     *
     * @return bool|null True = locked, false = failed, null = not locked.
     */
    public function locked()
    {
        return $this->locked;
    }

    /**
     * Unlock file.
     *
     * @return bool
     */
    public function unlock()
    {
        if (!$this->handle) {
            return false;
        }

        if ($this->locked) {
            flock($this->handle, LOCK_UN);
            $this->locked = null;
        }

        fclose($this->handle);
        $this->handle = null;

        return true;
    }

    /**
     * Check if file can be written.
     *
     * @return bool
     */
    public function writable()
    {
        if (null === $this->filename) {
            return false;
        }

        return $this->exists() ? is_writable($this->filename) : $this->writableDir(\dirname($this->filename));
    }

    /**
     * (Re)Load a file and return RAW file contents.
     *
     * @return string
     */
    public function load()
    {
        $this->raw = null !== $this->filename && $this->exists() ? (string) file_get_contents($this->filename) : '';
        $this->content = null;

        return $this->raw;
    }

    /**
     * Get/set raw file contents.
     *
     * @param string $var
     * @return string
     */
    public function raw($var = null)
    {
        if ($var !== null) {
            $this->raw = (string) $var;
            $this->content = null;
        }

        if (!\is_string($this->raw)) {
            $this->raw = $this->load();
        }

        return $this->raw;
    }

    /**
     * Get/set parsed file contents.
     *
     * @param string|array|null $var
     * @return string|array
     * @throws \RuntimeException
     */
    public function content($var = null)
    {
        if ($var !== null) {
            $this->content = $this->check($var);

            // Update RAW, too.
            $this->raw = $this->encode($this->content);

        } elseif ($this->content === null) {
            // Decode RAW file.
            try {
                $this->content = $this->decode($this->raw());
            } catch (\Exception $e) {
                throw new \RuntimeException(sprintf('Failed to read %s: %s', $this->filename, $e->getMessage()), 500, $e);
            }
        }

        return $this->content;
    }

    /**
     * Save file.
     *
     * @param  mixed  $data  Optional data to be saved, usually array.
     * @return void
     * @throws \RuntimeException
     */
    public function save($data = null)
    {
        if (null === $this->filename) {
            throw new \RuntimeException('Failed to save file: no filename');
        }

        if ($data !== null) {
            $this->content($data);
        }

        $filename = $this->filename;

        if (is_link($filename)) {
            $realname = realpath($filename);
            if ($realname === false) {
                throw new \RuntimeException('Failed to save file ' . $filename);
            }

            $filename = $realname;
        }

        $dir = \dirname($filename);

        if (!$dir || !$this->mkdir($dir)) {
            throw new \RuntimeException('Creating directory failed for ' . $filename);
        }

        try {
            if ($this->handle) {
                $tmp = true;
                // As we are using non-truncating locking, make sure that the file is empty before writing.
                if (@ftruncate($this->handle, 0) === false || @fwrite($this->handle, $this->raw()) === false) {
                    // Writing file failed, throw an error.
                    $tmp = false;
                }
            } else {
                // Create file with a temporary name and rename it to make the save action atomic.
                $tmp = $this->tempname($filename);
                if (file_put_contents($tmp, $this->raw()) === false) {
                    $tmp = false;
                } elseif (@rename($tmp, $filename) === false) {
                    @unlink($tmp);
                    $tmp = false;
                }
            }
        } catch (\Exception $e) {
            $tmp = false;
        }

        if ($tmp === false) {
            throw new \RuntimeException('Failed to save file ' . $filename);
        }

        // Touch the directory as well, thus marking it modified.
        @touch($dir);
    }

    /**
     * Rename file in the filesystem if it exists.
     *
     * @param string $filename
     * @return bool
     */
    public function rename($filename)
    {
        if (null !== $this->filename && $this->exists() && !@rename($this->filename, $filename)) {
            return false;
        }

        unset(static::$instances[$this->filename]);
        static::$instances[$filename] = $this;

        $this->filename = $filename;

        return true;
    }

    /**
     * Delete file from filesystem.
     *
     * @return bool
     */
    public function delete()
    {
        return null !== $this->filename && $this->exists() && unlink($this->filename);
    }

    /**
     * Check contents and make sure it is in correct format.
     *
     * Override in derived class.
     *
     * @param mixed $var
     * @return mixed
     */
    protected function check($var)
    {
        if (!\is_string($var)) {
            throw new \RuntimeException('Provided data is not a string');
        }

        return $var;
    }

    /**
     * Encode contents into RAW string.
     *
     * Override in derived class.
     *
     * @param array|string $var
     * @return string
     */
    protected function encode($var)
    {
        return \is_string($var) ? $var : '';
    }

    /**
     * Decode RAW string into contents.
     *
     * Override in derived class.
     *
     * @param string $var
     * @return array|string
     */
    protected function decode($var)
    {
        return $var;
    }

    /**
     * @param string $dir
     * @return bool
     */
    private function mkdir($dir)
    {
        // Silence error for open_basedir; should fail in mkdir instead.
        if (@is_dir($dir)) {
            return true;
        }

        $success = @mkdir($dir, 0777, true);

        if (!$success) {
            // Take yet another look, make sure that the folder doesn't exist.
            clearstatcache(true, $dir);
            if (!@is_dir($dir)) {
                return false;
            }
        }

        return true;
    }

    /**
     * @param string $dir
     * @return bool
     * @internal
     */
    protected function writableDir($dir)
    {
        if ($dir && !file_exists($dir)) {
            return $this->writableDir(\dirname($dir));
        }

        return $dir && is_dir($dir) && is_writable($dir);
    }

    /**
     * @param string $filename
     * @param int $length
     * @return string
     */
    protected function tempname($filename, $length = 5)
    {
        do {
            $test = $filename . substr(str_shuffle('0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'), 0, $length);
        } while (file_exists($test));

        return $test;
    }
}
