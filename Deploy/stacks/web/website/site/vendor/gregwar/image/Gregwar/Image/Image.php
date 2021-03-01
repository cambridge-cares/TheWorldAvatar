<?php

namespace Gregwar\Image;

use Gregwar\Cache\Cache;
use Gregwar\Cache\CacheInterface;
use Gregwar\Image\Adapter\AdapterInterface;
use Gregwar\Image\Adapter\GD;
use Gregwar\Image\Adapter\Imagick;
use Gregwar\Image\Exceptions\GenerationError;
use Gregwar\Image\Source\Create;
use Gregwar\Image\Source\Data;
use Gregwar\Image\Source\File;
use Gregwar\Image\Source\Resource;
use Gregwar\Image\Source\Source;

/**
 * Images handling class.
 *
 * @author Gregwar <g.passault@gmail.com>
 *
 * @method Image saveGif($file)
 * @method Image savePng($file)
 * @method Image saveJpeg($file, $quality)
 * @method Image resize($width = null, $height = null, $background = 'transparent', $force = false, $rescale = false, $crop = false)
 * @method Image forceResize($width = null, $height = null, $background = 'transparent')
 * @method Image scaleResize($width = null, $height = null, $background = 'transparent', $crop = false)
 * @method Image cropResize($width = null, $height = null, $background=0xffffff)
 * @method Image scale($width = null, $height = null, $background=0xffffff, $crop = false)
 * @method Image ($width = null, $height = null, $background = 0xffffff, $force = false, $rescale = false, $crop = false)
 * @method Image crop($x, $y, $width, $height)
 * @method Image enableProgressive()
 * @method Image force($width = null, $height = null, $background = 0xffffff)
 * @method Image zoomCrop($width, $height, $background = 'transparent', $xPosLetter = 'center', $yPosLetter = 'center')
 * @method Image fillBackground($background = 0xffffff)
 * @method Image negate()
 * @method Image brightness($brightness)
 * @method Image contrast($contrast)
 * @method Image grayscale()
 * @method Image emboss()
 * @method Image smooth($p)
 * @method Image sharp()
 * @method Image edge()
 * @method Image colorize($red, $green, $blue)
 * @method Image sepia()
 * @method Image merge(Image $other, $x = 0, $y = 0, $width = null, $height = null)
 * @method Image rotate($angle, $background = 0xffffff)
 * @method Image fill($color = 0xffffff, $x = 0, $y = 0)
 * @method Image write($font, $text, $x = 0, $y = 0, $size = 12, $angle = 0, $color = 0x000000, $align = 'left')
 * @method Image rectangle($x1, $y1, $x2, $y2, $color, $filled = false)
 * @method Image roundedRectangle($x1, $y1, $x2, $y2, $radius, $color, $filled = false)
 * @method Image line($x1, $y1, $x2, $y2, $color = 0x000000)
 * @method Image ellipse($cx, $cy, $width, $height, $color = 0x000000, $filled = false)
 * @method Image circle($cx, $cy, $r, $color = 0x000000, $filled = false)
 * @method Image polygon(array $points, $color, $filled = false)
 * @method Image flip($flipVertical, $flipHorizontal)
 */
class Image
{
    /**
     * Directory to use for file caching.
     * @var string
     */
    protected $cacheDir = 'cache/images';

    /**
     * Directory cache mode. Not used.
     * @var null
     */
    protected $cacheMode;

    /**
     * Internal adapter.
     *
     * @var AdapterInterface|null
     */
    protected $adapter;

    /**
     * Pretty name for the image.
     * @var string
     */
    protected $prettyName = '';

    /** @var bool */
    protected $prettyPrefix = false;

    /**
     * Transformations hash.
     * @var string|null
     */
    protected $hash;

    /**
     * The image source.
     * @var Source|null
     */
    protected $source;

    /**
     * Force image caching, even if there is no operation applied.
     * @var bool
     */
    protected $forceCache = true;

    /**
     * Supported types.
     * @var array
     */
    public static $types = array(
        'jpg'   => 'jpeg',
        'jpeg'  => 'jpeg',
        'webp'  => 'webp',
        'png'   => 'png',
        'gif'   => 'gif',
    );

    /**
     * Fallback image.
     * @var string
     */
    protected $fallback;

    /**
     * Use fallback image.
     * @var bool
     */
    protected $useFallbackImage = true;

    /**
     * Cache system.
     *
     * @var CacheInterface
     */
    protected $cache;

    /**
     * Get the cache system.
     *
     * @return CacheInterface
     */
    public function getCacheSystem()
    {
        if (is_null($this->cache)) {
            $this->cache = new Cache();
            $this->cache->setCacheDirectory($this->cacheDir);
        }

        return $this->cache;
    }

    /**
     * Set the cache system.
     *
     * @param CacheInterface $cache
     *
     * @return void
     */
    public function setCacheSystem(CacheInterface $cache)
    {
        $this->cache = $cache;
    }

    /**
     * Change the caching directory.
     *
     * @param string $cacheDir
     *
     * @return $this
     */
    public function setCacheDir($cacheDir)
    {
        $this->getCacheSystem()->setCacheDirectory($cacheDir);

        return $this;
    }

    /**
     * @param int $dirMode
     *
     * @return void
     */
    public function setCacheDirMode($dirMode)
    {
        $this->cache->setDirectoryMode($dirMode);
    }

    /**
     * Enable or disable to force cache even if the file is unchanged.
     *
     * @param bool $forceCache
     *
     * @return $this
     */
    public function setForceCache($forceCache = true)
    {
        $this->forceCache = $forceCache;

        return $this;
    }

    /**
     * The actual cache dir.
     *
     * @param string|null $actualCacheDir
     *
     * @return $this
     */
    public function setActualCacheDir($actualCacheDir)
    {
        $this->getCacheSystem()->setActualCacheDirectory($actualCacheDir);

        return $this;
    }

    /**
     * Sets the pretty name of the image.
     *
     * @param string $name
     * @param bool $prefix
     */
    public function setPrettyName($name, $prefix = true)
    {
        if (empty($name)) {
            return $this;
        }

        $this->prettyName = $this->urlize($name);
        $this->prettyPrefix = $prefix;

        return $this;
    }

    /**
     * Urlizes the prettyName.
     *
     * @param string $name
     *
     * @return string
     */
    protected function urlize($name)
    {
        $transliterator = '\Behat\Transliterator\Transliterator';

        if (class_exists($transliterator)) {
            $name = $transliterator::transliterate($name);
            $name = $transliterator::urlize($name);
        } else {
            $name = strtolower($name);
            $name = str_replace(' ', '-', $name);
            $name = preg_replace('/([^a-z0-9\-]+)/m', '', $name);
        }

        return $name;
    }

    /**
     * Operations array.
     * @var array
     */
    protected $operations = array();

    /**
     * Image constructor.
     *
     * @param string|null $originalFile
     * @param int|null $width
     * @param int|null $height
     */
    public function __construct($originalFile = null, $width = null, $height = null)
    {
        $this->setFallback(null);

        if ($originalFile) {
            $this->source = new File($originalFile);
        } else {
            $this->source = new Create($width, $height);
        }
    }

    /**
     * Sets the image data.
     *
     * @param string $data
     *
     * @return void
     */
    public function setData($data)
    {
        $this->source = new Data($data);
    }

    /**
     * Sets the resource.
     *
     * @param resource $resource
     *
     * @return void
     */
    public function setResource($resource)
    {
        $this->source = new Resource($resource);
    }

    /**
     * Use the fallback image or not.
     *
     * @param bool $useFallbackImage
     *
     * @return $this
     */
    public function useFallback($useFallbackImage = true)
    {
        $this->useFallbackImage = $useFallbackImage;

        return $this;
    }

    /**
     * Sets the fallback image to use.
     *
     * @param string|null $fallback
     *
     * @return $this
     */
    public function setFallback($fallback = null)
    {
        if ($fallback === null) {
            $this->fallback = __DIR__.'/images/error.jpg';
        } else {
            $this->fallback = $fallback;
        }

        return $this;
    }

    /**
     * Gets the fallback image path.
     *
     * @return string
     */
    public function getFallback()
    {
        return $this->fallback;
    }

    /**
     * Gets the fallback into the cache dir.
     *
     * @return string
     */
    public function getCacheFallback()
    {
        $fallback = $this->fallback;

        return $this->getCacheSystem()->getOrCreateFile('fallback.jpg', array(), function ($target) use ($fallback) {
            copy($fallback, $target);
        });
    }

    /**
     * @return AdapterInterface
     */
    public function getAdapter()
    {
        if (null === $this->adapter) {
            // Defaults to GD
            $this->setAdapter('gd');
        }

        return $this->adapter;
    }

    /**
     * @param AdapterInterface|string $adapter
     * @throws \Exception
     */
    public function setAdapter($adapter)
    {
        if ($adapter instanceof AdapterInterface) {
            $this->adapter = $adapter;
        } else {
            if (is_string($adapter)) {
                $adapter = strtolower($adapter);

                switch ($adapter) {
                case 'gd':
                    $this->adapter = new GD();
                    break;
                case 'imagemagick':
                case 'imagick':
                    $this->adapter = new Imagick();
                    break;
                default:
                    throw new \Exception('Unknown adapter: ' . $adapter);
                }
            } else {
                throw new \Exception('Unable to load the given adapter (not string or Adapter)');
            }
        }

        $this->adapter->setSource($this->source);
    }

    /**
     * Get the file path.
     *
     * @return string|null a string with the filename, null if the image does not depends on a file
     */
    public function getFilePath()
    {
        if ($this->source instanceof File) {
            return $this->source->getFile();
        }

        return null;
    }

    /**
     * Defines the file only after instantiation.
     *
     * @param string $originalFile the file path
     *
     * @return $this
     */
    public function fromFile($originalFile)
    {
        $this->source = new File($originalFile);

        return $this;
    }

    /**
     * Tells if the image is correct.
     *
     * @return bool
     */
    public function correct()
    {
        return $this->source->correct();
    }

    /**
     * Guess the file type.
     *
     * @return string
     */
    public function guessType()
    {
        return $this->source->guessType();
    }

    /**
     * Adds an operation.
     *
     * @param string $method
     * @param mixed $args
     *
     * @return void
     */
    protected function addOperation($method, $args)
    {
        $this->operations[] = array($method, $args);
    }

    /**
     * Generic function.
     *
     * @param string $methodName
     * @param array $args
     *
     * @return $this
     */
    public function __call($methodName, $args)
    {
        $adapter = $this->getAdapter();
        $reflection = new \ReflectionClass(get_class($adapter));

        if ($reflection->hasMethod($methodName)) {
            $method = $reflection->getMethod($methodName);

            if ($method->getNumberOfRequiredParameters() > count($args)) {
                throw new \InvalidArgumentException('Not enough arguments given for '.$methodName);
            }

            $this->addOperation($methodName, $args);

            return $this;
        }

        throw new \BadFunctionCallException('Invalid method: '.$methodName);
    }

    /**
     * Serialization of operations.
     *
     * @return string
     */
    public function serializeOperations()
    {
        $datas = array();

        foreach ($this->operations as $operation) {
            $method = $operation[0];
            $args = $operation[1];

            foreach ($args as &$arg) {
                if ($arg instanceof self) {
                    $arg = $arg->getHash();
                }
            }
            unset($arg);

            $datas[] = array($method, $args);
        }

        return serialize($datas);
    }

    /**
     * Generates the hash.
     *
     * @param string $type
     * @param int $quality
     *
     * @return void
     */
    public function generateHash($type = 'guess', $quality = 80)
    {
        $inputInfos = $this->source->getInfos();

        $datas = array(
            $inputInfos,
            $this->serializeOperations(),
            $type,
            $quality,
        );

        $this->hash = sha1(serialize($datas));
    }

    /**
     * Gets the hash.
     *
     * @param string $type
     * @param int $quality
     *
     * @return string
     */
    public function getHash($type = 'guess', $quality = 80)
    {
        if (null === $this->hash) {
            $this->generateHash($type, $quality);
        }

        return $this->hash;
    }

    /**
     * Gets the cache file name and generate it if it does not exists.
     * Note that if it exists, all the image computation process will
     * not be done.
     *
     * @param string $type    the image type
     * @param int    $quality the quality (for JPEG)
     * @param bool   $actual
     *
     * @return string
     */
    public function cacheFile($type = 'jpg', $quality = 80, $actual = false)
    {
        if ($type === 'guess') {
            $type = $this->guessType();
        }

        if (!count($this->operations) && $type === $this->guessType() && !$this->forceCache) {
            return $this->getFilename($this->getFilePath());
        }

        // Computes the hash
        $this->hash = $this->getHash($type, $quality);

        // Generates the cache file
        $cacheFile = '';

        if (!$this->prettyName || $this->prettyPrefix) {
            $cacheFile .= $this->hash;
        }

        if ($this->prettyPrefix) {
            $cacheFile .= '-';
        }

        if ($this->prettyName) {
            $cacheFile .= $this->prettyName;
        }

        $cacheFile .= '.'.$type;

        // If the files does not exists, save it
        $image = $this;

        // Target file should be younger than all the current image
        // dependencies
        $conditions = array(
            'younger-than' => $this->getDependencies(),
        );

        // The generating function
        $generate = function ($target) use ($image, $type, $quality) {
            $result = $image->save($target, $type, $quality);

            if ($result != $target) {
                throw new GenerationError($result);
            }
        };

        // Asking the cache for the cacheFile
        try {
            $file = $this->getCacheSystem()->getOrCreateFile($cacheFile, $conditions, $generate, $actual);
        } catch (GenerationError $e) {
            $file = $e->getNewFile();
        }

        // Nulling the resource
        $this->getAdapter()->setSource(new File($file));
        $this->getAdapter()->deinit();

        if ($actual) {
            return $file;
        }

        return $this->getFilename($file);
    }

    /**
     * Get cache data (to render the image).
     *
     * @param string $type    the image type
     * @param int    $quality the quality (for JPEG)
     *
     * @return string|false
     */
    public function cacheData($type = 'jpg', $quality = 80)
    {
        return file_get_contents($this->cacheFile($type, $quality));
    }

    /**
     * Hook to helps to extends and enhance this class.
     *
     * @param string $filename
     *
     * @return string
     */
    protected function getFilename($filename)
    {
        return $filename;
    }

    /**
     * Generates and output a jpeg cached file.
     *
     * @param int $quality
     *
     * @return string
     */
    public function jpeg($quality = 80)
    {
        return $this->cacheFile('jpg', $quality);
    }

    /**
     * Generates and output a gif cached file.
     *
     * @return string
     */
    public function gif()
    {
        return $this->cacheFile('gif');
    }

    /**
     * Generates and output a png cached file.
     *
     * @return string
     */
    public function png()
    {
        return $this->cacheFile('png');
    }

    /**
     * Generates and output a webp cached file.
     *
     * @param int $quality
     *
     * @return string
     */
    public function webp($quality = 80)
    {
        return $this->cacheFile('webp', $quality);
    }

    /**
     * Generates and output an image using the same type as input.
     *
     * @param int $quality
     *
     * @return string
     */
    public function guess($quality = 80)
    {
        return $this->cacheFile('guess', $quality);
    }

    /**
     * Get all the files that this image depends on.
     *
     * @return string[] this is an array of strings containing all the files that the
     *                  current Image depends on
     */
    public function getDependencies()
    {
        $dependencies = array();

        $file = $this->getFilePath();
        if ($file) {
            $dependencies[] = $file;
        }

        foreach ($this->operations as $operation) {
            foreach ($operation[1] as $argument) {
                if ($argument instanceof self) {
                    $dependencies = array_merge($dependencies, $argument->getDependencies());
                }
            }
        }

        return $dependencies;
    }

    /**
     * Applies the operations.
     *
     * @return void
     */
    public function applyOperations()
    {
        // Renders the effects
        foreach ($this->operations as $operation) {
            call_user_func_array(array($this->adapter, $operation[0]), $operation[1]);
        }
    }

    /**
     * Initialize the adapter.
     *
     * @return void
     */
    public function init()
    {
        $this->getAdapter()->init();
    }

    /**
     * Save the file to a given output.
     *
     * @param string $file
     * @param string|int $type
     * @param int $quality
     * @return string|false
     * @throws \Exception
     */
    public function save($file, $type = 'guess', $quality = 80)
    {
        if ($file) {
            $directory = dirname($file);

            if (!is_dir($directory)) {
                @mkdir($directory, 0777, true);
            }
        }

        if (is_int($type)) {
            $quality = $type;
            $type = 'jpeg';
        }

        if ($type === 'guess') {
            $type = $this->guessType();
        }

        if (!isset(self::$types[$type])) {
            throw new \InvalidArgumentException('Given type ('.$type.') is not valid');
        }

        $type = self::$types[$type];

        try {
            $this->init();
            $this->applyOperations();

            $success = false;

            if (null === $file) {
                ob_start();
            }

            if ($type === 'jpeg') {
                $success = $this->getAdapter()->saveJpeg($file, $quality);
            }

            if ($type === 'gif') {
                $success = $this->getAdapter()->saveGif($file);
            }

            if ($type === 'png') {
                $success = $this->getAdapter()->savePng($file);
            }

            if ($type === 'webp') {
                $success = $this->getAdapter()->saveWebP($file, $quality);
            }

            if (!$success) {
                return false;
            }

            return null === $file ? ob_get_clean() : $file;
        } catch (\Exception $e) {
            if ($this->useFallbackImage) {
                return null === $file ? file_get_contents($this->fallback) : $this->getCacheFallback();
            } else {
                throw $e;
            }
        }
    }

    /**
     * Get the contents of the image.
     *
     * @param string|int $type
     * @param int $quality
     * @return string|false
     * @throws \Exception
     */
    public function get($type = 'guess', $quality = 80)
    {
        return $this->save(null, $type, $quality);
    }

    /* Image API */

    /**
     * Image width.
     *
     * @return int
     */
    public function width()
    {
        return $this->getAdapter()->width();
    }

    /**
     * Image height.
     *
     * @return int
     */
    public function height()
    {
        return $this->getAdapter()->height();
    }

    /**
     * Tostring defaults to jpeg.
     *
     * @return string
     */
    public function __toString()
    {
        return $this->guess();
    }

    /**
     * Returning basic html code for this image.
     *
     * @param string $title
     * @param string $type
     * @param int $quality
     *
     * @return string
     */
    public function html($title = '', $type = 'jpg', $quality = 80)
    {
        return '<img title="'.$title.'" src="'.$this->cacheFile($type, $quality).'" />';
    }

    /**
     * Returns the Base64 inlineable representation.
     *
     * @param string $type
     * @param int $quality
     *
     * @return string
     */
    public function inline($type = 'jpg', $quality = 80)
    {
        $mime = $type;
        if ($mime === 'jpg') {
            $mime = 'jpeg';
        }

        return 'data:image/'.$mime.';base64,'.base64_encode(file_get_contents($this->cacheFile($type, $quality, true)));
    }

    /**
     * Creates an instance, useful for one-line chaining.
     *
     * @param string $file
     *
     * @return static
     */
    public static function open($file = '')
    {
        return new static($file);
    }

    /**
     * Creates an instance of a new resource.
     *
     * @param int $width
     * @param int $height
     *
     * @return static
     */
    public static function create($width, $height)
    {
        return new static(null, $width, $height);
    }

    /**
     * Creates an instance of image from its data.
     *
     * @param string $data
     *
     * @return static
     */
    public static function fromData($data)
    {
        $image = new static();
        $image->setData($data);

        return $image;
    }

    /**
     * Creates an instance of image from resource.
     *
     * @param resource $resource
     *
     * @return static
     */
    public static function fromResource($resource)
    {
        $image = new static();
        $image->setResource($resource);

        return $image;
    }
}
