<?php

namespace Gregwar\Image\Source;

use Gregwar\Image\Image;

/**
 * Open an image from a file.
 */
class File extends Source
{
    /** @var string */
    protected $file;

    /**
     * File constructor.
     *
     * @param string $file
     */
    public function __construct($file)
    {
        $this->file = $file;
    }

    /**
     * @return string
     */
    public function getFile()
    {
        return $this->file;
    }

    /**
     * @return bool
     */
    public function correct()
    {
        if (function_exists('exif_imagetype')) {
            return false !== @exif_imagetype($this->file);
        }

        return true;
    }

    /**
     * @return string
     */
    public function guessType()
    {
        if (function_exists('exif_imagetype')) {
            $type = @exif_imagetype($this->file);

            if (false !== $type) {
                if ($type === IMAGETYPE_JPEG) {
                    return 'jpeg';
                }

                if ($type === IMAGETYPE_GIF) {
                    return 'gif';
                }

                if ($type === IMAGETYPE_PNG) {
                    return 'png';
                }

                if ($type === IMAGETYPE_WEBP) {
                    return 'webp';
                }
            }
        }

        $parts = explode('.', $this->file);
        $ext = strtolower($parts[count($parts) - 1]);

        if (isset(Image::$types[$ext])) {
            return Image::$types[$ext];
        }

        return 'jpeg';
    }

    /**
     * @return array|string|null
     */
    public function getInfos()
    {
        return $this->file;
    }
}
