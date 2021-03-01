<?php

namespace Gregwar\Image\Source;

/**
 * Creates a new image from scratch.
 */
class Create extends Source
{
    /** @var int */
    protected $width;
    /** @var int */
    protected $height;

    /**
     * Create constructor.
     * @param int $width
     * @param int $height
     */
    public function __construct($width, $height)
    {
        $this->width = $width;
        $this->height = $height;
    }

    /**
     * @return int
     */
    public function getWidth()
    {
        return $this->width;
    }

    /**
     * @return int
     */
    public function getHeight()
    {
        return $this->height;
    }

    /**
     * @return array|string|null
     */
    public function getInfos()
    {
        return array($this->width, $this->height);
    }

    /**
     * @return bool
     */
    public function correct()
    {
        return $this->width > 0 && $this->height > 0;
    }
}
