<?php

namespace Gregwar\Image\Adapter;

use Gregwar\Image\Source\Source;

/**
 * Base Adapter Implementation to handle Image information.
 */
abstract class Adapter implements AdapterInterface
{
    /**
     * @var Source
     */
    protected $source;

    /**
     * The image resource handler.
     * @var resource|false
     */
    protected $resource;

    public function __construct()
    {
    }

    /**
     * {@inheritdoc}
     */
    public function setSource(Source $source)
    {
        $this->source = $source;

        return $this;
    }

    /**
     * {@inheritdoc}
     */
    public function getResource()
    {
        return $this->resource;
    }

    /**
     * Does this adapter supports the given type ?
     *
     * @param string
     * @return bool
     */
    protected function supports($type)
    {
        return false;
    }

    /**
     * Converts the image to true color.
     *
     * @return void
     */
    protected function convertToTrueColor()
    {
    }
}
