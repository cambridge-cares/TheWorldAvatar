<?php

namespace Gregwar\Image\Source;

/**
 * Having image in some string.
 */
class Data extends Source
{
    /** @var string */
    protected $data;

    /**
     * Data constructor.
     *
     * @param string $data
     */
    public function __construct($data)
    {
        $this->data = $data;
    }

    /**
     * @return string
     */
    public function getData()
    {
        return $this->data;
    }

    /**
     * @return array|string|null
     */
    public function getInfos()
    {
        return sha1($this->data);
    }
}
