<?php

namespace Gregwar\Image\Exceptions;

class GenerationError extends \Exception
{
    public $newNewFile;

    public function __construct($newNewFile)
    {
        parent::__construct();

        $this->newNewFile = $newNewFile;
    }

    public function getNewFile()
    {
        return $this->newNewFile;
    }
}
