<?php

namespace Phive\Twig\Extensions\Deferred;

class DeferredTokenParser extends \Twig_TokenParser
{
    private $blockTokenParser;

    public function setParser(\Twig_Parser $parser)
    {
        parent::setParser($parser);

        $this->blockTokenParser = new \Twig_TokenParser_Block();
        $this->blockTokenParser->setParser($parser);
    }

    public function parse(\Twig_Token $token)
    {
        $stream = $this->parser->getStream();
        $nameToken = $stream->next();
        $deferredToken = $stream->nextIf(\Twig_Token::NAME_TYPE, 'deferred');
        $stream->injectTokens(array($nameToken));

        $node = $this->blockTokenParser->parse($token);

        if ($deferredToken) {
            $this->replaceBlockNode($nameToken->getValue());
        }

        return $node;
    }

    public function getTag()
    {
        return 'block';
    }

    private function replaceBlockNode($name)
    {
        $block = $this->parser->getBlock($name)->getNode(0);
        $this->parser->setBlock($name, $this->createDeferredBlockNode($block));
    }

    private function createDeferredBlockNode(\Twig_Node_Block $block)
    {
        $name = $block->getAttribute('name');
        $deferredBlock = new DeferredBlockNode($name, new \Twig_Node(array()), $block->getTemplateLine());

        foreach ($block as $nodeName => $node) {
            $deferredBlock->setNode($nodeName, $node);
        }

        $deferredBlock->setTemplateName($block->getTemplateName());

        return $deferredBlock;
    }
}
