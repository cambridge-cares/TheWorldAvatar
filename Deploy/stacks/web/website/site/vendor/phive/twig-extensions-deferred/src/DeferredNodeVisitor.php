<?php

namespace Phive\Twig\Extensions\Deferred;

class DeferredNodeVisitor implements \Twig_NodeVisitorInterface
{
    private $hasDeferred = false;

    /**
     * {@inheritdoc}
     */
    public function enterNode(\Twig_NodeInterface $node, \Twig_Environment $env)
    {
        if (!$this->hasDeferred && $node instanceof DeferredBlockNode) {
            $this->hasDeferred = true;
        }

        return $node;
    }

    /**
     * {@inheritdoc}
     */
    public function leaveNode(\Twig_NodeInterface $node, \Twig_Environment $env)
    {
        if ($this->hasDeferred && $node instanceof \Twig_Node_Module) {
            $node->setNode('display_end', new \Twig_Node(array(new DeferredNode(), $node->getNode('display_end'))));
            $this->hasDeferred = false;
        }

        return $node;
    }

    /**
     * {@inheritdoc}
     */
    public function getPriority()
    {
        return 0;
    }
}
