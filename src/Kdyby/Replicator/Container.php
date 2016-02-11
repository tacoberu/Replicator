<?php

/**
 * This file is part of the Kdyby (http://www.kdyby.org)
 *
 * Copyright (c) 2008 Filip Procházka (filip@prochazka.su)
 *
 * For the full copyright and license information, please view the file license.md that was distributed with this source code.
 */

namespace Kdyby\Replicator;

use Nette;
use Nette\Forms\SubmitButton;
use Nette\Forms\FormContainer;
use Nette\Web\Html;

/**
 * @author Martin Takáč <martin@takac.name>
 * @author Filip Procházka <filip@prochazka.su>
 * @author Jan Tvrdík
 *
 * @method \Nette\Forms\Form getForm()
 * @method \Nette\Forms\FormContainer getParent()
 * @property \Nette\Forms\FormContainer $parent
 */
class Container extends FormContainer implements Nette\Forms\IFormControl
{

	/** @var bool */
	public $forceDefault;

	/** @var int */
	public $createDefault;

	/** @var string */
	public $containerClass = 'Nette\Forms\FormContainer';

	/** @var callable */
	protected $factoryCallback;

	/** @var boolean */
	private $submittedBy = FALSE;

	/** @var array */
	private $created = array();

	/** @var \Nette\Web\IHttpRequest */
	private $httpRequest;

	/** @var array */
	private $httpPost;

	/** @var Nette\Web\Html  control element template */
	private $control;

	/** @var Nette\ITranslator */
	private $translator = TRUE; // means autodetect

	/** @var array user options */
	private $options = array();

	/** @var Rules */
	private $rules;


	/**
	 * @param callable $factory
	 * @param int $createDefault
	 * @param bool $forceDefault
	 *
	 * @throws \InvalidArgumentException
	 */
	public function __construct($factory, $createDefault = 0, $forceDefault = FALSE)
	{
		parent::__construct();
		$this->monitor('Nette\Application\Presenter');

		try {
			$this->factoryCallback = callback($factory);
		} catch (\InvalidArgumentException $e) {
			$type = is_object($factory) ? 'instanceof ' . get_class($factory) : gettype($factory);
			throw new \InvalidArgumentException(
				'Replicator requires callable factory, ' . $type . ' given.', 0, $e
			);
		}

		$this->createDefault = (int)$createDefault;
		$this->forceDefault = $forceDefault;

		$this->control = Html::el('');
		$this->rules = new Nette\Forms\Rules($this);
	}



	/**
	 * @param callable $factory
	 */
	public function setFactory($factory)
	{
		$this->factoryCallback = callback($factory);
	}



	/**
	 * Magical component factory
	 *
	 * @param \Nette\ComponentModel\IContainer
	 */
	protected function attached($obj)
	{
		parent::attached($obj);

		if (!$obj instanceof Nette\Application\Presenter) {
			return;
		}

		$this->loadHttpData();
		$this->createDefault();
	}



	/**
	 * @param boolean $recursive
	 * @return \ArrayIterator|\Nette\Forms\FormContainer[]
	 */
	public function getContainers($recursive = FALSE)
	{
		return $this->getComponents($recursive, 'Nette\Forms\FormContainer');
	}



	/**
	 * @param boolean $recursive
	 * @return \ArrayIterator|Nette\Forms\SubmitButton[]
	 */
	public function getButtons($recursive = FALSE)
	{
		return $this->getComponents($recursive, 'Nette\Forms\ISubmitterControl');
	}



	// -- implementst IFormControl --



	/**
	 * Returns control's HTML element template.
	 * @return Nette\Web\Html
	 */
	final public function getControlPrototype()
	{
		return $this->control;
	}



	/**
	 * Generates control's HTML element.
	 * @return Nette\Web\Html
	 */
	public function getControl()
	{
		$this->setOption('rendered', TRUE);
		$control = clone $this->control;
		return $control;
	}



	/**
	 * Generates label's HTML element.
	 * @param  string
	 * @return NULL
	 */
	public function getLabel($caption = NULL)
	{
		return null;
	}



	/**
	 * Returns control's value.
	 * @return list of struct
	 */
	function getValue()
	{
		$values = array();
		foreach ($this->getContainers() as $row) {
			$values[] = $row->getValues();
		}
		return $values;
	}



	/**
	 * Sets control's value.
	 * @param  mixed
	 * @return void
	 */
	function setValue($value)
	{
		$this->setValues($value);
		return $this;
	}



	/**
	 * @return Rules
	 */
	final public function getRules()
	{
		return $this->rules;
	}



	/**
	 * Returns errors corresponding to control.
	 * @return array
	 */
	function getErrors()
	{
		return [];
	}



	/**
	 * Returns user-specific options.
	 * @return array
	 */
	final public function getOption($key, $default = NULL)
	{
		return isset($this->options[$key]) ? $this->options[$key] : $default;
	}



	/**
	 * Sets user-specific option.
	 * Common options:
	 * - 'rendered' - indicate if method getControl() have been called
	 * - 'required' - indicate if ':required' rule has been applied
	 * - 'description' - textual or Html object description (recognized by ConventionalRenderer)
	 * @param  string key
	 * @param  mixed  value
	 * @return FormControl  provides a fluent interface
	 */
	public function setOption($key, $value)
	{
		if ($value === NULL) {
			unset($this->options[$key]);

		} else {
			$this->options[$key] = $value;
		}
		return $this;
	}



	/**
	 * Is control disabled?
	 * @return bool
	 */
	function isDisabled()
	{
		return FALSE;
	}



	/**
	 * Returns translated string.
	 * @param  string
	 * @param  int      plural count
	 * @return string
	 */
	public function translate($s, $count = NULL)
	{
		return $s;
	}



	/**
	 * Magical component factory
	 *
	 * @param string $name
	 * @return \Nette\Forms\FormContainer
	 */
	protected function createComponent($name)
	{
		$container = $this->createContainer($name);
		$container->currentGroup = $this->currentGroup;
		$this->addComponent($container, $name, $this->getFirstControlName());

		$this->factoryCallback->invoke($container);

		return $this->created[$container->name] = $container;
	}



	/**
	 * @return string
	 */
	private function getFirstControlName()
	{
		$controls = iterator_to_array($this->getComponents(FALSE, 'Nette\Forms\IFormControl'));
		$firstControl = reset($controls);
		return $firstControl ? $firstControl->name : NULL;
	}



	/**
	 * @param string $name
	 *
	 * @return \Nette\Forms\FormContainer
	 */
	protected function createContainer($name)
	{
		$class = $this->containerClass;
		return new $class();
	}



	/**
	 * @return boolean
	 */
	public function isSubmittedBy()
	{
		if ($this->submittedBy) {
			return TRUE;
		}

		foreach ($this->getButtons(TRUE) as $button) {
			if ($button->isSubmittedBy()) {
				return $this->submittedBy = TRUE;
			}
		}

		return FALSE;
	}



	/**
	 * Create new container
	 *
	 * @param string|int $name
	 *
	 * @throws \InvalidArgumentException
	 * @return \Nette\Forms\FormContainer
	 */
	public function createOne($name = NULL)
	{
		if ($name === NULL) {
			$names = array_keys(iterator_to_array($this->getContainers()));
			$name = $names ? max($names) + 1 : 0;
		}

		// Container is overriden, therefore every request for getComponent($name, FALSE) would return container
		if (isset($this->created[$name])) {
			throw new \InvalidArgumentException("Container with name '$name' already exists.");
		}

		return $this[$name];
	}



	/**
	 * @param array|\Traversable $values
	 * @param bool $erase
 	 * @param bool $onlyDisabled
	 * @return \Nette\Forms\FormContainer|Replicator
	 */
	public function setValues($values, $erase = FALSE, $onlyDisabled = FALSE)
	{
		if (!$this->form->isAnchored() || !$this->form->isSubmitted()) {
			foreach ($values as $name => $value) {
				if ((is_array($value) || $value instanceof \Traversable) && !$this->getComponent($name, FALSE)) {
					$this->createOne($name);
				}
			}
		}

		return parent::setValues($values, $erase, $onlyDisabled);
	}



	/**
	 * Loads data received from POST
	 * @internal
	 */
	/*protected*/ function loadHttpData()
	{
		if (!$this->getForm()->isSubmitted()) {
			return;
		}

		foreach ((array) $this->getHttpData() as $name => $value) {
			if ((is_array($value) || $value instanceof \Traversable) && !$this->getComponent($name, FALSE)) {
				$this->createOne($name);
			}
		}
	}



	/**
	 * Creates default containers
	 * @internal
	 */
	protected function createDefault()
	{
		if (!$this->createDefault) {
			return;
		}

		if (!$this->getForm()->isSubmitted()) {
			foreach (range(0, $this->createDefault - 1) as $key) {
				$this->createOne($key);
			}

		} elseif ($this->forceDefault) {
			while (iterator_count($this->getContainers()) < $this->createDefault) {
				$this->createOne();
			}
		}
	}



	/**
	 * @param string $name
	 * @return array|null
	 */
	protected function getContainerValues($name)
	{
		$post = $this->getHttpData();
		return isset($post[$name]) ? $post[$name] : NULL;
	}



	/**
	 * @return mixed|NULL
	 */
	private function getHttpData()
	{
		if ($this->httpPost === NULL) {
			$path = explode(self::NAME_SEPARATOR, $this->lookupPath('Nette\Forms\Form'));
			$this->httpPost = Nette\ArrayTools::get($this->getForm()->getHttpData(), $path, NULL);
		}

		return $this->httpPost;
	}



	/**
	 * @internal
	 * @param \Nette\Web\HttpRequest $request
	 * @return Replicator
	 */
	public function setRequest(Nette\Web\HttpRequest $request)
	{
		$this->httpRequest = $request;
		return $this;
	}



	/**
	 * @return \Nette\Web\HttpRequest
	 */
	private function getRequest()
	{
		if ($this->httpRequest !== NULL) {
			return $this->httpRequest;
		}

		return $this->httpRequest = $this->getForm()->getHttpRequest();
	}



	/**
	 * @param \Nette\Forms\FormContainer $container
	 * @param boolean $cleanUpGroups
	 *
	 * @throws \InvalidArgumentException
	 * @return void
	 */
	public function remove(FormContainer $container, $cleanUpGroups = FALSE)
	{
		if (!$container->parent === $this) {
			throw new \InvalidArgumentException('Given component ' . $container->name . ' is not children of ' . $this->name . '.');
		}

		// to check if form was submitted by this one
		foreach ($container->getComponents(TRUE, 'Nette\Forms\ISubmitterControl') as $button) {
			/** @var \Nette\Forms\SubmitButton $button */
			if ($button->isSubmittedBy()) {
				$this->submittedBy = TRUE;
				break;
			}
		}

		/** @var \Nette\Forms\Controls\BaseControl[] $components */
		$components = $container->getComponents(TRUE);
		$this->removeComponent($container);

		// reflection is required to hack form groups
		$groupRefl = Nette\Reflection\ClassReflection::from('Nette\Forms\FormGroup');
		$controlsProperty = $groupRefl->getProperty('controls');
		$controlsProperty->setAccessible(TRUE);

		// walk groups and clean then from removed components
		$affected = array();
		foreach ($this->getForm()->getGroups() as $group) {
			/** @var \SplObjectStorage $groupControls */
			$groupControls = $controlsProperty->getValue($group);

			foreach ($components as $control) {
				if ($groupControls->contains($control)) {
					$groupControls->detach($control);

					if (!in_array($group, $affected, TRUE)) {
						$affected[] = $group;
					}
				}
			}
		}

		// remove affected & empty groups
		if ($cleanUpGroups && $affected) {
			foreach ($this->getForm()->getComponents(FALSE, 'Nette\Forms\FormContainer') as $container) {
				if ($index = array_search($container->currentGroup, $affected, TRUE)) {
					unset($affected[$index]);
				}
			}

			/** @var \Nette\Forms\FormGroup[] $affected */
			foreach ($affected as $group) {
				if (!$group->getControls() && in_array($group, $this->getForm()->getGroups(), TRUE)) {
					$this->getForm()->removeGroup($group);
				}
			}
		}
	}



	/**
	 * Counts filled values, filtered by given names
	 *
	 * @param array $components
	 * @param array $subComponents
	 * @return int
	 */
	public function countFilledWithout(array $components = array(), array $subComponents = array())
	{
		$httpData = array_diff_key((array)$this->getHttpData(), array_flip($components));

		if (!$httpData) {
			return 0;
		}

		$rows = array();
		$subComponents = array_flip($subComponents);
		foreach ($httpData as $item) {
			$filter = function ($value) use (&$filter) {
				if (is_array($value)) {
					return count(array_filter($value, $filter)) > 0;
				}
				return strlen($value);
			};

			if (is_array($item)) {
				$rows[] = array_filter(array_diff_key($item, $subComponents), $filter) ?: FALSE;
			}
		}

		return count(array_filter($rows));
	}



	/**
	 * @param array $exceptChildren
	 * @return bool
	 */
	public function isAllFilled(array $exceptChildren = array())
	{
		$components = array();
		foreach ($this->getComponents(FALSE, 'Nette\Forms\IControl') as $control) {
			/** @var \Nette\Forms\Controls\BaseControl $control */
			$components[] = $control->getName();
		}

		foreach ($this->getContainers() as $container) {
			foreach ($container->getComponents(TRUE, 'Nette\Forms\ISubmitterControl') as $button) {
				/** @var \Nette\Forms\SubmitButton $button */
				$exceptChildren[] = $button->getName();
			}
		}

		$filled = $this->countFilledWithout($components, array_unique($exceptChildren));
		return $filled === iterator_count($this->getContainers());
	}



	/**
	 * @param $name
	 * @return \Nette\Forms\FormContainer
	 */
	public function addContainer($name)
	{
		return $this[$name] = new FormContainer;
	}



	/**
	 * @param \Nette\IComponent $component
	 * @param string $name
	 * @param null $insertBefore
	 * @return \Nette\Forms\FormContainer
	 */
	public function addComponent(Nette\IComponent $component, $name, $insertBefore = NULL)
	{
		$group = $this->currentGroup;
		$this->currentGroup = NULL;
		parent::addComponent($component, $name, $insertBefore);
		$this->currentGroup = $group;
		return $this;
	}



	/**
	 * @var bool
	 */
	private static $registered = FALSE;

	/**
	 * @param string $methodName
	 * @return void
	 */
	public static function register($methodName = 'addDynamic')
	{
		if (self::$registered) {
			FormContainer::extensionMethod(self::$registered, function () {
				throw new \MemberAccessException;
			});
		}

		FormContainer::extensionMethod($methodName, function (FormContainer $_this, $name, $factory, $createDefault = 0, $forceDefault = FALSE) {
			$control = new Container($factory, $createDefault, $forceDefault);
			$control->currentGroup = $_this->currentGroup;
			return $_this[$name] = $control;
		});

		if (self::$registered) {
			return;
		}

		SubmitButton::extensionMethod('addRemoveOnClick', function (SubmitButton $_this, $callback = NULL) {
			$_this->setValidationScope(FALSE);
			$_this->onClick[] = function (SubmitButton $button) use ($callback) {
				$replicator = $button->lookup(__NAMESPACE__ . '\Container');
				/** @var Container $replicator */
				if (is_callable($callback)) {
					callback($callback)->invoke($replicator, $button->parent);
				}
				if ($form = $button->getForm(FALSE)) {
					$form->onSubmit = array();
				}
				$replicator->remove($button->parent);
			};
			return $_this;
		});

		SubmitButton::extensionMethod('addCreateOnClick', function (SubmitButton $_this, $allowEmpty = FALSE, $callback = NULL) {
			$_this->onClick[] = function (SubmitButton $button) use ($allowEmpty, $callback) {
				$replicator = $button->lookup(__NAMESPACE__ . '\Container');
				/** @var Container $replicator */
				if (!is_bool($allowEmpty)) {
					$callback = callback($allowEmpty);
					$allowEmpty = FALSE;
				}
				if ($allowEmpty === TRUE || $replicator->isAllFilled() === TRUE) {
					$newContainer = $replicator->createOne();
					if (is_callable($callback)) {
						callback($callback)->invoke($replicator, $newContainer);
					}
				}
				$button->getForm()->onSubmit = array();
			};
			return $_this;
		});

		self::$registered = $methodName;
	}

}
