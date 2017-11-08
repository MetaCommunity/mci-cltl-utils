mci-cltl-utils
==============

Summary: Generic utilities for programming with Common Lisp and ASDF

Availability: [git@github.com:MetaCommunity/mci-cltl-utils.git][mci-cltl-utils]


## Reference

## `info.metacommunity.cltl.utils` [System Definition]

**Overview:** `info.metacommunity.cltl.utils` is the primary _system 
definition_ provided in the [mci-cltl-utils][mci-cltl-utils] _source
tree_.

## `info.metacommunity.cltl.utils` [Package]

**Overview:** The _package_ `info.metacommunity.cltl.utils` _exports_
_symbols_ such that denote a number of Common Lisp _forms_ defined
within the  `info.metacommunity.cltl.utils` _system_.


### Generic Utilities

#### Generic Condition Utilities

##### `simple-style-warning` [Condition Class]

**Direct Class Precedence List:**
* `style-warning`
* `simple-condition`

##### `simple-style-warning` [Macro]

##### `simple-program-error` [Condition Class]

**Direct Class Precedence List:**
* `program-error`
* `simple-condition`

##### `simple-program-error` [Macro]

#### Functional Utilities

##### `with-safe-frefs` [Macro]


### Utilities for ASDF Systems

#### Type Specifiers

##### `component-designator` [Type Specifier]

**Compound Type Specifier Description:**

The _type_ `component-designator` represents the concatenation of the
types `string`, `symbol`, and `asdf:component`

#### Condition Classes

The `info.metacommunity.cltl.utils` _system_ provides a number of
_condition classes_ for use within extensions of [ASDF][asdf].

It's hoped that these _condition classes_ may serve to provide
sufficient detail for creating useful _restarts_ and other _debugger_
_interface_ elements, namely as to assist Common Lisp _program_
_developers_ and _program _users_ for resolving  _error conditions_
within Common Lisp _programs_ utilizing these extensions.

##### `component-condition` [Condition Class]

**Direct Class Precedence List:**
* `condition`

**Initargs**
* `:component`

**Accessors:**
* `component-condition-component`

##### `component-container-condition` [Condition Class]

**Direct Class Precedence List:**
* `condition`

**Initargs:**
* `:container`

**Accessors:**
* `component-container-condition-container`

##### `module-component-absent` [Condition Class]

**Direct Class Precedence List:**
* `program-error`
* `component-condition`
* `component-container-condition`


##### `system-not-found` [Condition Class]

**Direct Class Precedence List:**
* `program-error`
* `component-condition`
* `component-container-condition`

#### ASDF Component Search

##### `find-component*` [Generic Function]

**Overview:** This _generic function_ and its _primary_ _methods_, as
defined in this _system_, are defined essentially so as to provide an
interface onto each of `asdf:find-component` and `asdf:find-system`,
with detailed _condition_ handling, provided as per the _condition
classes_ defined in this system.

Exceptional situations: _Refer to source code_

##### `find-system*` [Function]

Overview: This _function_ is defined as to provide a convenient
interface onto `find-component*`, namely for locating ASDF _system
definitions_ within an active ASDF _source registry_ configuration.

Exceptional situations: _Refer to source code_


[mci-cltl-utils]: https://github.com/MetaCommunity/mci-cltl-utils
[asdf]: http://common-lisp.net/project/asdf/

<!--  LocalWords:  mci cltl utils ASDF frefs designator asdf Initargs
 -->
<!--  LocalWords:  Accessors
 -->
