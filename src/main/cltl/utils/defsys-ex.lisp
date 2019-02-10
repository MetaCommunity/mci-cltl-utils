;; defsys-ex.lisp
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial Implementation
;;
;;------------------------------------------------------------------------------

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:info.metacommunity.cltl.defsys
    (:nicknames #:defsys)
    (:use #:asdf #:cl)))


(in-package #:defsys)

;; A few thoughts towards an alternate DEFSYSTEM macro, DEFSYS
;;
;; * Dependency paths
;;
;;     * "Opaque system" and "Component tree" views of an ASDF system
;;       as with regards to system dependencies
;;
;;        * Example for a "Component tree" view: Garnet Gadgets
;;            * Garnet Gadgets may be as represented by their definitive
;;               KR schema and by their definitions as defined in
;;               correlated source files
;;
;;            * The Garnet-Gadgets system defines an abstract container
;;              for Garnet Gadgets source files
;;
;;            * An application may depend, directly, on only a subset
;;              of the full set of Garnet Gadgets
;;
;;            * An embedded application "Should not" be required to
;;              load the entire Garnet Gadget system, for utilizing
;;              only a small number of Garnet Gadgets, directly
;;
;;  * Defining a more CLOS-centric protocol for system definition
;;
;;      * ASDF:DEFSYSTEM is a macro with a specific syntax
;;        and specific behaviors, some of which may not be
;;        expected by system developers
;;
;;         * e.g. ASDF DEFSYSTEM [ASDF 3.1.3.8] effectively
;;           overrides some extensions onto component pathname handling
;;
;;             * Case in point:
;;                 SHARED-INITIALIZE :AROUND GARNET-SYSTEMS:CURSOR T
;;                   &REST INITARGS &KEY &ALLOW-OTHER-KEYS
;;
;;                 * {refer to source code}
;;
;;                 * Observation: One of an "only way" for the _CURSOR pathname
;;                   defaulting_ to be exactly implemented is by way
;;                   of "hacking" the INITARGS in SHARED-INITIALIZE
;;                   :AROUND, before CALL-NEXT-METHOD
;;
;;      * Every procedure implemented in the macroexpansion of
;;        ASDF:DEFSYSTEM may alternately be implemented within
;;        a method specialized onto a generic function, in defining
;;        an exacting object system protocol for system definition
;;
;;  * IDE integration
;;
;;      * Concept: IDE-supported interaction with functional testing
;;        frameworks
;;
;;          * Example: jUnit support in the Eclipse IDE
;;
;;          * Concept: Extending onto the hypothetical CORBA for
;;            Remote Common Lisp framework, with extensions for
;;            defining test suites, etc, and their relations to
;;            specific source code resources, in IDL and otherwise
;;
;;      * Concept: IDE-driven controls for advancing a codebase from
;;        "testing" to "release"
;;
;;          * Sidebar: The Eclipse Mylyn framework and 'QA'
;;
;;      * Prerequisite: A consistent concept of an IDE in Common Lisp
;;
;;          * Related concept: The SWANK protocol
;;
;;              * Possibly related concept: CORBA
;;
;;                  * A set of IDL interface definitions may be
;;                    defined around the SWANK protocol, as for
;;                    purpose of defining a CORBA interface
;;                    for program interaction with a  Common Lisp
;;                    implementation via an Object Request Broker
;;                    (ORB)
;;
;;              * Possibly related concept: The Dandelion plugin
;;                for the Eclipse IDE
;;
;;                  * Dandelion may provide a helpful baseline for
;;                    an extension implementing the same Lisp ORB
;;                    Interaction model, in the Eclipse IDE
;;
;;                  * Whereas Dandelion is a plugin for the Eclipse
;;                    IDE, the Eclipse IDE itself would serve to
;;                    provide a reference framework, in addressing
;;                    questions such as: How to define an IDL
;;                    interface by which a Common Lisp editing
;;                    platform -- if not a formal, Common Lisp
;;                    application debugger (FOSS) for "production
;;                    applications" [!!] -- may be able to "Browse the
;;                    source location" of the definition of a
;;                    function, class, or other Common Lisp object
;;                    defined in an exacting source file?
;;
;;                      * Towards that effect: If the source file is
;;                        available on the same network as the
;;                        connecting debugger application, the source
;;                        file may be represented -- via the
;;                        respective IDL implementation -- with a URL
;;                        denoting the source file's location.
;;
;;                      * The Eclipse IDE offers a "remote servers"
;;                        framework, such that could be extended onto,
;;                        in facilitating the remote access for Common
;;                        Lisp application debugging.
;;
;;                  * "This project", then, may present an
;;                    opportunity towards extending Dandelion for
;;                    integrating the Eclipse IDE with Common Lisp
;;                    debuggers, in any Common Lisp implementation
;;                    that has a reference interface already defined
;;                    in SLIME/Swank. (Of course, it would also require
;;                    an implementation of the IDL interface
;;                    definitions within the respective Common Lisp
;;                    implementation - towards which effect, the
;;                    SLIME/SWANK source code would likewise serve as a
;;                    reference, then leaving a simple question of how
;;                    best to define an interface to an object request
;;                    broker, in Common Lisp?)
;;
;;                      * Additional concern: Authentication via CORBA
;;
;;                          * Additional concern: Integration with
;;                            Kerberos and SSH
;;
;;                          * Possible approach: FFI onto The ACE ORB
;;
;;                          * Another possible approach: FFI onto
;;                            JacORB -- possibly an ideal approach,
;;                            from the perspective of developing a
;;                            combined systems platform in Java and
;;                            Common Lisp, although FFI onto Java may
;;                            not be feasible by all approaches, in
;;                            all Common Lisp implementations
;;                            (referencing: concerns about interrupt
;;                            handling in SBCL, such that effectively
;;                            interferes CL+J in SBCL)
;;
;;                          * Another possible approach: Update and
;;                            extend CLORB
;;
;;                              * May be ideal, as with regards to
;;                                limiting Common Lisp applications'
;;                                dependencies onto an external JVM
;;
;;                              * May seem like an arduous task,
;;                                superficially, compared to an idea
;;                                of "implementing FFI" onto an
;;                                existing ORB implementation such that
;;                                supports both of "IIOP over SSH" and
;;                                Kerberos authentication, already,
;;                                namely JacORB. Of course, that may
;;                                also serve to require FFI interfaces
;;                                onto a Kerberos implementation and
;;                                OpenSSL.
;;
;;                              * In a sense of "Software reusability"
;;                                beyond specific programming language
;;                                platforms, the "Use FFI" idea may be
;;                                ideal.
;;
;;              * Possibly related concept (in a far reach) :
;;                Mobile/embedded computing, desktop virtualization
;;                architectures, and CORBA
;;
;;                  * XLIB protocol as a reference framework, in
;;                    context with the X Window System
;;
;;                  * DXPC as an extensional X Window System protocol
;;
;;                  * NoMachine's NX architecture - a combination of
;;                    open source (e.g nxcodec) and proprietary
;;                    (e.g. commercial support) components
;;
;;                  * Citrix Receiver and Xen
;;
;;                  * Sidebar: Xen and Microkernel architectures
;;
;;              * Possibly related concept (in a further reach) :
;;                 Realtime CORBA as a protocol for inter-application
;;                 communications within an embedded computing
;;                 platform
;;                  * May be developed to prototype, on an Android
;;                    platform and/or other ARM architectures
;;                    e.g. Samsung Chromebook w/ Crouton or CruBuntu



;;; * [Sidebar] RESOURCE-SYSTEM

(defclass resource (component)
  ())

(defclass resource-file (resource static-file)
  ())

(defclass resource-module (resource module)
  ()
  (:default-initargs
   :default-component-class 'resource))

(defclass resource-system (resource system)
  ())

(defmethod source-file-type ((component resource)
			     system)
    (values nil))


;;; * ...

#| [Sidebar] Initial prototype for a URI format for dependency specifiers

 format: urn:x-dep:<app>:<app-specific-part>

    for <app> ::= maven
          Context: ABCL / CL+J / FOIL / ...
          The <app-specific-part> must constitute a

    for <app> ::= ivy
          Context: ABCL / CL+J / FOIL / ...
          (Specification TBD) (TO DO: Study Ivy documentation)

    for <app> ::=  deb
          Context: Host OS packaging system (Debian)
          <app-specific-part> must be (TO DO) of a format to identify,
          at minimum, a Debian package -- optionally, with a
          dependency range specifier (c.f Debian packaging system
          'control' files) and a distribution-specific architecture
          specifier (TO DO: reference the standard distribution
          specifiers for Debian and Ubuntu distros, here)

          e.g
             urn:x-dep:deb:sbcl:[1.1.14]:[1.2.3]
              denotes 'sbcl' package, host architecture, 1.1.4 <= version <= 1.2.3

             urn:x-dep:deb:sbcl:1.1.14:1.2.3:ia32
              denotes 'sbcl' package, 'ia32' architecture, 1.1.4 < version < 1.2.3

             urn:x-dep:deb:sbcl:1.2.3::amd64
              denotes 'sbcl' package, 'amd64' architecture, version = 1.2.3

           TO DO: Make reference to {that effective standard for
           version specifier strings} and define behaviors for version
           string sort order, in "this protocol"

    for <app> ::= ASDF
          Context: Common Lisp systems (typically, Common Lisp software code)
          <app-specific-part> would denote .... ?

          Thoughts:

            0. Generic instance: <app-specific-part> would denote a
               'path' to an ASDF component, e.g.

                 "urn:x-dep:asdf:clim-gtkairo:Backends/gtkairo;ffi"

           1. Concern: Verbosity of dependency specifiers within
              a context of ASDF system definitions

    Inevitable sidebar: Integrating a Maven repository into ASDF
     or, extending ASDF::*SOURCE-REGISTRY* onto Apache Maven

            1. Using ASDF to load a system definition directly from an
               HTTP server?

                    * Concern: Checksums

            2. Using ASDF to unbundle and load a system definition
               provided within a Maven archive (ZIP format) => !

                    * Would require a corresponding specification for
                      a method for including ASDF system definitions
                      within Maven archives (TBD)

                    * Advantage: "This approach" would serve to extend
                      of the Maven resource distribution architecture
                      (alternately Apache Ivy), likewise extending of
                      Maven's support for HTTP authentication,
                      resource repository selection, resource
                      bundle [JAR and POM] checksums, "and so on"
|#


(defgeneric dependency-object (dependency))

(defgeneric dependency-subject (dependency))

(defclass dependency ()
  ;; an instance of this class effectively serves as a predicate in an
  ;; expression -  for SYSTEM A, COMPONENT B, AND DEPENDENCY D
  ;;
  ;;   {A D B}
  ;;
  ;; denoting:
  ;;   COMPONENT (subject) 'A'
  ;;    depends on OBJECT 'B'
  ;;    by way of DEPENDENCY D
  ;;
  ((object
    ;; the object of the dependency - conventionally a component
    ;; designator, a "must exist" feature type designator, or a module
    ;; type feature designator
    :initarg :object :accessor dependency-object)
   (subject
    ;; i.e. the component that depends on the specified OBJECT
    :initarg :subject
    :accessor dependency-subject
   )))


(defclass versioned-dependency ()
  ;; In some terms - as  perhaps may be reminiscent of the Web Ontology
  ;; Language [OWL] - a VERSIONED-DEPENDENCY effectively
  ;; provides an annotation to a DEPENDENCY D
  ;;
  ;;   {A D B}
  ;;
  ;; ...in specifying one or both of "minimum version" and a "maximum
  ;; version" required for the component B in dependency D, such that
  ;; the version specifiers would be interpreted in a context relevant
  ;; for the effective type of the object B
  ;;
  ;; For purpose of convenience in this application, a semantics
  ;; extending of ASDF is applied instead of a semantics extending of
  ;; the Resource Description Framework [RDF] for dependency version
  ;; annotation
  ())

;;;; Dependency classes for dependency specifiers normative in ASDF,
;;;  referencing the ASDF manual

(defclass component-dependency (dependency)
  ((object
    :type string)))

(defclass versioned-component-dependency (versioned-dependency
					  component-dependency)
  ())

(defclass feature-dependency (dependency)
  ((object
    :type symbol
    :accessor dependency-feature)))


(defclass module-dependency (feature-dependency)
  ;; for ASDF :REQUIRE dependency specifiers
  ())

;;;; Additional dependency types (TBD)

(defclass file (dependency)
  ())

(defclass executable-file (file)
  ;; for specifying dependencies onto shell commands
  ())

(defclass host-package (dependency)
  ;; for specifying dependencies onto a host packaging system
  ;; e.g.
  ;;   Debian packaging
  ;;      e.g. dpkg-query -f '${db:status-abbrev}${binary:package} ${version}\n' -W pngcrush
  ;;      ....expecting only one package entry listed
  ;
  ;;   Cygwin (e.g. "cygcheck -c pngcrush")
  ;;
  ())

(defclass debian-package (host-package)
  ())

(defclass yum-package (host)
  ())

(defclass cygwin-package (host-package)
  ())

;; To Do: HOST-PACKAGE subclass for Gentoo?





(defgeneric ensure-dependency (name body)
  (:method ((name (eql :path)) (body cons))
    ;; evaluate BODY as a path to a component's subcomponent
    )
  (:method ((name string) (body null))
    ;; evaluate NAME as a system name
    )
  #+TO-DO
  (:method ((name iri) (body null))
    ;; load a "remote" system definition denoted at the URI <IRI>
    ;;


    ;;  Prerequisites:
    ;;   1. IRI handling protocol for application
    ;;        suggestion: Similar to the KDE KIO framework,
    ;;           dispatch on IRI scheme
    ;;        concern: Semantics and application of a resource proxy
    ;;                 framework for the underlying Lisp implementation
    ;;   2. Implementation of IRI handling protocol
    ;;      for all supported IRI
    ;;
    ;;  FIXME after nr. 2: Rename IRI specializer to e.g. HTTP-URI
    )

  (:method ((name (eql :os-package)) body)
    ;; initialize  as an instance of a subclass of HOST-PACKAGE
    ;; specific to the host OS
    ;;
    ;; TO DO: Protocol for determining the identity of the host OS
    ;;  FIRST: `uname -o' if available
    ;;   if "GNU/Linux" case: then also (e.g) 'lsb_release -a'
    ;;   if 'Cygwin' case: ???
    ;;
    ;; NOTE: A dependency onto a Debian package may be denoted
    ;; with a architecture specifier , explicitly:
    ;;       e.g amd64 or ia32
    ;; or else the architecture defaults to that of ... a
    ;; distribution-specific architecture designator extending of
    ;;   `uname -m' (unofficial) .e.g
    ;;          x86_64 [kernel machine type] =i.e.=> amd64 [host OS architecture]
    )


  ;; next are for compat with ASDF:DEFSYSTEM
  ;; cf. ASDF/PARSE-DEFSYSTEM::PARSE-DEPENDENCY-DEF
  ;;

  (:method ((name (eql :feature) body))
    ;; Nb. Semantically similar to a :REQUIRE dependency
    ;; Intialize as FEATURE-DEPENCENCY
    )
  (:method ((name (eql :require) body))
    ;; initialize as MODULE-DEPENDENCY
    )
  (:method ((name (eql :version)) body)
    ; Nb. initialize as  VERSIONED-COMPONENT-DEPENDENCY
    ))


(defun parse-dependency-speciifer (spec)
  (etypecase spec
    (cons (destructuring-bind (name &rest args) spec
	    (ensure-dependency name arts)))
    (t (ensure-dependency (coerce-name spec)
			  nil))))


#+NIL
(defsys #:a
  :components
  ((utils:resource "a.1")))

#+NIL
(defsys #:b
    :depends-on ((:path "a" "a.1")))



#+NIL
(defsystem #:c
  :pathname #.*default-pathname-defaults*
  :components
  ((resource "c.1")
   (resource "c.2"
	     :depends-on ("c.1"))))

;; (describe (find-component* "c.2" "c"))
;; ^ note the asdf::sideway-dependencies slot
;; see also:
;;   ASDF:COMPONENT-SIDEWAY-DEPENDENCIES
;;   ASDF:SIDEWAY-OPERATION
;;   ASDF:PREPARE-OP



;; Local Variables:
;; ispell-buffer-session-localwords: ("ABCL" "CLORB" "CLOS" "CORBA" "Checksums" "Citrix" "CruBuntu" "Cygwin" "DEFSYSTEM" "DEPENCENCY" "DXPC" "FFI" "FIXME" "FOSS" "IDL" "IIOP" "INITARGS" "Intialize" "JVM" "JacORB" "KDE" "KIO" "Kerberos" "Microkernel" "Mylyn" "NX" "NoMachine's" "OpenSSL" "RDF" "Realtime" "SBCL" "SIDEWAY" "TBD" "URI" "XLIB" "Xen" "accessor" "amd" "args" "asdf" "centric" "checksums" "codebase" "compat" "cygcheck" "cygwin" "debian" "defclass" "defgeneric" "defmethod" "defpackage" "defsys" "defsystem" "defun" "dep" "designator" "destructuring" "distros" "dpkg" "eql" "etypecase" "eval" "ffi" "ia" "initarg" "initargs" "iri" "jUnit" "lsb" "macroexpansion" "nr" "nxcodec" "os" "pathname" "pngcrush" "reusability" "sbcl" "specializer" "speciifer" "subcomponent" "toplevel" "uname" "unbundle" "utils")
;; End:
