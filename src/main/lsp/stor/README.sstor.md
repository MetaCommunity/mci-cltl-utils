[LTP sstor] Local Storage and Retrieval of Symbolic Objects
===========================================================


## Design - Overview

[Prototype System]

### Design - Overview - Usage Cases

- **enum** - Local storage and retrieval of similarly typed objects via
  a single storage object, per object name values assumed unique onto
  the storage object.

- **table** - Local storage and retrieval of similarly typed objects,
  using arbitrary indexing and query-like semantics assumed constant
  onto a single storage object.

- **linking** - Local storage, retrieval, and traversal of similarly
  typed objects, with a semantics of traversal assumed constant for one
  or more of a _linking domain_ defined onto the set of objects.

## Summary of Implementation - Notes

### Enum Objects - Notes

* Juxtaposed to some hash table implementations, this implementation
  utilizes a single storage vector for each enumeration (enum)
  container. In some further details, the storage vector may be
  implemented as a Common Lisp adjustable array, in a fairly simple
  methodology for local object storage.

* Provides API definitions and extensible protocol classes for static and
  dynamic enum containers, interoperable with CLOS

* Approximately synonymous concepts: Object name; Object key; Object
  descriptor


### Table Objects - Notes

* May interoperate with external object systems

* May provide a backing-storage semantics for items in indexed tables,
  in local object storage and access (string checksum, other)

* May provide an ordered indexing semantics, for sequentially indexed
  tables in local object storage and access

* Prototype: Local storage of XML DOM node tables (prerequisites may
  include: XML data type model)

* Prototype: Local access onto arbitrary data storage, via typically
  SQL-oriented APIs (prerequisites may include: SQL data type model)


### Linked Objects - Notes

* Concept: Linking Domain - towards a generalized functional semantics
  for definition and traversal of associative references across a set of
  _Linked Objects_

* Topic: Knowledge Representation in Software Program Definitions.

* Topic: RDF as declarative notation for specification and generalized
  application of associative object references, generally in a form of
  RDF graphs. See also: Redland project - librdf, raptor, and rasqal
  APIs; XML Schema support in libxml2.

* Topic: Knowledge Representation in Software Documentation Systems.

* Topic: Knowledge Representation in Arbitrary Knowledgebase Systems.


### Storage Objects - General Considerations - Notes

* Storage Domain as a first-order object

    * NB: An implementation of this concept may be shared across
      individual protocols, in this system

    * Topic: Definition and Application of _Storage Domain Metaclass_
      Objects - applications for specialization of polymorphic functions
      for storage allocation, storage initialization, storage access,
      storage management, and deallocation of ephemeral storage
      objects.

    * Functional definitions may include:

        * STORAGE-RUNTIME-STATE (??) (NB: This may be, in effect, "Hard
          wired" into any single storage object protocol. In any manner
          extensional to each application protocol, as such, any
          information about storage Runtime state may be of use mostly
          for informational purposes, such as in storage management
          applications)

        * INITIALIZE-STORAGE and/or ENSURE-STORAGE - Generalized
          functional protocol for storage initialization, for purpose of
          application, in a manner principally extensional to any
          program-level initialization for storage objects.

        * CLOSE-STORAGE - Generalized functional protocol for management
          of _reference deallocation_ in storage objects.

    * Protocol Class Definitions may include:

        * STORAGE (??)

        * STORAGE-DOMAIN (??)

    * Initial testing

        - Trivial Enum Prototype

        - External Tables - MariaDB API for MariaDB Embeddded Databases
          & SQL Type Systems

        - External Tables - UNIX ODBC for MariaDB Embeddded Databases &
          SQL Type Systems

        - External Linked Objects - Data object initialization and
          filesystem storage for editorial/bibliographical information
          systems in arbitrary knowledgebases

            - NB: MARC; TeX, Hyperref, and BibTeX; RDA; FRBR and CIDOC
              CRM.

            - Storage Synchronization Protocols onto e.g:

                - Arbitrary Transport Protocols
                    - e.g w/ Private Zotero Storage : SQL, via UNIX
                      domain socket or w/ KRB5 and local INET domain
                      socket (NB sqlite)

                - Arbitrary Granularity of Model Entities
                    - Annotation (Resource Map and Annotation Entities),
                      Bibliographical Terms (cf. CIDOC CRM and site
                      taxonomy definitions), Bibliographical File
                      Resources, Reference Definitions, Editorial Terms,
                      Editorial Content Resources, and Provenance
                      metadata e.g

                - Arbitrary local storage
                    - Ephemeral storage access (vis. proxy) service e.g

        - Knowledge Representation for Systems Management

    * DEVO Tools Integration TBD

* Entity Model as a first-order object

* Linking Domain as a first-order object, closed onto any one set of
  Storage Domain and Entity Model Definition

* May interoperate with external object systems (static and dynamic
  external storage)

* Template-oriented protocol

* See also: Common Logic (ISO CL) CLIF, CGIF notations; OMG XMI
  notation, as a transport encoding -- in XML syntax -- for model
  descriptions using a notation of OMG MOF, OMG UML, and/or OMG OCL


### TBD - API Design and Implementation - Notes


* [LTP sstor] and generalized namespace schema

    * `ensure-namespace`, `find-namespace`, ...

    * `(import object &optional namespace)` with implementation per
      `(object, namespace)`, dispatching to `(import-object object namespace)`


* [LTP sstor] and arbitrary object modeling systems


* [LTP sstor] and type schema - NB: ASN.1 notation as a generally
  portable notation for definition of type schema for arbitrary
  applications.


* [LTP sstor] and functional protocol descriptions for storage, query,
  and reference onto arbitrary information sets.


* Extensions for arbitrary modeling systems - common modeling protocol
  and implementations.


* Extension for systems management protocols - common systems management
  protocols and implementations.


<!-- LocalWords: APIs ASN CGIF CLIF CLOS DOM enum LTP MOF Metaclass OCL -->
<!-- LocalWords: OMG Simula TBD UML XMI enum interoperable interoperate -->
<!-- LocalWords: modl namespace sstor RDF Redland librdf rasqal CRM -->
<!-- LocalWords:  libxml Knowledgebase deallocation RUNTIME Runtime -->
<!-- LocalWords:  Embeddded ODBC filesystem knowledgebases Hyperref -->
<!-- LocalWords:  BibTeX FRBR CIDOC Zotero KRB INET sqlite vis DEVO -->
