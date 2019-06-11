[LTP sstor] Local Storage and Retrieval of Symbolic Objects
===========================================================


## Design - Overview

- **enum** - Local storage and retrieval of similarly typed objects, per
  locally unique object name values

- **table** - Local storage and retrieval of similarly typed objects with
  arbitrary index semantics

- **linking** - Local storage and traversal of similarly typed objects
  with arbitrary linking domains


## Summary of Implementation - Notes


### [sstor enum]

* Juxtaposed to some hash table implementations, this implementation
  utilizes a single storage vector for each enumeration (enum)
  container. In some further details, the storage vector may be
  implemented as a Common Lisp adjustable array, in a fairly simple
  methodology for local object storage.

* Provides API definitions and extensible protocol classes for static and
  dynamic enum containers, interoperable with CLOS

* Approximately synonymous concepts: Object name; Object key; Object
  descriptor


### [sstor table]

* May interoperate with external object systems

* May provide a backing-storage semantics for items in indexed tables,
  in local object storage and access (string checksum, other)

* May provide an ordered indexing semantics, for sequentially indexed
  tables in local object storage and access

* Prototype: Local storage of XML DOM node tables (prerequisites may
  include: XML data type model)

* Prototype: Local access onto arbitrary data storage, via typically
  SQL-oriented APIs (prerequisites may include: SQL data type model)


### [sstor linking]

* Storage Domain as a first-order object

    * NB: An implementation of this concept may be shared across
      individual protocols, in this system

    * Topic: Definition and Application of a Storage Domain Metaclass

    * Functional definitions to include:

        * STORAGE-RUNTIME-STATE

        * INITIALIZE-STORAGE

        * CLOSE-STORAGE

    * Protocol Class Definitions to include:

        * STORAGE

        * STORAGE-DOMAIN - see also: PROTOTYPE-CLASS [LTP singleton]

    * Initial testing

        - Trivial Enum Prototype

        - MariaDB API for MariaDB Embeddded Databases & SQL Type Systems

        - UNIX ODBC for MariaDB Embeddded Databases & SQL Type Systems

        - Data object and fileysystem storage for distributed
          editorial/bibliographical systems

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
                      Editoial Content Resources, and Provenance
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


* [LTP sstor] and generally Simula-like object systems

    * `ensure-namespace`, `find-namespace`, ...

    * `(import object &optional namespace)` with implementation per
      `(object, namespace)`, dispatching to `(import-object object namespace)`


* [LTP sstor] and arbitrary object modeling systems


* [LTP sstor] and type schema in ASN.1 notation


* [LTP sstor] and functional protocol descriptions


* Extension in [LTP modl] systems - common modeling protocol and
  implementations


* Extension in [LTP SM] systems - common systems management protocol and
  implementations


<!-- LocalWords: APIs ASN CGIF CLIF CLOS DOM enum LTP MOF Metaclass OCL -->
<!-- LocalWords: OMG Simula TBD UML XMI enum interoperable interoperate -->
<!-- LocalWords: modl namespace sstor -->
