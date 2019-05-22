
(defmacro eval-when-test ((&rest specifers) &body body
                                              &enviroment environment)

  ;; Synopsis / Overview:
  ;;  Generalization of eval-when semmantics onto, in manner, "DevOps" Processes
  ;;
  ;; Synopsis / Application:
  ;;  Provide a simple semantics for developing "Inline Test" forms
  ;;  within Common Lisp source files. Provide a structural method for
  ;;  application of inline tests during normal software system build
  ;;  processes. May be integrated with pkgsrc port build automation [pkgsrc]
  ;;
  ;; Notes
  ;;  - pkgsrc mk-files - Ports mk-files - TEST_DEPENDS
  ;;  - pkgsrc build sytstem parameters - PKGSRC_RUN_TEST
  ;;  - pkgsrc mk-files - pkgsrc 'test' Makefile target
  ;;  - pkgsrc guide [pkgsrc] edn. 2017/07/30
  ;;    - s. 17.13, "The test phase" - denoted "TODO"
  ;;    - s. III, "The pkgsrc infrastructure internals"
  ;;      - s. 25, "Regression tests" - per the pkgtools/pkg_regress port
  ;;        - NB port pkgtools/pkg_regress; test categorization in pkgsrc
  ;;          (and/or for the pkg_regress port); "regress" test category
  ;;    - See also: FreeBSD port mk-files
  )

;; TBD:
;; - Test Specifiers - Syntax and Application Behaviors
;;   - Test Naming
;;   - Test Grouping
;;   - Test Evaluation Constraints
;;     - Code Evaluation Phase (onto ANSI CL vis. standard syntax and
;;       behaviors of CL:EVAL-WHEN)
;;     - Test Code Constraint onto Dependent Tests
;;       (when-success/when-fail)
;;   - Test Component Dependencies
;;     - "Test Harness" Intrgration onto ASDF - Dependency Management?
;; - Test Definition, Eval, and Reporting - Procedural Model
;;   - This topic representing the "Bread and Butter" of the "Test
;;     Harness," in applications
;;   - Test Definition
;;     - Inline Test Definition
;;     - Test Defintion in External File
;;     - Test Definition in Runtime Application Code Generation
;;     - "Test Harness" Intrgration onto ASDF - Test Declaration in
;;       System Definition
;;   - Test Orchestration
;;     - "Test Harness" Intrgration onto ASDF - ASDF:TEST-OP
;;     - Test Activation
;;     - Test Monitoring
;;   - Test Reporting
;;     - Report Generation
;;     - Report Publishing
;;
;;   - NST - API; Ports

