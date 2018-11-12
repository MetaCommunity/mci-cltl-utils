
(defmacro eval-when-test ((&rest specifers) &body body
                                              &enviroment environment)
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
