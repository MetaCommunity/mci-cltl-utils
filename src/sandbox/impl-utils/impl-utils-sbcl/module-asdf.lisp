;; module-asdf.lisp - Provide SBCL contrib systems via ASDF, for REQUIRE
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2018 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
;;
;;------------------------------------------------------------------------------

(defun module-provide-sbcl-contrib-asdf (name &optional (noerror t))
  ;; NB: Mostly portable - should be OK to compile and load this
  ;; defun, even when ASDF is not loaded

  ;; * Overview
  ;;
  ;; This function allows for loading SBCL contrib systems via ASDF,
  ;; independent of the configuration of ASDF system paths. When an SBCL
  ;; contrib system is available as an ASDF system definition via the
  ;; logical pathname "sys:contrib;<name>;<name>.asd" then that system
  ;; definition will be loaded and the corresponding system <name> will
  ;; be used for an ASDF LOAD-OP onto the ASDF PERFORM method. The
  ;; system definition will be returned.
  ;;
  ;; Otherwise, when evaluated via SBCL REQUIRE, the value NIL will be
  ;; returned.
  ;;
  ;; This function may be evaluated independent of ASDF REQUIRE. In
  ;; such instances, the optional NOERROR argument may be provided with a
  ;; nil value. If a system definition cannot be located at the
  ;; specified pathname and NOERROR is nil, then an error of type
  ;; SIMPLE-ERROR will be be produced. If NOERROR is true - as per the
  ;; default behavior of this function - and a system definition cannot
  ;; be located at the specified pathname. then NIL will be returned.
  ;;
  ;;
  ;; * Configuration
  ;;
  ;; This function requires that the SBCL image will have been
  ;; configured for logical pathnames under "sys:contrib;" such as for
  ;; the sb-posix system, "sys:contrib;sb-posix;sb-posix.asd". The SBCL
  ;; installation may have been automatically configured for this
  ;; logical pathname, in many conventional installations.
  ;;
  ;;
  ;; * Installation
  ;;
  ;; This function may be defined within in an ASDF user-init file e.g
  ;; with a UNIX system pathname ~/.sbclrc
  ;;
  ;; In the following example, it's assumed that ASDF either has been
  ;; loaded into the running Lisp image, or may be loaded independent of
  ;; this function definition, using SBCL REQUIRE
  ;;
  ;; Example:
  ;;
  ;;
  ;; (when (progn (require '#:asdf) (find-package '#:asdf))
  ;;
  ;;    <function body>
  ;;
  ;;    (eval-when (:load-toplevel :execute)
  ;;      (setq sb-ext:*module-provider-functions*
  ;;         (pushnew 'module-provide-sbcl-contrib-asdf
  ;;                   sb-ext:*module-provider-functions*))))
  ;;
  ;;
  ;; Subsequently, the following form should evaluate successfully:
  ;;
  ;; (require '#:sb-posix)
  ;;
  ;;
  ;; Notes
  ;;
  ;; Sometimes, an SBCL contrib system may not have been successfully
  ;; installed with SBCL -- such as may occur when the compilation
  ;; failed one or more tests, such as may coincide with any undefined
  ;; references during sb-grovel for the sb-posix system, in some SBCL
  ;; builds.
  ;;
  ;; When the original source code for any single SBCL contrib system is
  ;; available at a known pathname, on the installation host, then --
  ;; concerning this method of using an ASDF PERFORM operation to
  ;; compile and load the original contrib system -- this function may
  ;; serve to provide a manner of an expedient utility for compiling
  ;; and loading the original source code for the respective contrib
  ;; system.

  (let* ((syspath (format nil "sys:contrib;~a;~a.asd"
                          name name))
         ;; NB: The following will ignore errors in logical pathname
         ;; translation, such that the Lisp may not be able to detect
         ;; trivially
         (sysdef-path (ignore-errors (probe-file syspath))))
    (cond
      (sysdef-path
       (let* ((asdf-pkg (or (find-package '#:asdf)
                            noerror
                            (error "ASDF not loaded?")))
              (*package* asdf-pkg))
         (when asdf-pkg
           ;; NB: In loading the system definition directly, this
           ;; function may be used independent of any system pathname
           ;; configuration in ASDF
           (load sysdef-path)
           (block load-op-x
             (flet ((symbol-ref (name)
                      ;; for compile/load time safety w/o ASDF
                      (declare (type symbol name))
                      (or (find-symbol (symbol-name name) asdf-pkg)
                          (and noerror (return-from load-op-x nil))
                          (progn
                            (error "Symbol ~s not found in ~s"
                                   name asdf-pkg)))))
               (let ((sysdef (funcall (symbol-ref '#:find-system)
                                      name
                                      nil)))
                 (when sysdef
                   (funcall (symbol-ref '#:operate)
                            (symbol-ref '#:load-op)
                            sysdef)
                   ;; return the system definition from within REQUIRE
                   sysdef))))
           )))
      (noerror nil)
      (t
       (error "No system definition found for module ~s at ~s"
              name syspath)))))


;; Local Variables:
;; ispell-buffer-session-localwords: ("funcall" "init" "noerror" "pathname" "pathnames" "posix" "progn" "pushnew" "sb" "sbcl" "sbclrc" "setq" "sys" "sysdef" "syspath" "toplevel")
;; End:
