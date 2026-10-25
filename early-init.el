;;; early-init.el --- Emacs 27+ pre-initialisation config  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:
(setq package-enable-at-startup nil)

;;; disable compact font cache
(setq inhibit-compacting-font-caches t)

;; Disable native compilation: Emacs 31.1's bundled libgccjit passes
;; "-mmacosx-version-min=18.0" which Apple clang 21 rejects, breaking startup.
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (setq native-comp-enable-subr-trampolines nil)
  (setq native-comp-deferred-compilation nil
        package-native-compile nil
        comp-deferred-compilation-black-list '(".*")))

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
