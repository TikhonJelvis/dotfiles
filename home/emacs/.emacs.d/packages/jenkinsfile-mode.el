;;; jenkinsfile-mode.el --- Major mode for editing Jenkins declarative pipeline syntax -*- lexical-binding: t -*-

;; Copyright (c) 2019 John Louis Del Rosario
;; Package-Requires: ((emacs "24") (groovy-mode "2.0"))
;; Homepage: https://github.com/john2x/jenkinsfile-mode
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a major mode `jenkinsfile-mode' (derived from groovy-mode)
;; for editing Jenkins declarative pipeline files.

;;; Code:

(require 'groovy-mode)


(setq jenkinsfile-mode-font-lock-keywords
      `(("pipeline" . font-lock-preprocessor-face)
        ("agent\\|post\\|stages\\|steps" . font-lock-builtin-face)
        (,(regexp-opt '("environment" "options" "parameters"
                        "triggers" "stage" "tools" "input"
                        "when" "parallel" "script") 'symbols)
         . font-lock-constant-face)
        (,(regexp-opt '("always" "changed" "fixed"
                        "regression" "aborted" "failure" "success"
                        "unstable" "unsuccessful" "cleanup") 'symbols)
         . font-lock-keyword-face)
        (,(regexp-opt '("buildDiscarder" "checkoutToSubdirectory"
                        "disableConcurrentBuilds" "disableResume"
                        "newContainerPerStage" "overrideIndexTriggers"
                        "preserveStashes" "quietPeriod" "retry"
                        "skipDefaultCheckout" "skipStagesAfterUnstable"
                        "timeout" "timestamps" "parallelsAlwaysFailFast") 'symbols)
         . font-lock-keyword-face)
        (,(regexp-opt '("string" "text" "booleanParam" "choice" "file"
                        "password") 'symbols)
         . font-lock-keyword-face)
        (,(regexp-opt '("cron" "pollSCM" "upstream") 'symbols)
         . font-lock-keyword-face)
        (,(regexp-opt '("id" "ok" "subitter" "submitterParameter" "parameters") 'symbols)
         . font-lock-keyword-face)
        (,(regexp-opt '("branch" "buildingTag" "changelog" "changeset" "changeRequest"
                        "environment" "equals" "expression" "tag" "not" "allOf"
                        "anyOf" "triggeredBy") 'symbols)
         . font-lock-keyword-face)
        ))

(setq jenkinsfile-mode-font-lock-defaults
      (append groovy-font-lock-keywords jenkinsfile-mode-font-lock-keywords))

(define-derived-mode jenkinsfile-mode groovy-mode "jenkinsfile-mode"
  "Major mode for editing Jenkins declarative pipeline files."
  (setq font-lock-defaults '(jenkinsfile-mode-font-lock-defaults)))

(provide 'jenkinsfile-mode)
;;; jenkinsfile-mode.el ends here
