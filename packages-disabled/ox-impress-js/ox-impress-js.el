;;; ox-impress-js.el --- impress.js Back-End for Org Export Engine

;; Copyright (C) 2014 Takumi KINJO

;; Author: Takumi Kinjo <takumi dot kinjo at gmail dot org>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library is a customized HTML back-end for Org generic exporter
;; to export to impress.js. This library is based on ox-html.el and
;; impress.js. I am appreciate those great works.

;;; Code:

;;; Dependencies

(require 'ox)
(require 'ox-publish)
(require 'format-spec)
(eval-when-compile (require 'cl) (require 'table nil 'noerror))


;;; Function Declarations

(declare-function org-id-find-id-file "org-id" (id))
(declare-function htmlize-region "ext:htmlize" (beg end))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))
(declare-function mm-url-decode-entities "mm-url" ())

;;; Define Back-End

(org-export-define-backend 'impress-js
  '((bold . org-impress-js-bold)
    (center-block . org-impress-js-center-block)
    (clock . org-impress-js-clock)
    (code . org-impress-js-code)
    (drawer . org-impress-js-drawer)
    (dynamic-block . org-impress-js-dynamic-block)
    (entity . org-impress-js-entity)
    (example-block . org-impress-js-example-block)
    (export-block . org-impress-js-export-block)
    (export-snippet . org-impress-js-export-snippet)
    (fixed-width . org-impress-js-fixed-width)
    (footnote-definition . org-impress-js-footnote-definition)
    (footnote-reference . org-impress-js-footnote-reference)
    (headline . org-impress-js-headline)
    (horizontal-rule . org-impress-js-horizontal-rule)
    (inline-src-block . org-impress-js-inline-src-block)
    (inlinetask . org-impress-js-inlinetask)
    (inner-template . org-impress-js-inner-template)
    (italic . org-impress-js-italic)
    (item . org-impress-js-item)
    (keyword . org-impress-js-keyword)
    (latex-environment . org-impress-js-latex-environment)
    (latex-fragment . org-impress-js-latex-fragment)
    (line-break . org-impress-js-line-break)
    (link . org-impress-js-link)
    (node-property . org-impress-js-node-property)
    (paragraph . org-impress-js-paragraph)
    (plain-list . org-impress-js-plain-list)
    (plain-text . org-impress-js-plain-text)
    (planning . org-impress-js-planning)
    (property-drawer . org-impress-js-property-drawer)
    (quote-block . org-impress-js-quote-block)
    (radio-target . org-impress-js-radio-target)
    (section . org-impress-js-section)
    (special-block . org-impress-js-special-block)
    (src-block . org-impress-js-src-block)
    (statistics-cookie . org-impress-js-statistics-cookie)
    (strike-through . org-impress-js-strike-through)
    (subscript . org-impress-js-subscript)
    (superscript . org-impress-js-superscript)
    (table . org-impress-js-table)
    (table-cell . org-impress-js-table-cell)
    (table-row . org-impress-js-table-row)
    (target . org-impress-js-target)
    (template . org-impress-js-template)
    (timestamp . org-impress-js-timestamp)
    (underline . org-impress-js-underline)
    (verbatim . org-impress-js-verbatim)
    (verse-block . org-impress-js-verse-block))
  :export-block "impress.js"
  :filters-alist '((:filter-options . org-impress-js-infojs-install-script)
		   (:filter-final-output . org-impress-js-final-function))
  :menu-entry
  '(?j "Export to impress.js HTML"
       ((?J "As impress.js HTML buffer" org-impress-js-export-as-html)
	(?j "As impress.js HTML file" org-impress-js-export-to-html)
	(?o "As impress.js HTML file and open"
	    (lambda (a s v b)
	      (if a (org-impress-js-export-to-html t s v b)
		(org-open-file (org-impress-js-export-to-html nil s v b)))))))
  :options-alist
  '((:creator "CREATOR" nil org-impress-js-creator-string)
    (:html-doctype "HTML_DOCTYPE" nil org-impress-js-doctype)
    (:html-description nil nil org-impress-js-description)
    (:html-fallback-message nil nil org-impress-js-fallback-message)
    (:html-hint-message nil nil org-impress-js-hint-message)
    (:html-hint-js nil nil org-impress-js-hint-js)
    (:html-link-home "HTML_LINK_HOME" nil org-impress-js-link-home)
    (:html-link-up "HTML_LINK_UP" nil org-impress-js-link-up)
    (:html-head "HTML_HEAD" nil org-impress-js-head newline)
    (:html-head-extra "HTML_HEAD_EXTRA" nil org-impress-js-head-extra newline)
    (:html-container "HTML_CONTAINER" nil org-impress-js-container-element)
    (:html-mathjax "HTML_MATHJAX" nil "" space)
    (:infojs-opt "INFOJS_OPT" nil nil)
    ;; Retrieve LaTeX header for fragments.
    (:latex-header "LATEX_HEADER" nil nil newline)
    (:html-extension nil nil org-impress-js-extension)
    (:html-link-org-as-html nil nil org-impress-js-link-org-files-as-html)
    (:html-html5-fancy nil "html5-fancy" org-impress-js-html5-fancy)
    (:html-link-use-abs-url nil "html-link-use-abs-url" org-impress-js-link-use-abs-url)
    (:html-postamble nil "html-postamble" org-impress-js-postamble)
    (:html-preamble nil "html-preamble" org-impress-js-preamble)
    (:html-impress-js-stylesheet "IMPRESSJS_STYLE" nil org-impress-js-stylesheet newline)
    (:html-impress-js-javascript "IMPRESSJS_SRC" nil org-impress-js-javascript newline)
    (:html-head-include-default-style nil "html-style" org-impress-js-head-include-default-style)
    (:html-head-include-scripts nil "html-scripts" org-impress-js-head-include-scripts)
    (:html-table-attributes nil nil org-impress-js-table-default-attributes)
    (:html-table-row-tags nil nil org-impress-js-table-row-tags)
    (:html-xml-declaration nil nil org-impress-js-xml-declaration)
    (:html-inline-images nil nil org-impress-js-inline-images)
    (:with-latex nil "tex" org-impress-js-with-latex)))


;;; Internal Variables

(defvar org-impress-js-format-table-no-css)
(defvar htmlize-buffer-places)  ; from htmlize.el

(defvar org-impress-js--pre/postamble-class "status"
  "CSS class used for pre/postamble")

(defconst org-impress-js-html5-elements
  '("article" "aside" "audio" "canvas" "details" "figcaption"
    "figure" "footer" "header" "menu" "meter" "nav" "output"
    "progress" "section" "video")
  "New elements in html5.

<hgroup> is not included because it's currently impossible to
wrap special blocks around multiple headlines. For other blocks
that should contain headlines, use the HTML_CONTAINER property on
the headline itself.")

(defconst org-impress-js-special-string-regexps
  '(("\\\\-" . "&#x00ad;")		; shy
    ("---\\([^-]\\)" . "&#x2014;\\1")	; mdash
    ("--\\([^-]\\)" . "&#x2013;\\1")	; ndash
    ("\\.\\.\\." . "&#x2026;"))		; hellip
  "Regular expressions for special string conversion.")

(defconst org-impress-js-scripts
  "<script type=\"text/javascript\">
/*
@licstart  The following is the entire license notice for the
JavaScript code in this tag.

Copyright (C) 2012-2013 Free Software Foundation, Inc.

The JavaScript code in this tag is free software: you can
redistribute it and/or modify it under the terms of the GNU
General Public License (GNU GPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option)
any later version.  The code is distributed WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

As additional permission under GNU GPL version 3 section 7, you
may distribute non-source (e.g., minimized or compacted) forms of
that code without the copy of the GNU GPL normally required by
section 4, provided you include this license notice and a URL
through which recipients can access the Corresponding Source.


@licend  The above is the entire license notice
for the JavaScript code in this tag.
*/
<!--/*--><![CDATA[/*><!--*/
 function CodeHighlightOn(elem, id)
 {
   var target = document.getElementById(id);
   if(null != target) {
     elem.cacheClassElem = elem.className;
     elem.cacheClassTarget = target.className;
     target.className = \"code-highlighted\";
     elem.className   = \"code-highlighted\";
   }
 }
 function CodeHighlightOff(elem, id)
 {
   var target = document.getElementById(id);
   if(elem.cacheClassElem)
     elem.className = elem.cacheClassElem;
   if(elem.cacheClassTarget)
     target.className = elem.cacheClassTarget;
 }
/*]]>*///-->
</script>"
  "Basic JavaScript that is needed by HTML files produced by Org mode.")

(defconst org-impress-js-style-default
  "<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  .title  { text-align: center; }
  .todo   { font-family: monospace; color: red; }
  .done   { color: green; }
  .tag    { background-color: #eee; font-family: monospace;
            padding: 2px; font-size: 80%; font-weight: normal; }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .right  { margin-left: auto; margin-right: 0px;  text-align: right; }
  .left   { margin-left: 0px;  margin-right: auto; text-align: left; }
  .center { margin-left: auto; margin-right: auto; text-align: center; }
  .underline { text-decoration: underline; }
  #postamble p, #preamble p { font-size: 90%; margin: .2em; }
  p.verse { margin-left: 3%; }
  pre {
    border: 1px solid #ccc;
    box-shadow: 3px 3px 3px #eee;
    padding: 8pt;
    font-family: monospace;
    overflow: auto;
    margin: 1.2em;
  }
  pre.src {
    position: relative;
    overflow: visible;
    padding-top: 1.2em;
  }
  pre.src:before {
    display: none;
    position: absolute;
    background-color: white;
    top: -10px;
    right: 10px;
    padding: 3px;
    border: 1px solid black;
  }
  pre.src:hover:before { display: inline;}
  pre.src-sh:before    { content: 'sh'; }
  pre.src-bash:before  { content: 'sh'; }
  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }
  pre.src-R:before     { content: 'R'; }
  pre.src-perl:before  { content: 'Perl'; }
  pre.src-java:before  { content: 'Java'; }
  pre.src-sql:before   { content: 'SQL'; }

  table { border-collapse:collapse; }
  caption.t-above { caption-side: top; }
  caption.t-bottom { caption-side: bottom; }
  td, th { vertical-align:top;  }
  th.right  { text-align: center;  }
  th.left   { text-align: center;   }
  th.center { text-align: center; }
  td.right  { text-align: right;  }
  td.left   { text-align: left;   }
  td.center { text-align: center; }
  dt { font-weight: bold; }
  .footpara:nth-child(2) { display: inline; }
  .footpara { display: block; }
  .footdef  { margin-bottom: 1em; }
  .figure { padding: 1em; }
  .figure p { text-align: center; }
  .inlinetask {
    padding: 10px;
    border: 2px solid gray;
    margin: 10px;
    background: #ffffcc;
  }
  #org-div-home-and-up
   { text-align: right; font-size: 70%; white-space: nowrap; }
  textarea { overflow-x: auto; }
  .linenr { font-size: smaller }
  .code-highlighted { background-color: #ffff00; }
  .org-info-js_info-navigation { border-style: none; }
  #org-info-js_console-label
    { font-size: 10px; font-weight: bold; white-space: nowrap; }
  .org-info-js_search-highlight
    { background-color: #ffff00; color: #000000; font-weight: bold; }
  /*]]>*/-->
</style>"
  "The default style specification for exported impress.js HTML files.
You can use `org-impress-js-head' and `org-impress-js-head-extra' to add to
this style.  If you don't want to include this default style,
customize `org-impress-js-head-include-default-style'.")


;;; User Configuration Variables

(defgroup org-export-impress-js nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export HTML"
  :group 'org-export)

;;;; Handle infojs

(defvar org-impress-js-infojs-opts-table
  '((path PATH "http://orgmode.org/org-info.js")
    (view VIEW "info")
    (toc TOC :with-toc)
    (ftoc FIXED_TOC "0")
    (tdepth TOC_DEPTH "max")
    (sdepth SECTION_DEPTH "max")
    (mouse MOUSE_HINT "underline")
    (buttons VIEW_BUTTONS "0")
    (ltoc LOCAL_TOC "1")
    (up LINK_UP :html-link-up)
    (home LINK_HOME :html-link-home))
  "JavaScript options, long form for script, default values.")

(defcustom org-impress-js-use-infojs 'when-configured
  "Non-nil when Sebastian Rose's Java Script org-info.js should be active.
This option can be nil or t to never or always use the script.
It can also be the symbol `when-configured', meaning that the
script will be linked into the export file if and only if there
is a \"#+INFOJS_OPT:\" line in the buffer.  See also the variable
`org-impress-js-infojs-options'."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "When configured in buffer" when-configured)
	  (const :tag "Always" t)))

(defcustom org-impress-js-infojs-options
  (mapcar (lambda (x) (cons (car x) (nth 2 x))) org-impress-js-infojs-opts-table)
  "Options settings for the INFOJS JavaScript.
Each of the options must have an entry in `org-impress-js-infojs-opts-table'.
The value can either be a string that will be passed to the script, or
a property.  This property is then assumed to be a property that is defined
by the Export/Publishing setup of Org.
The `sdepth' and `tdepth' parameters can also be set to \"max\", which
means to use the maximum value consistent with other options."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type
  `(set :greedy t :inline t
	,@(mapcar
	   (lambda (x)
	     (list 'cons (list 'const (car x))
		   '(choice
		     (symbol :tag "Publishing/Export property")
		     (string :tag "Value"))))
	   org-impress-js-infojs-opts-table)))

(defcustom org-impress-js-infojs-template
  "<script type=\"text/javascript\" src=\"%SCRIPT_PATH\">
/**
 *
 * @source: %SCRIPT_PATH
 *
 * @licstart  The following is the entire license notice for the
 *  JavaScript code in %SCRIPT_PATH.
 *
 * Copyright (C) 2012-2013 Free Software Foundation, Inc.
 *
 *
 * The JavaScript code in this tag is free software: you can
 * redistribute it and/or modify it under the terms of the GNU
 * General Public License (GNU GPL) as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option)
 * any later version.  The code is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.
 *
 * As additional permission under GNU GPL version 3 section 7, you
 * may distribute non-source (e.g., minimized or compacted) forms of
 * that code without the copy of the GNU GPL normally required by
 * section 4, provided you include this license notice and a URL
 * through which recipients can access the Corresponding Source.
 *
 * @licend  The above is the entire license notice
 * for the JavaScript code in %SCRIPT_PATH.
 *
 */
</script>

<script type=\"text/javascript\">

/*
@licstart  The following is the entire license notice for the
JavaScript code in this tag.

Copyright (C) 2012-2013 Free Software Foundation, Inc.

The JavaScript code in this tag is free software: you can
redistribute it and/or modify it under the terms of the GNU
General Public License (GNU GPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option)
any later version.  The code is distributed WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

As additional permission under GNU GPL version 3 section 7, you
may distribute non-source (e.g., minimized or compacted) forms of
that code without the copy of the GNU GPL normally required by
section 4, provided you include this license notice and a URL
through which recipients can access the Corresponding Source.


@licend  The above is the entire license notice
for the JavaScript code in this tag.
*/

<!--/*--><![CDATA[/*><!--*/
%MANAGER_OPTIONS
org_html_manager.setup();  // activate after the parameters are set
/*]]>*///-->
</script>"
  "The template for the export style additions when org-info.js is used.
Option settings will replace the %MANAGER-OPTIONS cookie."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defun org-impress-js-infojs-install-script (exp-plist backend)
  "Install script in export options when appropriate.
EXP-PLIST is a plist containing export options.  BACKEND is the
export back-end currently used."

  ;; Disable toc option because slide can be broken when exported with toc.
  (plist-put exp-plist :with-toc nil)

  (unless (or (memq 'body-only (plist-get exp-plist :export-options))
	      (not org-impress-js-use-infojs)
	      (and (eq org-impress-js-use-infojs 'when-configured)
		   (or (not (plist-get exp-plist :infojs-opt))
		       (string-match "\\<view:nil\\>"
				     (plist-get exp-plist :infojs-opt)))))
    (let* ((template org-impress-js-infojs-template)
	   (ptoc (plist-get exp-plist :with-toc))
	   (hlevels (plist-get exp-plist :headline-levels))
	   (sdepth hlevels)
	   (tdepth (if (integerp ptoc) (min ptoc hlevels) hlevels))
	   (options (plist-get exp-plist :infojs-opt))
	   (table org-impress-js-infojs-opts-table)
	   style)
      (dolist (entry table)
	(let* ((opt (car entry))
	       (var (nth 1 entry))
	       ;; Compute default values for script option OPT from
	       ;; `org-impress-js-infojs-options' variable.
	       (default
		 (let ((default (cdr (assq opt org-impress-js-infojs-options))))
		   (if (and (symbolp default) (not (memq default '(t nil))))
		       (plist-get exp-plist default)
		     default)))
	       ;; Value set through INFOJS_OPT keyword has precedence
	       ;; over the default one.
	       (val (if (and options
			     (string-match (format "\\<%s:\\(\\S-+\\)" opt)
					   options))
			(match-string 1 options)
		      default)))
	  (case opt
	    (path (setq template
			(replace-regexp-in-string
			 "%SCRIPT_PATH" val template t t)))
	    (sdepth (when (integerp (read val))
		      (setq sdepth (min (read val) sdepth))))
	    (tdepth (when (integerp (read val))
		      (setq tdepth (min (read val) tdepth))))
	    (otherwise (setq val
			     (cond
			      ((or (eq val t) (equal val "t")) "1")
			      ((or (eq val nil) (equal val "nil")) "0")
			      ((stringp val) val)
			      (t (format "%s" val))))
		       (push (cons var val) style)))))
      ;; Now we set the depth of the *generated* TOC to SDEPTH,
      ;; because the toc will actually determine the splitting.  How
      ;; much of the toc will actually be displayed is governed by the
      ;; TDEPTH option.
      (setq exp-plist (plist-put exp-plist :with-toc sdepth))
      ;; The table of contents should not show more sections than we
      ;; generate.
      (setq tdepth (min tdepth sdepth))
      (push (cons "TOC_DEPTH" tdepth) style)
      ;; Build style string.
      (setq style (mapconcat
		   (lambda (x) (format "org_html_manager.set(\"%s\", \"%s\");"
				  (car x)
				  (cdr x)))
		   style "\n"))
      (when (and style (> (length style) 0))
	(and (string-match "%MANAGER_OPTIONS" template)
	     (setq style (replace-match style t t template))
	     (setq exp-plist
		   (plist-put
		    exp-plist :html-head-extra
		    (concat (or (plist-get exp-plist :html-head-extra) "")
			    "\n"
			    style)))))
      ;; This script absolutely needs the table of contents, so we
      ;; change that setting.
      (unless (plist-get exp-plist :with-toc)
	(setq exp-plist (plist-put exp-plist :with-toc t)))
      ;; Return the modified property list.
      exp-plist)))

;;;; Bold, etc.

(defcustom org-impress-js-text-markup-alist
  '((bold . "<b>%s</b>")
    (code . "<code>%s</code>")
    (italic . "<i>%s</i>")
    (strike-through . "<del>%s</del>")
    (underline . "<span class=\"underline\">%s</span>")
    (verbatim . "<code>%s</code>"))
  "Alist of HTML expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underline' and `verbatim'.  The value is
a formatting string to wrap fontified text with.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(alist :key-type (symbol :tag "Markup type")
		:value-type (string :tag "Format string"))
  :options '(bold code italic strike-through underline verbatim))

(defcustom org-impress-js-indent nil
  "Non-nil means to indent the generated HTML.
Warning: non-nil may break indentation of source code blocks."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-impress-js-use-unicode-chars nil
  "Non-nil means to use unicode characters instead of HTML entities."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

;;;; Drawers

(defcustom org-impress-js-format-drawer-function
  (lambda (name contents) contents)
  "Function called to format a drawer in HTML code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

The default value simply returns the value of CONTENTS."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)

;;;; Footnotes

(defcustom org-impress-js-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'org-export-impress-js
  :type 'string)

(defcustom org-impress-js-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-impress-js
  :type 'string)

(defcustom org-impress-js-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'org-export-impress-js
  :type 'string)

;;;; Headline

(defcustom org-impress-js-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title."
  :group 'org-export-impress-js
  :type 'integer)

(defcustom org-impress-js-format-headline-function 'ignore
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags (string or nil).

The function result will be used in the section format string."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)

;;;; HTML-specific

(defcustom org-impress-js-allow-name-attribute-in-anchors nil
  "When nil, do not set \"name\" attribute in anchors.
By default, when appropriate, anchors are formatted with \"id\"
but without \"name\" attribute."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

;;;; Inlinetasks

(defcustom org-impress-js-format-inlinetask-function 'ignore
  "Function called to format an inlinetask in HTML code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)

;;;; LaTeX

(defcustom org-impress-js-with-latex org-export-with-latex
  "Non-nil means process LaTeX math snippets.

When set, the exporter will process LaTeX environments and
fragments.

This option can also be set with the +OPTIONS line,
e.g. \"tex:mathjax\".  Allowed values are:

nil            Ignore math snippets.
`verbatim'     Keep everything in verbatim
`dvipng'       Process the LaTeX fragments to images.  This will also
               include processing of non-math environments.
`imagemagick'  Convert the LaTeX fragments to pdf files and use
               imagemagick to convert pdf files to png files.
`mathjax'      Do MathJax preprocessing and arrange for MathJax.js to
               be loaded.
t              Synonym for `mathjax'."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Do not process math in any way" nil)
	  (const :tag "Use dvipng to make images" dvipng)
	  (const :tag "Use imagemagick to make images" imagemagick)
	  (const :tag "Use MathJax to display math" mathjax)
	  (const :tag "Leave math verbatim" verbatim)))

(defcustom org-impress-js-description
  "impress.js is a presentation tool based on the power of CSS3 transforms and transitions in modern browsers and inspired by the idea behind prezi.com."
  "For metadata description."
  :group 'org-export-impress-js
  :type 'string)

(defcustom org-impress-js-fallback-message
  "    <p>Your browser <b>doesn't support the features required</b> by impress.js, so you are presented with a simplified version of this presentation.</p>
<p>For the best experience please use the latest <b>Chrome</b>, <b>Safari</b> or <b>Firefox</b> browser.</p>
"
  "impress.js fallback-message."
  :group 'org-export-impress-js
  :type 'string)

(defcustom org-impress-js-hint-message
  "    <p>Use a spacebar or arrow keys to navigate</p>\n"
  "impress.js hint message."
  :group 'org-export-impress-js
  :type 'string)

(defcustom org-impress-js-hint-js
  "if (\"ontouchstart\" in document.documentElement) {
document.querySelector(\".hint\").innerHTML = \"<p>Tap on the left or right to navigate</p>\";
}
"
  "impress.js hint JavaScript."
  :group 'org-export-impress-js
  :type 'string)

;;;; Links :: Generic

(defcustom org-impress-js-link-org-files-as-html t
  "Non-nil means make file links to `file.org' point to `file.html'.
When `org-mode' is exporting an `org-mode' file to HTML, links to
non-html files are directly put into a href tag in HTML.
However, links to other Org-mode files (recognized by the
extension `.org.) should become links to the corresponding html
file, assuming that the linked `org-mode' file will also be
converted to HTML.
When nil, the links still point to the plain `.org' file."
  :group 'org-export-impress-js
  :type 'boolean)

;;;; Links :: Inline images

(defcustom org-impress-js-inline-images t
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.1")
  :type 'boolean)

(defcustom org-impress-js-inline-image-rules
  '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
    ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
    ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'"))
  "Rules characterizing image files that can be inlined into HTML.
A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))

;;;; Plain Text

(defcustom org-impress-js-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-impress-js-protect'."
  :group 'org-export-impress-js
  :type '(repeat (cons (string :tag "Character")
		       (string :tag "HTML equivalent"))))

;;;; Src Block

(defcustom org-impress-js-htmlize-output-type 'inline-css
  "Output type to be used by htmlize when formatting code snippets.
Choices are `css', to export the CSS selectors only, or `inline-css', to
export the CSS attribute values inline in the HTML.  We use as default
`inline-css', in order to make the resulting HTML self-containing.

However, this will fail when using Emacs in batch mode for export, because
then no rich font definitions are in place.  It will also not be good if
people with different Emacs setup contribute HTML files to a website,
because the fonts will represent the individual setups.  In these cases,
it is much better to let Org/Htmlize assign classes only, and to use
a style file to define the look of these classes.
To get a start for your css file, start Emacs session and make sure that
all the faces you are interested in are defined, for example by loading files
in all modes you want.  Then, use the command
\\[org-impress-js-htmlize-generate-css] to extract class definitions."
  :group 'org-export-impress-js
  :type '(choice (const css) (const inline-css)))

(defcustom org-impress-js-htmlize-font-prefix "org-"
  "The prefix for CSS class names for htmlize font specifications."
  :group 'org-export-impress-js
  :type 'string)

;;;; Table

(defcustom org-impress-js-table-default-attributes
  '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides")
  "Default attributes and values which will be used in table tags.
This is a plist where attributes are symbols, starting with
colons, and values are strings.

When exporting to HTML5, these values will be disregarded."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(plist :key-type (symbol :tag "Property")
		:value-type (string :tag "Value")))

(defcustom org-impress-js-table-header-tags '("<th scope=\"%s\"%s>" . "</th>")
  "The opening tag for table header fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-impress-js-table-use-header-tags-for-first-column'.
See also the variable `org-impress-js-table-align-individual-fields'."
  :group 'org-export-impress-js
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-impress-js-table-data-tags '("<td%s>" . "</td>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-impress-js-table-align-individual-fields'."
  :group 'org-export-impress-js
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-impress-js-table-row-tags '("<tr>" . "</tr>")
  "The opening and ending tags for table rows.
This is customizable so that alignment options can be specified.
Instead of strings, these can be Lisp forms that will be
evaluated for each row in order to construct the table row tags.

During evaluation, these variables will be dynamically bound so that
you can reuse them:

       `row-number': row number (0 is the first row)
  `rowgroup-number': group number of current row
 `start-rowgroup-p': non-nil means the row starts a group
   `end-rowgroup-p': non-nil means the row ends a group
        `top-row-p': non-nil means this is the top row
     `bottom-row-p': non-nil means this is the bottom row

For example:

\(setq org-impress-js-table-row-tags
      (cons '(cond (top-row-p \"<tr class=\\\"tr-top\\\">\")
                   (bottom-row-p \"<tr class=\\\"tr-bottom\\\">\")
                   (t (if (= (mod row-number 2) 1)
			  \"<tr class=\\\"tr-odd\\\">\"
			\"<tr class=\\\"tr-even\\\">\")))
	    \"</tr>\"))

will use the \"tr-top\" and \"tr-bottom\" classes for the top row
and the bottom row, and otherwise alternate between \"tr-odd\" and
\"tr-even\" for odd and even rows."
  :group 'org-export-impress-js
  :type '(cons
	  (choice :tag "Opening tag"
		  (string :tag "Specify")
		  (sexp))
	  (choice :tag "Closing tag"
		  (string :tag "Specify")
		  (sexp))))

(defcustom org-impress-js-table-align-individual-fields t
  "Non-nil means attach style attributes for alignment to each table field.
When nil, alignment will only be specified in the column tags, but this
is ignored by some browsers (like Firefox, Safari).  Opera does it right
though."
  :group 'org-export-impress-js
  :type 'boolean)

(defcustom org-impress-js-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'org-export-impress-js
  :type 'boolean)

(defcustom org-impress-js-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-impress-js
  :type 'boolean)

;;;; Tags

(defcustom org-impress-js-tag-class-prefix ""
  "Prefix to class names for TODO keywords.
Each tag gets a class given by the tag itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-impress-js
  :type 'string)

;;;; Template :: Generic

(defcustom org-impress-js-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-impress-js
  :type 'string)

(defcustom org-impress-js-xml-declaration
  '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
    ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
  "The extension for exported HTML files.
%s will be replaced with the charset of the exported file.
This may be a string, or an alist with export extensions
and corresponding declarations.

This declaration only applies when exporting to XHTML."
  :group 'org-export-impress-js
  :type '(choice
	  (string :tag "Single declaration")
	  (repeat :tag "Dependent on extension"
		  (cons (string :tag "Extension")
			(string :tag "Declaration")))))

(defcustom org-impress-js-coding-system 'utf-8
  "Coding system for HTML export.
Use utf-8 as the default value."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'coding-system)

(defconst org-impress-js-doctype "html5"
  "Document type definition to use for exported impress.js HTML files.")

(defcustom org-impress-js-html5-fancy nil
  "Non-nil means using new HTML5 elements.
This variable is ignored for anything other than HTML5 export.

For compatibility with Internet Explorer, it's probably a good
idea to download some form of the html5shiv (for instance
https://code.google.com/p/html5shiv/) and add it to your
HTML_HEAD_EXTRA, so that your pages don't break for users of IE
versions 8 and below."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-impress-js-container-element "div"
  "HTML element to use for wrapping top level sections.
Can be set with the in-buffer HTML_CONTAINER property or for
publishing, with :html-container.

Note that changing the default will prevent you from using
org-info.js for your website."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-impress-js-divs
  '((preamble  "div" "preamble")
    (content   "div" "impress")
    (postamble "div" "postamble"))
  "Alist of the three section elements for HTML export.
The car of each entry is one of 'preamble, 'content or 'postamble.
The cdrs of each entry are the ELEMENT_TYPE and ID for each
section of the exported document.

Note that changing the default will prevent you from using
org-info.js for your website."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(list :greedy t
	       (list :tag "Preamble"
		     (const :format "" preamble)
		     (string :tag "element") (string :tag "     id"))
	       (list :tag "Content"
		     (const :format "" content)
		     (string :tag "element") (string :tag "     id"))
	       (list :tag "Postamble" (const :format "" postamble)
		     (string :tag "     id") (string :tag "element"))))

(defconst org-impress-js-checkbox-types
  '((unicode .
     ((on . "&#x2611;") (off . "&#x2610;") (trans . "&#x2610;")))
    (ascii .
     ((on . "<code>[X]</code>")
      (off . "<code>[&#xa0;]</code>")
      (trans . "<code>[-]</code>")))
    (html .
	  ((on . "<input type='checkbox' checked='checked' />")
	  (off . "<input type='checkbox' />")
	  (trans . "<input type='checkbox' />"))))
  "Alist of checkbox types.
The cdr of each entry is an alist list three checkbox types for
HTML export: `on', `off' and `trans'.

The choices are:
  `unicode' Unicode characters (HTML entities)
  `ascii'   ASCII characters
  `html'    HTML checkboxes

Note that only the ascii characters implement tri-state
checkboxes. The other two use the `off' checkbox for `trans'.")

(defcustom org-impress-js-checkbox-type 'ascii
  "The type of checkboxes to use for HTML export.
See `org-impress-js-checkbox-types' for for the values used for each
option."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "ASCII characters" ascii)
	  (const :tag "Unicode characters" unicode)
	  (const :tag "HTML checkboxes" html)))

(defcustom org-impress-js-metadata-timestamp-format "%Y-%m-%d %a %H:%M"
  "Format used for timestamps in preamble, postamble and metadata.
See `format-time-string' for more information on its components."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

;;;; Template :: Mathjax

(defcustom org-impress-js-mathjax-options
  '((path  "http://orgmode.org/mathjax/MathJax.js")
    (scale "100")
    (align "center")
    (indent "2em")
    (mathml nil))
  "Options for MathJax setup.

path        The path where to find MathJax
scale       Scaling for the HTML-CSS backend, usually between 100 and 133
align       How to align display math: left, center, or right
indent      If align is not center, how far from the left/right side?
mathml      Should a MathML player be used if available?
            This is faster and reduces bandwidth use, but currently
            sometimes has lower spacing quality.  Therefore, the default is
            nil.  When browsers get better, this switch can be flipped.

You can also customize this for each buffer, using something like

#+MATHJAX: scale:\"133\" align:\"right\" mathml:t path:\"/MathJax/\""
  :group 'org-export-impress-js
  :type '(list :greedy t
	      (list :tag "path   (the path from where to load MathJax.js)"
		    (const :format "       " path) (string))
	      (list :tag "scale  (scaling for the displayed math)"
		    (const :format "       " scale) (string))
	      (list :tag "align  (alignment of displayed equations)"
		    (const :format "       " align) (string))
	      (list :tag "indent (indentation with left or right alignment)"
		    (const :format "       " indent) (string))
	      (list :tag "mathml (should MathML display be used is possible)"
		    (const :format "       " mathml) (boolean))))

(defcustom org-impress-js-mathjax-template
  "<script type=\"text/javascript\" src=\"%PATH\"></script>
<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
    MathJax.Hub.Config({
        // Only one of the two following lines, depending on user settings
        // First allows browser-native MathML display, second forces HTML/CSS
        :MMLYES: config: [\"MMLorHTML.js\"], jax: [\"input/TeX\"],
        :MMLNO: jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [\"STIX\",\"TeX\"],
             preferredFont: \"TeX\",
             webFont: \"TeX\",
             imageFont: \"TeX\",
             showMathMenu: true,
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        }
    });
/*]]>*///-->
</script>"
  "The MathJax setup for XHTML files."
  :group 'org-export-impress-js
  :type 'string)

;;;; Template :: Postamble

(defcustom org-impress-js-postamble 'auto
  "Non-nil means insert a postamble in HTML export.

When set to 'auto, check against the
`org-export-with-author/email/creator/date' variables to set the
content of the postamble.  When set to a string, use this string
as the postamble.  When t, insert a string as defined by the
formatting string in `org-impress-js-postamble-format'.

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting :html-postamble in publishing projects will take
precedence over this variable."
  :group 'org-export-impress-js
  :type '(choice (const :tag "No postamble" nil)
		 (const :tag "Auto postamble" auto)
		 (const :tag "Default formatting string" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-impress-js-postamble-format
  '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">%c</p>
<p class=\"validation\">%v</p>"))
  "Alist of languages and format strings for the HTML postamble.

The first element of each list is the language code, as used for
the LANGUAGE keyword.  See `org-export-default-language'.

The second element of each list is a format string to format the
postamble itself.  This format string can contain these elements:

  %t stands for the title.
  %a stands for the author's name.
  %e stands for the author's email.
  %d stands for the date.
  %c will be replaced by `org-impress-js-creator-string'.
  %v will be replaced by `org-impress-js-validation-link'.
  %T will be replaced by the export time.
  %C will be replaced by the last modification time.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-impress-js
  :type '(repeat
	  (list (string :tag "Language")
		(string :tag "Format string"))))

(defcustom org-impress-js-validation-link
  "<a href=\"http://validator.w3.org/check?uri=referer\">Validate</a>"
  "Link to HTML validation service."
  :group 'org-export-impress-js
  :type 'string)

(defcustom org-impress-js-creator-string
  (format "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> %s (<a href=\"http://orgmode.org\">Org</a> mode %s)"
	  emacs-version
	  (if (fboundp 'org-version) (org-version) "unknown version"))
  "Information about the creator of the HTML document.
This option can also be set on with the CREATOR keyword."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(string :tag "Creator string"))

;;;; Template :: Preamble

(defcustom org-impress-js-preamble t
  "Non-nil means insert a preamble in HTML export.

When t, insert a string as defined by the formatting string in
`org-impress-js-preamble-format'.  When set to a string, use this
formatting string instead (see `org-impress-js-postamble-format' for an
example of such a formatting string).

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting :html-preamble in publishing projects will take
precedence over this variable."
  :group 'org-export-impress-js
  :type '(choice (const :tag "No preamble" nil)
		 (const :tag "Default preamble" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-impress-js-preamble-format '(("en" ""))
  "Alist of languages and format strings for the HTML preamble.

The first element of each list is the language code, as used for
the LANGUAGE keyword.  See `org-export-default-language'.

The second element of each list is a format string to format the
preamble itself.  This format string can contain these elements:

  %t stands for the title.
  %a stands for the author's name.
  %e stands for the author's email.
  %d stands for the date.
  %c will be replaced by `org-impress-js-creator-string'.
  %v will be replaced by `org-impress-js-validation-link'.
  %T will be replaced by the export time.
  %C will be replaced by the last modification time.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\".

See the default value of `org-impress-js-postamble-format' for an
example."
  :group 'org-export-impress-js
  :type '(repeat
	  (list (string :tag "Language")
		(string :tag "Format string"))))

(defcustom org-impress-js-link-up ""
  "Where should the \"UP\" link of exported HTML pages lead?"
  :group 'org-export-impress-js
  :type '(string :tag "File or URL"))

(defcustom org-impress-js-link-home ""
  "Where should the \"HOME\" link of exported HTML pages lead?"
  :group 'org-export-impress-js
  :type '(string :tag "File or URL"))

(defcustom org-impress-js-link-use-abs-url nil
  "Should we prepend relative links with HTML_LINK_HOME?"
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.1")
  :type 'boolean)

(defcustom org-impress-js-home/up-format
  "<div id=\"org-div-home-and-up\">
 <a accesskey=\"h\" href=\"%s\"> UP </a>
 |
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
  "Snippet used to insert the HOME and UP links.
This is a format string, the first %s will receive the UP link,
the second the HOME link.  If both `org-impress-js-link-up' and
`org-impress-js-link-home' are empty, the entire snippet will be
ignored."
  :group 'org-export-impress-js
  :type 'string)

;;;; Template :: Scripts

(define-obsolete-variable-alias
  'org-impress-js-style-include-scripts 'org-impress-js-head-include-scripts "24.4")
(defcustom org-impress-js-head-include-scripts t
  "Non-nil means include the JavaScript snippets in exported HTML files.
The actual script is defined in `org-impress-js-scripts' and should
not be modified."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

;;;; Template :: Styles

(define-obsolete-variable-alias
  'org-impress-js-style-include-default 'org-impress-js-head-include-default-style "24.4")
(defcustom org-impress-js-head-include-default-style t
  "Non-nil means include the default style in exported HTML files.
The actual style is defined in `org-impress-js-style-default' and
should not be modified.  Use `org-impress-js-head' to use your own
style information."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)
;;;###autoload
(put 'org-impress-js-head-include-default-style 'safe-local-variable 'booleanp)

(define-obsolete-variable-alias 'org-impress-js-style 'org-impress-js-head "24.4")
(defcustom org-impress-js-head ""
  "Org-wide head definitions for exported HTML files.

This variable can contain the full HTML structure to provide a
style, including the surrounding HTML tags.  You can consider
including definitions for the following classes: title, todo,
done, timestamp, timestamp-kwd, tag, target.

For example, a valid value would be:

   <style type=\"text/css\">
    <![CDATA[
       p { font-weight: normal; color: gray; }
       h1 { color: black; }
      .title { text-align: center; }
      .todo, .timestamp-kwd { color: red; }
      .done { color: green; }
    ]]>
   </style>

If you want to refer to an external style, use something like

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\" />

As the value of this option simply gets inserted into the HTML
<head> header, you can use it to add any arbitrary text to the
header.

You can set this on a per-file basis using #+HTML_HEAD:,
or for publication projects using the :html-head property."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-impress-js-stylesheet "http://bartaz.github.io/impress.js/css/impress-demo.css"
  "Path to a default CSS file for impress.js. 

Use IMPRESSJS_STYLE option in your org-mode file is available too."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-impress-js-javascript "http://bartaz.github.io/impress.js/js/impress.js"
  "Path to a JavaScript file for impress.js.

Use IMPRESSJS_SRC option in your org-mode file is available too."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

;;;###autoload
(put 'org-impress-js-head 'safe-local-variable 'stringp)

(defcustom org-impress-js-head-extra ""
  "More head information to add in the HTML output.

You can set this on a per-file basis using #+HTML_HEAD_EXTRA:,
or for publication projects using the :html-head-extra property."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)
;;;###autoload
(put 'org-impress-js-head-extra 'safe-local-variable 'stringp)

;;;; Todos

(defcustom org-impress-js-todo-kwd-class-prefix ""
  "Prefix to class names for TODO keywords.
Each TODO keyword gets a class given by the keyword itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-impress-js
  :type 'string)


;;; Internal Functions

(defun org-impress-js-xhtml-p (info) nil)

(defun org-impress-js-html5-p (info) t)

(defun org-impress-js-close-tag (tag attr info)
  (concat "<" tag " " attr " />"))

(defun org-impress-js-doctype (info) "Return correct html doctype tag." "<!DOCTYPE html>")

(defun org-impress-js--make-attribute-string (attributes)
  "Return a list of attributes, as a string.
ATTRIBUTES is a plist where values are either strings or nil. An
attributes with a nil value will be omitted from the result."
  (let (output)
    (dolist (item attributes (mapconcat 'identity (nreverse output) " "))
      (cond ((null item) (pop output))
            ((symbolp item) (push (substring (symbol-name item) 1) output))
            (t (let ((key (car output))
                     (value (replace-regexp-in-string
                             "\"" "&quot;" (org-impress-js-encode-plain-text item))))
                 (setcar output (format "%s=\"%s\"" key value))))))))

(defun org-impress-js--wrap-image (contents info &optional caption label)
  "Wrap CONTENTS string within an appropriate environment for images.
INFO is a plist used as a communication channel.  When optional
arguments CAPTION and LABEL are given, use them for caption and
\"id\" attribute."
  (let ((html5-fancy (and (org-impress-js-html5-p info)
			  (plist-get info :html-html5-fancy))))
    (format (if html5-fancy "\n<figure%s>%s%s\n</figure>"
	      "\n<div%s class=\"figure\">%s%s\n</div>")
	    ;; ID.
	    (if (not (org-string-nw-p label)) ""
	      (format " id=\"%s\"" (org-export-solidify-link-text label)))
	    ;; Contents.
	    (format "\n<p>%s</p>" contents)
	    ;; Caption.
	    (if (not (org-string-nw-p caption)) ""
	      (format (if html5-fancy "\n<figcaption>%s</figcaption>"
			"\n<p>%s</p>")
		      caption)))))

(defun org-impress-js--format-image (source attributes info)
  "Return \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by
`org-export-read-attribute'.  INFO is a plist used as
a communication channel."
  (if (string= "svg" (file-name-extension source))
      (org-impress-js--svg-image source attributes info)
    (org-impress-js-close-tag
     "img"
     (org-impress-js--make-attribute-string
      (org-combine-plists
       (list :src source
	     :alt (if (string-match-p "^ltxpng/" source)
		      (org-impress-js-encode-plain-text
		       (org-find-text-property-in-string 'org-latex-src source))
		    (file-name-nondirectory source)))
       attributes))
     info)))

(defun org-impress-js--svg-image (source attributes info)
  "Return \"object\" appropriate for embedding svg file SOURCE
with assoicated ATTRIBUTES. INFO is a plist used as a
communication channel.

The special attribute \"fallback\" can be used to specify a fallback
image file to use if the object embedding is not supported."
  (let ((fallback (plist-get attributes :fallback))
	(attrs (org-impress-js--make-attribute-string
		(plist-put attributes :fallback nil))))
  (format "<object type=\"image/svg+xml\" data=\"%s\" %s>\n%s</object>"
	  source attrs
	  (if fallback
	      (org-impress-js-close-tag
	       "img" (format "src=\"%s\" %s" fallback attrs) info)
	    "Sorry, your browser does not support SVG."))))

(defun org-impress-js--textarea-block (element)
  "Transcode ELEMENT into a textarea block.
ELEMENT is either a src block or an example block."
  (let* ((code (car (org-export-unravel-code element)))
	 (attr (org-export-read-attribute :attr_html element)))
    (format "<p>\n<textarea cols=\"%s\" rows=\"%s\">\n%s</textarea>\n</p>"
	    (or (plist-get attr :width) 80)
	    (or (plist-get attr :height) (org-count-lines code))
	    code)))

(defun org-impress-js--has-caption-p (element &optional info)
  "Non-nil when ELEMENT has a caption affiliated keyword.
INFO is a plist used as a communication channel.  This function
is meant to be used as a predicate for `org-export-get-ordinal' or
a value to `org-impress-js-standalone-image-predicate'."
  (org-element-property :caption element))

;;;; Table

(defun org-impress-js-htmlize-region-for-paste (beg end)
  "Convert the region between BEG and END to HTML, using htmlize.el.
This is much like `htmlize-region-for-paste', only that it uses
the settings define in the org-... variables."
  (let* ((htmlize-output-type org-impress-js-htmlize-output-type)
	 (htmlize-css-name-prefix org-impress-js-htmlize-font-prefix)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

;;;###autoload
(defun org-impress-js-htmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-impress-js-htmlize-output-type' to `css', calls
to the function `org-impress-js-htmlize-region-for-paste' will
produce code that uses these same face definitions."
  (interactive)
  (require 'htmlize)
  (and (get-buffer "*html*") (kill-buffer "*html*"))
  (with-temp-buffer
    (let ((fl (face-list))
	  (htmlize-css-name-prefix "org-")
	  (htmlize-output-type 'css)
	  f i)
      (while (setq f (pop fl)
		   i (and f (face-attribute f :inherit)))
	(when (and (symbolp f) (or (not i) (not (listp i))))
	  (insert (org-add-props (copy-sequence "1") nil 'face f))))
      (htmlize-region (point-min) (point-max))))
  (org-pop-to-buffer-same-window "*html*")
  (goto-char (point-min))
  (if (re-search-forward "<style" nil t)
      (delete-region (point-min) (match-beginning 0)))
  (if (re-search-forward "</style>" nil t)
      (delete-region (1+ (match-end 0)) (point-max)))
  (beginning-of-line 1)
  (if (looking-at " +") (replace-match ""))
  (goto-char (point-min)))

(defun org-impress-js--make-string (n string)
  "Build a string by concatenating N times STRING."
  (let (out) (dotimes (i n out) (setq out (concat string out)))))

(defun org-impress-js-fix-class-name (kwd)	; audit callers of this function
  "Turn todo keyword KWD into a valid class name.
Replaces invalid characters with \"_\"."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" kwd)
      (setq kwd (replace-match "_" t t kwd))))
  kwd)

(defun org-impress-js-format-footnote-reference (n def refcnt)
  "Format footnote reference N with definition DEF into HTML."
  (let ((extra (if (= refcnt 1) "" (format ".%d"  refcnt))))
    (format org-impress-js-footnote-format
	    (let* ((id (format "fnr.%s%s" n extra))
		   (href (format " href=\"#fn.%s\"" n))
		   (attributes (concat " class=\"footref\"" href)))
	      (org-impress-js--anchor id n attributes)))))

(defun org-impress-js-format-footnotes-section (section-name definitions)
  "Format footnotes section SECTION-NAME."
  (if (not definitions) ""
    (format org-impress-js-footnotes-section section-name definitions)))

(defun org-impress-js-format-footnote-definition (fn)
  "Format the footnote definition FN."
  (let ((n (car fn)) (def (cdr fn)))
    (format
     "<div class=\"footdef\">%s %s</div>\n"
     (format org-impress-js-footnote-format
	     (let* ((id (format "fn.%s" n))
		    (href (format " href=\"#fnr.%s\"" n))
		    (attributes (concat " class=\"footnum\"" href)))
	       (org-impress-js--anchor id n attributes)))
     def)))

(defun org-impress-js-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions
		    (plist-get info :parse-tree) info))
	 (fn-alist
	  (loop for (n type raw) in fn-alist collect
		(cons n (if (eq (org-element-type raw) 'org-data)
			    (org-trim (org-export-data raw info))
			  (format "<p>%s</p>"
				  (org-trim (org-export-data raw info))))))))
    (when fn-alist
      (org-impress-js-format-footnotes-section
       (org-impress-js--translate "Footnotes" info)
       (format
	"\n%s\n"
	(mapconcat 'org-impress-js-format-footnote-definition fn-alist "\n"))))))


;;; Template

(defun org-impress-js--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let ((protect-string
	 (lambda (str)
	   (replace-regexp-in-string
	    "\"" "&quot;" (org-impress-js-encode-plain-text str))))
	(title (org-export-data (plist-get info :title) info))
	(author (and (plist-get info :with-author)
		     (let ((auth (plist-get info :author)))
		       (and auth
			    ;; Return raw Org syntax, skipping non
			    ;; exportable objects.
			    (org-element-interpret-data
			     (org-element-map auth
				 (cons 'plain-text org-element-all-objects)
			       'identity info))))))
	(description (plist-get info :html-description))
	(keywords (plist-get info :keywords))
	(charset (or (and org-impress-js-coding-system
			  (fboundp 'coding-system-get)
			  (coding-system-get org-impress-js-coding-system
					     'mime-charset))
		     "iso-8859-1")))
    (concat
     (format "<title>%s</title>\n" title)
     (when (plist-get info :time-stamp-file)
       (format-time-string
	 (concat "<!-- " org-impress-js-metadata-timestamp-format " -->\n")))
     (format
      (if (org-impress-js-html5-p info)
	  (org-impress-js-close-tag "meta" " charset=\"%s\"" info)
	(org-impress-js-close-tag
	 "meta" " http-equiv=\"Content-Type\" content=\"text/html;charset=%s\""
	 info))
      charset) "\n"
     (org-impress-js-close-tag "meta" " name=\"generator\" content=\"Org-mode\"" info)
     "\n"
     (org-impress-js-close-tag "meta" " name=\"viewport\" content=\"width=1024\"" info)
     "\n"
     (org-impress-js-close-tag "meta" " name=\"apple-mobile-web-app-capable\" content=\"yes\"" info) "\n"
     (and (org-string-nw-p author)
	  (concat
	   (org-impress-js-close-tag "meta"
			       (format " name=\"author\" content=\"%s\""
				       (funcall protect-string author))
			       info)
	   "\n"))
     (and (org-string-nw-p description)
	  (concat
	   (org-impress-js-close-tag "meta"
			       (format " name=\"description\" content=\"%s\"\n"
				       (funcall protect-string description))
			       info)
	   "\n"))
     (and (org-string-nw-p keywords)
	  (concat
	   (org-impress-js-close-tag "meta"
			       (format " name=\"keywords\" content=\"%s\""
				       (funcall protect-string keywords))
			       info)
	   "\n"))
      (org-impress-js-close-tag "link" " href=\"http://fonts.googleapis.com/css?family=Open+Sans:regular,semibold,italic,italicsemibold|PT+Sans:400,700,400italic,700italic|PT+Serif:400,700,400italic,700italic\" rel=\"stylesheet\"" info) "\n"
      (org-impress-js-close-tag "link" " rel=\"shortcut icon\" href=\"favicon.png\"" info) "\n"
      (org-impress-js-close-tag "link" " rel=\"apple-touch-icon\" href=\"apple-touch-icon.png\"" info) "\n")))

(defun org-impress-js--build-impress-js-stylesheet (info)
  "Return a link tag to load impress.js CSS file.
INFO is a plist used as a communication channel."
  (org-element-normalize-string
   (concat
    (when (plist-get info :html-impress-js-stylesheet)
      (org-impress-js-close-tag "link"
			  (format " rel=\"stylesheet\" href=\"%s\" type=\"text/css\""
				  (plist-get info :html-impress-js-stylesheet))
			  info)))))

(defun org-impress-js--build-head (info)
  "Return information for the <head>..</head> of the HTML output.
INFO is a plist used as a communication channel."
  (org-element-normalize-string
   (concat
    (when (plist-get info :html-head-include-default-style)
      (org-element-normalize-string org-impress-js-style-default))
    (org-element-normalize-string (plist-get info :html-head))
    (org-element-normalize-string (plist-get info :html-head-extra))
    (when (and (plist-get info :html-htmlized-css-url)
	       (eq org-impress-js-htmlize-output-type 'css))
      (org-impress-js-close-tag "link"
			  (format " rel=\"stylesheet\" href=\"%s\" type=\"text/css\""
				  (plist-get info :html-htmlized-css-url))
			  info))
    (when (plist-get info :html-head-include-scripts) org-impress-js-scripts))))

(defun org-impress-js--build-mathjax-config (info)
  "Insert the user setup into the mathjax template.
INFO is a plist used as a communication channel."
  (when (and (memq (plist-get info :with-latex) '(mathjax t))
	     (org-element-map (plist-get info :parse-tree)
		 '(latex-fragment latex-environment) 'identity info t))
    (let ((template org-impress-js-mathjax-template)
	  (options org-impress-js-mathjax-options)
	  (in-buffer (or (plist-get info :html-mathjax) ""))
	  name val (yes "   ") (no "// ") x)
      (mapc
       (lambda (e)
	 (setq name (car e) val (nth 1 e))
	 (if (string-match (concat "\\<" (symbol-name name) ":") in-buffer)
	     (setq val (car (read-from-string
			     (substring in-buffer (match-end 0))))))
	 (if (not (stringp val)) (setq val (format "%s" val)))
	 (if (string-match (concat "%" (upcase (symbol-name name))) template)
	     (setq template (replace-match val t t template))))
       options)
      (setq val (nth 1 (assq 'mathml options)))
      (if (string-match (concat "\\<mathml:") in-buffer)
	  (setq val (car (read-from-string
			  (substring in-buffer (match-end 0))))))
      ;; Exchange prefixes depending on mathml setting.
      (if (not val) (setq x yes yes no no x))
      ;; Replace cookies to turn on or off the config/jax lines.
      (if (string-match ":MMLYES:" template)
	  (setq template (replace-match yes t t template)))
      (if (string-match ":MMLNO:" template)
	  (setq template (replace-match no t t template)))
      ;; Return the modified template.
      (org-element-normalize-string template))))

(defun org-impress-js-format-spec (info)
  "Return format specification for elements that can be
used in the preamble or postamble."
  `((?t . ,(org-export-data (plist-get info :title) info))
    (?d . ,(org-export-data (org-export-get-date info) info))
    (?T . ,(format-time-string org-impress-js-metadata-timestamp-format))
    (?a . ,(org-export-data (plist-get info :author) info))
    (?e . ,(mapconcat
	    (lambda (e)
	      (format "<a href=\"mailto:%s\">%s</a>" e e))
	    (split-string (plist-get info :email)  ",+ *")
	    ", "))
    (?c . ,(plist-get info :creator))
    (?C . ,(let ((file (plist-get info :input-file)))
	     (format-time-string org-impress-js-metadata-timestamp-format
				 (if file (nth 5 (file-attributes file))
				   (current-time)))))
    (?v . ,(or org-impress-js-validation-link ""))))

(defun org-impress-js--build-pre/postamble (type info)
  "Return document preamble or postamble as a string, or nil.
TYPE is either 'preamble or 'postamble, INFO is a plist used as a
communication channel."
  (let ((section (plist-get info (intern (format ":html-%s" type))))
	(spec (org-impress-js-format-spec info)))
    (when section
      (let ((section-contents
	     (if (functionp section) (funcall section info)
	       (cond
		((stringp section) (format-spec section spec))
		((eq section 'auto)
		 (let ((date (cdr (assq ?d spec)))
		       (author (cdr (assq ?a spec)))
		       (email (cdr (assq ?e spec)))
		       (creator (cdr (assq ?c spec)))
		       (timestamp (cdr (assq ?T spec)))
		       (validation-link (cdr (assq ?v spec))))
		   (concat
		    (when (and (plist-get info :with-date)
			       (org-string-nw-p date))
		      (format "<p class=\"date\">%s: %s</p>\n"
			      (org-impress-js--translate "Date" info)
			      date))
		    (when (and (plist-get info :with-author)
			       (org-string-nw-p author))
		      (format "<p class=\"author\">%s: %s</p>\n"
			      (org-impress-js--translate "Author" info)
			      author))
		    (when (and (plist-get info :with-email)
			       (org-string-nw-p email))
		      (format "<p class=\"email\">%s: %s</p>\n"
			      (org-impress-js--translate "Email" info)
			      email))
		    (when (plist-get info :time-stamp-file)
		      (format
		       "<p class=\"date\">%s: %s</p>\n"
		       (org-impress-js--translate "Created" info)
		       (format-time-string org-impress-js-metadata-timestamp-format)))
		    (when (plist-get info :with-creator)
		      (format "<p class=\"creator\">%s</p>\n" creator))
		    (format "<p class=\"validation\">%s</p>\n"
			    validation-link))))
		(t (format-spec
		    (or (cadr (assoc
			       (plist-get info :language)
			       (eval (intern
				      (format "org-impress-js-%s-format" type)))))
			(cadr
			 (assoc
			  "en"
			  (eval
			   (intern (format "org-impress-js-%s-format" type))))))
		    spec))))))
	(when (org-string-nw-p section-contents)
	  (concat
	   (format "<%s id=\"%s\" class=\"%s\">\n"
		   (nth 1 (assq type org-impress-js-divs))
		   (nth 2 (assq type org-impress-js-divs))
		   org-impress-js--pre/postamble-class)
	   (org-element-normalize-string section-contents)
	   (format "</%s>\n" (nth 1 (assq type org-impress-js-divs)))))))))

(defun org-impress-js--build-fallback-message (info)
  "Return impress.js fallback-message as a string.
INFO is a plist used as a communication channel."
  (concat "<div class=\"fallback-message\">\n"
	  (plist-get info :html-fallback-message)
	  "</div>\n"))

(defun org-impress-js--build-title (info)
  "Return a title step.

Postamble will be embeded if available. See `org-impress-js-postamble'."
  (org-element-normalize-string
   (concat
    "<div id=\"title\" class=\"step\" data-x=\"0\" data-y=\"0\" data-scale=\"1\">\n"
    ;; Document title.
    (let ((title (plist-get info :title)))
      (format "<h1>%s</h1>\n" (org-export-data (or title "") info)))
    (org-impress-js--build-pre/postamble 'postamble info)
    "</div>\n")))

(defun org-impress-js--build-hint-message (info)
  "Return impress.js hint message as a string.
INFO is a plist used as a communication channel."
  (concat "<div class=\"hint\">\n"
	  (plist-get info :html-hint-message)
	  "</div>\n"))

(defun org-impress-js--build-init-impress-js (info)
  "Return a init script for impress.js as a string.
INFO is a plist used as a communication channel."
  (concat "<script>\n"
	  (plist-get info :html-hint-js)
	  "</script>\n"
	  (format "<script src=\"%s\"></script>\n"
		  (plist-get info :html-impress-js-javascript))
	  "<script>impress().init();</script>\n"))

(defun org-impress-js-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-impress-js-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-impress-js-footnote-section info)))

(defun org-impress-js-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-impress-js-html5-p info)) (org-impress-js-xhtml-p info))
     (let ((decl (or (and (stringp org-impress-js-xml-declaration)
			      org-impress-js-xml-declaration)
			 (cdr (assoc (plist-get info :html-extension)
				     org-impress-js-xml-declaration))
			 (cdr (assoc "html" org-impress-js-xml-declaration))

			 "")))
       (when (not (or (eq nil decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
		  (or (and org-impress-js-coding-system
			   (fboundp 'coding-system-get)
			   (coding-system-get org-impress-js-coding-system 'mime-charset))
		      "iso-8859-1"))))))
   (org-impress-js-doctype info)
   "\n"
   (format "<html lang=\"%s\">\n" (plist-get info :language))
   "<head>\n"
   (org-impress-js--build-meta-info info)
   (org-impress-js--build-impress-js-stylesheet info)
   (org-impress-js--build-head info)
   (org-impress-js--build-mathjax-config info)
   "</head>\n"
   "<body class=\"impress-not-supported\">\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format org-impress-js-home/up-format
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Preamble.
   ;; (org-impress-js--build-pre/postamble 'preamble info)
   ;; Fallback message.
   (org-impress-js--build-fallback-message info)
   ;; Document contents.
   (format "<%s id=\"%s\">\n"
	   (nth 1 (assq 'content org-impress-js-divs))
	   (nth 2 (assq 'content org-impress-js-divs)))
   ;; Title.
   (org-impress-js--build-title info)
   contents
   (format "</%s>\n"
	   (nth 1 (assq 'content org-impress-js-divs)))
   ;; Postamble.
   ;; (org-impress-js--build-pre/postamble 'postamble info)
   ;; Hint message.
   (org-impress-js--build-hint-message info)
   ;; impress.js init.
   (org-impress-js--build-init-impress-js info)
   ;; Closing document.
   "</body>\n</html>"))

(defun org-impress-js--translate (s info)
  "Translate string S according to specified language.
INFO is a plist used as a communication channel."
  (org-export-translate s :html info))

;;;; Anchor

(defun org-impress-js--anchor (&optional id desc attributes)
  "Format a HTML anchor."
  (let* ((name (and org-impress-js-allow-name-attribute-in-anchors id))
	 (attributes (concat (and id (format " id=\"%s\"" id))
			     (and name (format " name=\"%s\"" name))
			     attributes)))
    (format "<a%s>%s</a>" attributes (or desc ""))))

;;;; Todo

(defun org-impress-js--todo (todo)
  "Format TODO keywords into HTML."
  (when todo
    (format "<span class=\"%s %s%s\">%s</span>"
	    (if (member todo org-done-keywords) "done" "todo")
	    org-impress-js-todo-kwd-class-prefix (org-impress-js-fix-class-name todo)
	    todo)))

;;;; Tags

(defun org-impress-js--tags (tags)
  "Format TAGS into HTML."
  (when tags
    (format "<span class=\"tag\">%s</span>"
	    (mapconcat
	     (lambda (tag)
	       (format "<span class=\"%s\">%s</span>"
		       (concat org-impress-js-tag-class-prefix
			       (org-impress-js-fix-class-name tag))
		       tag))
	     tags "&#xa0;"))))

;;;; Headline

(defun* org-impress-js-format-headline
  (todo todo-type priority text tags
	&key level section-number headline-label &allow-other-keys)
  "Format a headline in HTML."
  (let ((section-number
	 (when section-number
	   (format "<span class=\"section-number-%d\">%s</span> "
		   level section-number)))
	(todo (org-impress-js--todo todo))
	(tags (org-impress-js--tags tags)))
    (concat section-number todo (and todo " ") text
	    (and tags "&#xa0;&#xa0;&#xa0;") tags)))

;;;; Src Code

(defun org-impress-js-fontify-code (code lang)
  "Color CODE with htmlize library.
CODE is a string representing the source code to colorize.  LANG
is the language used for CODE, as a string, or nil."
  (when code
    (cond
     ;; Case 1: No lang.  Possibly an example block.
     ((not lang)
      ;; Simple transcoding.
      (org-impress-js-encode-plain-text code))
     ;; Case 2: No htmlize or an inferior version of htmlize
     ((not (and (require 'htmlize nil t) (fboundp 'htmlize-region-for-paste)))
      ;; Emit a warning.
      (message "Cannot fontify src block (htmlize.el >= 1.34 required)")
      ;; Simple transcoding.
      (org-impress-js-encode-plain-text code))
     (t
      ;; Map language
      (setq lang (or (assoc-default lang org-src-lang-modes) lang))
      (let* ((lang-mode (and lang (intern (format "%s-mode" lang)))))
	(cond
	 ;; Case 1: Language is not associated with any Emacs mode
	 ((not (functionp lang-mode))
	  ;; Simple transcoding.
	  (org-impress-js-encode-plain-text code))
	 ;; Case 2: Default.  Fontify code.
	 (t
	  ;; htmlize
	  (setq code (with-temp-buffer
		       ;; Switch to language-specific mode.
		       (funcall lang-mode)
		       (insert code)
		       ;; Fontify buffer.
		       (font-lock-fontify-buffer)
		       ;; Remove formatting on newline characters.
		       (save-excursion
			 (let ((beg (point-min))
			       (end (point-max)))
			   (goto-char beg)
			   (while (progn (end-of-line) (< (point) end))
			     (put-text-property (point) (1+ (point)) 'face nil)
			     (forward-char 1))))
		       (org-src-mode)
		       (set-buffer-modified-p nil)
		       ;; Htmlize region.
		       (org-impress-js-htmlize-region-for-paste
			(point-min) (point-max))))
	  ;; Strip any enclosing <pre></pre> tags.
	  (let* ((beg (and (string-match "\\`<pre[^>]*>\n*" code) (match-end 0)))
		 (end (and beg (string-match "</pre>\\'" code))))
	    (if (and beg end) (substring code beg end) code)))))))))

(defun org-impress-js-do-format-code
  (code &optional lang refs retain-labels num-start)
  "Format CODE string as source code.
Optional arguments LANG, REFS, RETAIN-LABELS and NUM-START are,
respectively, the language of the source code, as a string, an
alist between line numbers and references (as returned by
`org-export-unravel-code'), a boolean specifying if labels should
appear in the source code, and the number associated to the first
line of code."
  (let* ((code-lines (org-split-string code "\n"))
	 (code-length (length code-lines))
	 (num-fmt
	  (and num-start
	       (format "%%%ds: "
		       (length (number-to-string (+ code-length num-start))))))
	 (code (org-impress-js-fontify-code code lang)))
    (org-export-format-code
     code
     (lambda (loc line-num ref)
       (setq loc
	     (concat
	      ;; Add line number, if needed.
	      (when num-start
		(format "<span class=\"linenr\">%s</span>"
			(format num-fmt line-num)))
	      ;; Transcoded src line.
	      loc
	      ;; Add label, if needed.
	      (when (and ref retain-labels) (format " (%s)" ref))))
       ;; Mark transcoded line as an anchor, if needed.
       (if (not ref) loc
	 (format "<span id=\"coderef-%s\" class=\"coderef-off\">%s</span>"
		 ref loc)))
     num-start refs)))

(defun org-impress-js-format-code (element info)
  "Format contents of ELEMENT as source code.
ELEMENT is either an example block or a src block.  INFO is
a plist used as a communication channel."
  (let* ((lang (org-element-property :language element))
	 ;; Extract code and references.
	 (code-info (org-export-unravel-code element))
	 (code (car code-info))
	 (refs (cdr code-info))
	 ;; Does the src block contain labels?
	 (retain-labels (org-element-property :retain-labels element))
	 ;; Does it have line numbers?
	 (num-start (case (org-element-property :number-lines element)
		      (continued (org-export-get-loc element info))
		      (new 0))))
    (org-impress-js-do-format-code code lang refs retain-labels num-start)))


;;; Tables of Contents

(defun org-impress-js-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is a
plist used as a communication channel.  Return the table of
contents as a string, or nil if it is empty."
  (let ((toc-entries
	 (mapcar (lambda (headline)
		   (cons (org-impress-js--format-toc-headline headline info)
			 (org-export-get-relative-level headline info)))
		 (org-export-collect-headlines info depth)))
	(outer-tag (if (and (org-impress-js-html5-p info)
			    (plist-get info :html-html5-fancy))
		       "nav"
		     "div")))
    (when toc-entries
      (concat (format "<%s id=\"table-of-contents\">\n" outer-tag)
	      (format "<h%d>%s</h%d>\n"
		      org-impress-js-toplevel-hlevel
		      (org-impress-js--translate "Table of Contents" info)
		      org-impress-js-toplevel-hlevel)
	      "<div id=\"text-table-of-contents\">"
	      (org-impress-js--toc-text toc-entries)
	      "</div>\n"
	      (format "</%s>\n" outer-tag)))))

(defun org-impress-js--toc-text (toc-entries)
  "Return innards of a table of contents, as a string.
TOC-ENTRIES is an alist where key is an entry title, as a string,
and value is its relative level, as an integer."
  (let* ((prev-level (1- (cdar toc-entries)))
	 (start-level prev-level))
    (concat
     (mapconcat
      (lambda (entry)
	(let ((headline (car entry))
	      (level (cdr entry)))
	  (concat
	   (let* ((cnt (- level prev-level))
		  (times (if (> cnt 0) (1- cnt) (- cnt)))
		  rtn)
	     (setq prev-level level)
	     (concat
	      (org-impress-js--make-string
	       times (cond ((> cnt 0) "\n<ul>\n<li>")
			   ((< cnt 0) "</li>\n</ul>\n")))
	      (if (> cnt 0) "\n<ul>\n<li>" "</li>\n<li>")))
	   headline)))
      toc-entries "")
     (org-impress-js--make-string (- prev-level start-level) "</li>\n</ul>\n"))))

(defun org-impress-js--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((headline-number (org-export-get-headline-number headline info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data-with-backend
		(org-export-get-alt-title headline info)
		;; Create an anonymous back-end that will ignore any
		;; footnote-reference, link, radio-target and target
		;; in table of contents.
		(org-export-create-backend
		 :parent 'html
		 :transcoders '((footnote-reference . ignore)
				(link . (lambda (object c i) c))
				(radio-target . (lambda (object c i) c))
				(target . ignore)))
		info))
	 (tags (and (eq (plist-get info :with-tags) t)
		    (org-export-get-tags headline info))))
    (format "<a href=\"#%s\">%s</a>"
	    ;; Label.
	    (org-export-solidify-link-text
	     (or (org-element-property :CUSTOM_ID headline)
		 (concat "sec-"
			 (mapconcat #'number-to-string headline-number "-"))))
	    ;; Body.
	    (concat
	     (and (not (org-export-low-level-p headline info))
		  (org-export-numbered-headline-p headline info)
		  (concat (mapconcat #'number-to-string headline-number ".")
			  ". "))
	     (apply (if (not (eq org-impress-js-format-headline-function 'ignore))
			(lambda (todo todo-type priority text tags &rest ignore)
			  (funcall org-impress-js-format-headline-function
				   todo todo-type priority text tags))
		      #'org-impress-js-format-headline)
		    todo todo-type priority text tags :section-number nil)))))

(defun org-impress-js-list-of-listings (info)
  "Build a list of listings.
INFO is a plist used as a communication channel.  Return the list
of listings as a string, or nil if it is empty."
  (let ((lol-entries (org-export-collect-listings info)))
    (when lol-entries
      (concat "<div id=\"list-of-listings\">\n"
	      (format "<h%d>%s</h%d>\n"
		      org-impress-js-toplevel-hlevel
		      (org-impress-js--translate "List of Listings" info)
		      org-impress-js-toplevel-hlevel)
	      "<div id=\"text-list-of-listings\">\n<ul>\n"
	      (let ((count 0)
		    (initial-fmt (format "<span class=\"listing-number\">%s</span>"
					 (org-impress-js--translate "Listing %d:" info))))
		(mapconcat
		 (lambda (entry)
		   (let ((label (org-element-property :name entry))
			 (title (org-trim
				 (org-export-data
				  (or (org-export-get-caption entry t)
				      (org-export-get-caption entry))
				  info))))
		     (concat
		      "<li>"
		      (if (not label)
			  (concat (format initial-fmt (incf count)) " " title)
			(format "<a href=\"#%s\">%s %s</a>"
				(org-export-solidify-link-text label)
				(format initial-fmt (incf count))
				title))
		      "</li>")))
		 lol-entries "\n"))
	      "\n</ul>\n</div>\n</div>"))))

(defun org-impress-js-list-of-tables (info)
  "Build a list of tables.
INFO is a plist used as a communication channel.  Return the list
of tables as a string, or nil if it is empty."
  (let ((lol-entries (org-export-collect-tables info)))
    (when lol-entries
      (concat "<div id=\"list-of-tables\">\n"
	      (format "<h%d>%s</h%d>\n"
		      org-impress-js-toplevel-hlevel
		      (org-impress-js--translate "List of Tables" info)
		      org-impress-js-toplevel-hlevel)
	      "<div id=\"text-list-of-tables\">\n<ul>\n"
	      (let ((count 0)
		    (initial-fmt (format "<span class=\"table-number\">%s</span>"
					 (org-impress-js--translate "Table %d:" info))))
		(mapconcat
		 (lambda (entry)
		   (let ((label (org-element-property :name entry))
			 (title (org-trim
				 (org-export-data
				  (or (org-export-get-caption entry t)
				      (org-export-get-caption entry))
				  info))))
		     (concat
		      "<li>"
		      (if (not label)
			  (concat (format initial-fmt (incf count)) " " title)
			(format "<a href=\"#%s\">%s %s</a>"
				(org-export-solidify-link-text label)
				(format initial-fmt (incf count))
				title))
		      "</li>")))
		 lol-entries "\n"))
	      "\n</ul>\n</div>\n</div>"))))


;;; Transcode Functions

;;;; Bold

(defun org-impress-js-bold (bold contents info)
  "Transcode BOLD from Org to HTML.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format (or (cdr (assq 'bold org-impress-js-text-markup-alist)) "%s")
	  contents))

;;;; Center Block

(defun org-impress-js-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "<div class=\"center\">\n%s</div>" contents))

;;;; Clock

(defun org-impress-js-clock (clock contents info)
  "Transcode a CLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<p>
<span class=\"timestamp-wrapper\">
<span class=\"timestamp-kwd\">%s</span> <span class=\"timestamp\">%s</span>%s
</span>
</p>"
	  org-clock-string
	  (org-translate-time
	   (org-element-property :raw-value
				 (org-element-property :value clock)))
	  (let ((time (org-element-property :duration clock)))
	    (and time (format " <span class=\"timestamp\">(%s)</span>" time)))))

;;;; Code

(defun org-impress-js-code (code contents info)
  "Transcode CODE from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format (or (cdr (assq 'code org-impress-js-text-markup-alist)) "%s")
	  (org-impress-js-encode-plain-text (org-element-property :value code))))

;;;; Drawer

(defun org-impress-js-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (if (functionp org-impress-js-format-drawer-function)
      (funcall org-impress-js-format-drawer-function
	       (org-element-property :drawer-name drawer)
	       contents)
    ;; If there's no user defined function: simply
    ;; display contents of the drawer.
    contents))

;;;; Dynamic Block

(defun org-impress-js-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  contents)

;;;; Entity

(defun org-impress-js-entity (entity contents info)
  "Transcode an ENTITY object from Org to HTML.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :html entity))

;;;; Example Block

(defun org-impress-js-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (if (org-export-read-attribute :attr_html example-block :textarea)
      (org-impress-js--textarea-block example-block)
    (format "<pre class=\"example\">\n%s</pre>"
	    (org-impress-js-format-code example-block info))))

;;;; Export Snippet

(defun org-impress-js-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (eq (org-export-snippet-backend export-snippet) 'html)
    (org-element-property :value export-snippet)))

;;;; Export Block

(defun org-impress-js-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "HTML")
    (org-remove-indentation (org-element-property :value export-block))))

;;;; Fixed Width

(defun org-impress-js-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "<pre class=\"example\">\n%s</pre>"
	  (org-impress-js-do-format-code
	   (org-remove-indentation
	    (org-element-property :value fixed-width)))))

;;;; Footnote Reference

(defun org-impress-js-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       org-impress-js-footnote-separator))
   (cond
    ((not (org-export-footnote-first-reference-p footnote-reference info))
     (org-impress-js-format-footnote-reference
      (org-export-get-footnote-number footnote-reference info)
      "IGNORED" 100))
    ;; Inline definitions are secondary strings.
    ((eq (org-element-property :type footnote-reference) 'inline)
     (org-impress-js-format-footnote-reference
      (org-export-get-footnote-number footnote-reference info)
      "IGNORED" 1))
    ;; Non-inline footnotes definitions are full Org data.
    (t (org-impress-js-format-footnote-reference
	(org-export-get-footnote-number footnote-reference info)
	"IGNORED" 1)))))

;;;; Headline

(defun org-impress-js-format-headline--wrap
  (headline info &optional format-function &rest extra-keys)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((level (+ (org-export-get-relative-level headline info)
		   (1- org-impress-js-toplevel-hlevel)))
	 (headline-number (org-export-get-headline-number headline info))
	 (section-number (and (not (org-export-low-level-p headline info))
			      (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 headline-number ".")))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data (org-element-property :title headline) info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (headline-label (or (org-element-property :CUSTOM_ID headline)
			     (concat "sec-" (mapconcat 'number-to-string
						       headline-number "-"))))
	 (format-function
	  (cond ((functionp format-function) format-function)
		((not (eq org-impress-js-format-headline-function 'ignore))
		 (lambda (todo todo-type priority text tags &rest ignore)
		   (funcall org-impress-js-format-headline-function
			    todo todo-type priority text tags)))
		(t 'org-impress-js-format-headline))))
    (apply format-function
	   todo todo-type  priority text tags
	   :headline-label headline-label :level level
	   :section-number section-number extra-keys)))

(defun org-impress-js-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Empty contents?
  (setq contents (or contents ""))
  (let* ((numberedp (org-export-numbered-headline-p headline info))
	 (level (org-export-get-relative-level headline info))
	 (text (org-export-data (org-element-property :title headline) info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 (org-export-get-headline-number
					  headline info) ".")))
	 ;; Create the headline text.
	 (full-text (org-impress-js-format-headline--wrap headline info))
	 ;; Attributes used to position presentation steps
	 (class (org-export-get-node-property :CLASS headline))
	 (data-x (org-export-get-node-property :DATA-X headline))
	 (data-y (org-export-get-node-property :DATA-Y headline))
	 (data-z (org-export-get-node-property :DATA-Z headline))
	 (data-scale (org-export-get-node-property :DATA-SCALE headline))
	 (data-rotate (org-export-get-node-property :DATA-ROTATE headline))
	 (data-rotate-x (org-export-get-node-property :DATA-ROTATE-X headline))
	 (data-rotate-y (org-export-get-node-property :DATA-ROTATE-Y headline))
	 (data-rotate-z (org-export-get-node-property :DATA-ROTATE-Z headline)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((org-export-low-level-p headline info)
      ;; Build the real contents of the sub-tree.
      (let* ((type (if numberedp 'ordered 'unordered))
	     (itemized-body (org-impress-js-format-list-item
			     contents type nil info nil full-text)))
	(concat
	 (and (org-export-first-sibling-p headline info)
	      (org-impress-js-begin-plain-list type))
	 itemized-body
	 (and (org-export-last-sibling-p headline info)
	      (org-impress-js-end-plain-list type)))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (let* ((section-number (mapconcat 'number-to-string
					(org-export-get-headline-number
					 headline info) "-"))
	     (ids (remove 'nil
			  (list (org-element-property :CUSTOM_ID headline)
				(concat "sec-" section-number)
				(org-element-property :ID headline))))
	     (preferred-id (car ids))
	     (extra-ids (cdr ids))
	     (extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
	     ;; Ignore the section indentations.
	     (level1 1)
	     (first-content (car (org-element-contents headline))))
	(format "<%s id=\"%s\" class=\"%s\"%s>%s%s\n"
		(org-impress-js--container headline info)
		(format "outline-container-%s"
			(or (org-element-property :CUSTOM_ID headline)
			    (concat "sec-" section-number)))
		(concat (format "outline-%d" level1) (and extra-class " ")
			extra-class
			(if class (format " %s" class) " step"))
		(concat (and data-x (format " data-x=\"%s\"" data-x))
			(and data-y (format " data-y=\"%s\"" data-y))
			(and data-z (format " data-z=\"%s\"" data-z))
			(and data-scale (format " data-scale=\"%s\"" data-scale))
			(and data-rotate (format " data-rotate=\"%s\"" data-rotate))
			(and data-rotate-x (format " data-rotate-x=\"%s\"" data-rotate-x))
			(and data-rotate-y (format " data-rotate-y=\"%s\"" data-rotate-y))
			(and data-rotate-z (format " data-rotate-z=\"%s\"" data-rotate-z)))
		(format "\n<h%d id=\"%s\">%s%s</h%d>\n"
			level1
			preferred-id
			(mapconcat
			 (lambda (x)
			   (let ((id (org-export-solidify-link-text
				      (if (org-uuidgen-p x) (concat "ID-" x)
					x))))
			     (org-impress-js--anchor id)))
			 extra-ids "")
			full-text
			level1)
		;; When there is no section, pretend there is an empty
		;; one to get the correct <div class="outline- ...>
		;; which is needed by `org-info.js'.
		(if (not (eq (org-element-type first-content) 'section))
		    (concat (org-impress-js-section first-content "" info)
			    contents)
		  contents)))))))

(defun org-impress-js--container (headline info)
  (or (org-element-property :HTML_CONTAINER headline)
      (if (= 1 (org-export-get-relative-level headline info))
	  (plist-get info :html-container)
	"div")))

;;;; Horizontal Rule

(defun org-impress-js-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE  object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-impress-js-close-tag "hr" nil info))

;;;; Inline Src Block

(defun org-impress-js-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((org-lang (org-element-property :language inline-src-block))
	 (code (org-element-property :value inline-src-block)))
    (error "Cannot export inline src block")))

;;;; Inlinetask

(defun org-impress-js-format-section (text class &optional id)
  "Format a section with TEXT into a HTML div with CLASS and ID."
  (let ((extra (concat (when id (format " id=\"%s\"" id)))))
    (concat (format "<div class=\"%s\"%s>\n" class extra) text "</div>\n")))

(defun org-impress-js-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (cond
   ;; If `org-impress-js-format-inlinetask-function' is not 'ignore, call it
   ;; with appropriate arguments.
   ((not (eq org-impress-js-format-inlinetask-function 'ignore))
    (let ((format-function
	   (function*
	    (lambda (todo todo-type priority text tags
		     &key contents &allow-other-keys)
	      (funcall org-impress-js-format-inlinetask-function
		       todo todo-type priority text tags contents)))))
      (org-impress-js-format-headline--wrap
       inlinetask info format-function :contents contents)))
   ;; Otherwise, use a default template.
   (t (format "<div class=\"inlinetask\">\n<b>%s</b>%s\n%s</div>"
	      (org-impress-js-format-headline--wrap inlinetask info)
	      (org-impress-js-close-tag "br" nil info)
	      contents))))

;;;; Italic

(defun org-impress-js-italic (italic contents info)
  "Transcode ITALIC from Org to HTML.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format (or (cdr (assq 'italic org-impress-js-text-markup-alist)) "%s") contents))

;;;; Item

(defun org-impress-js-checkbox (checkbox info)
  "Format CHECKBOX into HTML.
INFO is a plist holding contextual information.  See
`org-impress-js-checkbox-type' for customization options."
  (cdr (assq checkbox
	     (cdr (assq org-impress-js-checkbox-type org-impress-js-checkbox-types)))))

(defun org-impress-js-format-list-item (contents type checkbox info
					     &optional term-counter-id
					     headline)
  "Format a list item into HTML."
  (let ((checkbox (concat (org-impress-js-checkbox checkbox info)
			  (and checkbox " ")))
	(br (org-impress-js-close-tag "br" nil info)))
    (concat
     (case type
       (ordered
	(let* ((counter term-counter-id)
	       (extra (if counter (format " value=\"%s\"" counter) "")))
	  (concat
	   (format "<li%s>" extra)
	   (when headline (concat headline br)))))
       (unordered
	(let* ((id term-counter-id)
	       (extra (if id (format " id=\"%s\"" id) "")))
	  (concat
	   (format "<li%s>" extra)
	   (when headline (concat headline br)))))
       (descriptive
	(let* ((term term-counter-id))
	  (setq term (or term "(no term)"))
	  ;; Check-boxes in descriptive lists are associated to tag.
	  (concat (format "<dt> %s </dt>"
			  (concat checkbox term))
		  "<dd>"))))
     (unless (eq type 'descriptive) checkbox)
     contents
     (case type
       (ordered "</li>")
       (unordered "</li>")
       (descriptive "</dd>")))))

(defun org-impress-js-item (item contents info)
  "Transcode an ITEM element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (counter (org-element-property :counter item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-data tag info)))))
    (org-impress-js-format-list-item
     contents type checkbox info (or tag counter))))

;;;; Keyword

(defun org-impress-js-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "HTML") value))))
     
;;;; Latex Environment

(defun org-impress-js-format-latex (latex-frag processing-type info)
  "Format a LaTeX fragment LATEX-FRAG into HTML.
PROCESSING-TYPE designates the tool used for conversion.  It is
a symbol among `mathjax', `dvipng', `imagemagick', `verbatim' nil
and t.  See `org-impress-js-with-latex' for more information.  INFO is
a plist containing export properties."
  (let ((cache-relpath "") (cache-dir ""))
    (unless (eq processing-type 'mathjax)
      (let ((bfn (or (buffer-file-name)
		     (make-temp-name
		      (expand-file-name "latex" temporary-file-directory))))
	    (latex-header
	     (let ((header (plist-get info :latex-header)))
	       (and header
		    (concat (mapconcat
			     (lambda (line) (concat "#+LATEX_HEADER: " line))
			     (org-split-string header "\n")
			     "\n")
			    "\n")))))
	(setq cache-relpath
	      (concat "ltxpng/"
		      (file-name-sans-extension
		       (file-name-nondirectory bfn)))
	      cache-dir (file-name-directory bfn))
	;; Re-create LaTeX environment from original buffer in
	;; temporary buffer so that dvipng/imagemagick can properly
	;; turn the fragment into an image.
	(setq latex-frag (concat latex-header latex-frag))))
    (with-temp-buffer
      (insert latex-frag)
      (org-format-latex cache-relpath cache-dir nil "Creating LaTeX Image..."
			nil nil processing-type)
      (buffer-string))))

(defun org-impress-js-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((processing-type (plist-get info :with-latex))
	(latex-frag (org-remove-indentation
		     (org-element-property :value latex-environment)))
	(attributes (org-export-read-attribute :attr_html latex-environment)))
    (case processing-type
      ((t mathjax)
       (org-impress-js-format-latex latex-frag 'mathjax info))
      ((dvipng imagemagick)
       (let ((formula-link
	      (org-impress-js-format-latex latex-frag processing-type info)))
	 (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
	   ;; Do not provide a caption or a name to be consistent with
	   ;; `mathjax' handling.
	   (org-impress-js--wrap-image
	    (org-impress-js--format-image
	     (match-string 1 formula-link) attributes info) info))))
      (t latex-frag))))

;;;; Latex Fragment

(defun org-impress-js-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((latex-frag (org-element-property :value latex-fragment))
	(processing-type (plist-get info :with-latex)))
    (case processing-type
      ((t mathjax)
       (org-impress-js-format-latex latex-frag 'mathjax info))
      ((dvipng imagemagick)
       (let ((formula-link
	      (org-impress-js-format-latex latex-frag processing-type info)))
	 (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
	   (org-impress-js--format-image (match-string 1 formula-link) nil info))))
      (t latex-frag))))

;;;; Line Break

(defun org-impress-js-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat (org-impress-js-close-tag "br" nil info) "\n"))

;;;; Link

(defun org-impress-js-inline-image-p (link info)
  "Non-nil when LINK is meant to appear as an image.
INFO is a plist used as a communication channel.  LINK is an
inline image when it has no description and targets an image
file (see `org-impress-js-inline-image-rules' for more information), or
if its description is a single link targeting an image file."
  (if (not (org-element-contents link))
      (org-export-inline-image-p link org-impress-js-inline-image-rules)
    (not
     (let ((link-count 0))
       (org-element-map (org-element-contents link)
	   (cons 'plain-text org-element-all-objects)
	 (lambda (obj)
	   (case (org-element-type obj)
	     (plain-text (org-string-nw-p obj))
	     (link (if (= link-count 1) t
		     (incf link-count)
		     (not (org-export-inline-image-p
			   obj org-impress-js-inline-image-rules))))
	     (otherwise t)))
         info t)))))

(defvar org-impress-js-standalone-image-predicate)
(defun org-impress-js-standalone-image-p (element info)
  "Test if ELEMENT is a standalone image.

INFO is a plist holding contextual information.

Return non-nil, if ELEMENT is of type paragraph and its sole
content, save for white spaces, is a link that qualifies as an
inline image.

Return non-nil, if ELEMENT is of type link and its containing
paragraph has no other content save white spaces.

Return nil, otherwise.

Bind `org-impress-js-standalone-image-predicate' to constrain paragraph
further.  For example, to check for only captioned standalone
images, set it to:

  \(lambda (paragraph) (org-element-property :caption paragraph))"
  (let ((paragraph (case (org-element-type element)
		     (paragraph element)
		     (link (org-export-get-parent element)))))
    (and (eq (org-element-type paragraph) 'paragraph)
	 (or (not (and (boundp 'org-impress-js-standalone-image-predicate)
		       (functionp org-impress-js-standalone-image-predicate)))
	     (funcall org-impress-js-standalone-image-predicate paragraph))
	 (not (let ((link-count 0))
		(org-element-map (org-element-contents paragraph)
		    (cons 'plain-text org-element-all-objects)
		  (lambda (obj) (case (org-element-type obj)
			     (plain-text (org-string-nw-p obj))
			     (link
			      (or (> (incf link-count) 1)
				  (not (org-impress-js-inline-image-p obj info))))
			     (otherwise t)))
		  info 'first-match 'link))))))

(defun org-impress-js-link (link desc info)
  "Transcode a LINK object from Org to HTML.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((home (when (plist-get info :html-link-home)
		 (org-trim (plist-get info :html-link-home))))
	 (use-abs-url (plist-get info :html-link-use-abs-url))
	 (link-org-files-as-html-maybe
	  (function
	   (lambda (raw-path info)
	     "Treat links to `file.org' as links to `file.html', if needed.
           See `org-impress-js-link-org-files-as-html'."
	     (cond
	      ((and org-impress-js-link-org-files-as-html
		    (string= ".org"
			     (downcase (file-name-extension raw-path "."))))
	       (concat (file-name-sans-extension raw-path) "."
		       (plist-get info :html-extension)))
	      (t raw-path)))))
	 (type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (org-string-nw-p desc))
	 (path
	  (cond
	   ((member type '("http" "https" "ftp" "mailto"))
	    (org-link-escape
	     (org-link-unescape
	      (concat type ":" raw-path)) org-link-escape-chars-browser))
	   ((string= type "file")
	    ;; Treat links to ".org" files as ".html", if needed.
	    (setq raw-path
		  (funcall link-org-files-as-html-maybe raw-path info))
	    ;; If file path is absolute, prepend it with protocol
	    ;; component - "file://".
	    (cond ((file-name-absolute-p raw-path)
		   (setq raw-path
			 (concat "file://" (expand-file-name
					    raw-path))))
		  ((and home use-abs-url)
		   (setq raw-path (concat (file-name-as-directory home) raw-path))))
	    ;; Add search option, if any.  A search option can be
	    ;; relative to a custom-id or a headline title.  Append
	    ;; a hash sign to any unresolved option, as it might point
	    ;; to a target.
	    (let ((option (org-element-property :search-option link)))
	      (cond ((not option) raw-path)
		    ((eq (aref option 0) ?#) (concat raw-path option))
		    (t
		     (let ((destination
			    (org-publish-resolve-external-fuzzy-link
			     (org-element-property :path link) option)))
		       (concat raw-path
			       (if (not destination) (concat "#" option)
				 (concat "#sec-"
					 (mapconcat #'number-to-string
						    destination "-")))))))))
	   (t raw-path)))
	 ;; Extract attributes from parent's paragraph.  HACK: Only do
	 ;; this for the first link in parent (inner image link for
	 ;; inline images).  This is needed as long as attributes
	 ;; cannot be set on a per link basis.
	 (attributes-plist
	  (let* ((parent (org-export-get-parent-element link))
		 (link (let ((container (org-export-get-parent link)))
			 (if (and (eq (org-element-type container) 'link)
				  (org-impress-js-inline-image-p link info))
			     container
			   link))))
	    (and (eq (org-element-map parent 'link 'identity info t) link)
		 (org-export-read-attribute :attr_html parent))))
	 (attributes
	  (let ((attr (org-impress-js--make-attribute-string attributes-plist)))
	    (if (org-string-nw-p attr) (concat " " attr) "")))
	 protocol)
    (cond
     ;; Image file.
     ((and org-impress-js-inline-images
	   (org-export-inline-image-p link org-impress-js-inline-image-rules))
      (org-impress-js--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(when destination
	  (format "<a href=\"#%s\"%s>%s</a>"
		  (org-export-solidify-link-text path)
		  attributes
		  (org-export-data (org-element-contents destination) info)))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  ;; ID link points to an external file.
	  (plain-text
	   (let ((fragment (concat "ID-" path))
		 ;; Treat links to ".org" files as ".html", if needed.
		 (path (funcall link-org-files-as-html-maybe
				destination info)))
	     (format "<a href=\"%s#%s\"%s>%s</a>"
		     path fragment attributes (or desc destination))))
	  ;; Fuzzy link points nowhere.
	  ((nil)
	   (format "<i>%s</i>"
		   (or desc
		       (org-export-data
			(org-element-property :raw-link link) info))))
	  ;; Link points to a headline.
	  (headline
	   (let ((href
		  ;; What href to use?
		  (cond
		   ;; Case 1: Headline is linked via it's CUSTOM_ID
		   ;; property.  Use CUSTOM_ID.
		   ((string= type "custom-id")
		    (org-element-property :CUSTOM_ID destination))
		   ;; Case 2: Headline is linked via it's ID property
		   ;; or through other means.  Use the default href.
		   ((member type '("id" "fuzzy"))
		    (format "sec-%s"
			    (mapconcat 'number-to-string
				       (org-export-get-headline-number
					destination info) "-")))
		   (t (error "Shouldn't reach here"))))
		 ;; What description to use?
		 (desc
		  ;; Case 1: Headline is numbered and LINK has no
		  ;; description.  Display section number.
		  (if (and (org-export-numbered-headline-p destination info)
			   (not desc))
		      (mapconcat 'number-to-string
				 (org-export-get-headline-number
				  destination info) ".")
		    ;; Case 2: Either the headline is un-numbered or
		    ;; LINK has a custom description.  Display LINK's
		    ;; description or headline's title.
		    (or desc (org-export-data (org-element-property
					       :title destination) info)))))
	     (format "<a href=\"#%s\"%s>%s</a>"
		     (org-export-solidify-link-text href) attributes desc)))
	  ;; Fuzzy link points to a target or an element.
	  (t
	   (let* ((path (org-export-solidify-link-text path))
		  (org-impress-js-standalone-image-predicate 'org-impress-js--has-caption-p)
		  (number (cond
			   (desc nil)
			   ((org-impress-js-standalone-image-p destination info)
			    (org-export-get-ordinal
			     (org-element-map destination 'link
			       'identity info t)
			     info 'link 'org-impress-js-standalone-image-p))
			   (t (org-export-get-ordinal
			       destination info nil 'org-impress-js--has-caption-p))))
		  (desc (cond (desc)
			      ((not number) "No description for this link")
			      ((numberp number) (number-to-string number))
			      (t (mapconcat 'number-to-string number ".")))))
	     (format "<a href=\"#%s\"%s>%s</a>" path attributes desc))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" path)))
	(format "<a href=\"#%s\"%s%s>%s</a>"
		fragment
		(org-trim
		 (format (concat "class=\"coderef\""
				 " onmouseover=\"CodeHighlightOn(this, '%s');\""
				 " onmouseout=\"CodeHighlightOff(this, '%s');\"")
			 fragment fragment))
		attributes
		(format (org-export-get-coderef-format path desc)
			(org-export-resolve-coderef path info)))))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'html))
     ;; External link with a description part.
     ((and path desc) (format "<a href=\"%s\"%s>%s</a>" path attributes desc))
     ;; External link without a description part.
     (path (format "<a href=\"%s\"%s>%s</a>" path attributes path))
     ;; No path, only description.  Try to do something useful.
     (t (format "<i>%s</i>" desc)))))

;;;; Node Property

(defun org-impress-js-node-property (node-property contents info)
  "Transcode a NODE-PROPERTY element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))

;;;; Paragraph

(defun org-impress-js-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((parent (org-export-get-parent paragraph))
	 (parent-type (org-element-type parent))
	 (style '((footnote-definition " class=\"footpara\"")))
	 (extra (or (cadr (assoc parent-type style)) "")))
    (cond
     ((and (eq (org-element-type parent) 'item)
	   (= (org-element-property :begin paragraph)
	      (org-element-property :contents-begin parent)))
      ;; Leading paragraph in a list item have no tags.
      contents)
     ((org-impress-js-standalone-image-p paragraph info)
      ;; Standalone image.
      (let ((caption
	     (let ((raw (org-export-data
			 (org-export-get-caption paragraph) info))
		   (org-impress-js-standalone-image-predicate
		    'org-impress-js--has-caption-p))
	       (if (not (org-string-nw-p raw)) raw
		 (concat
                  "<span class=\"figure-number\">"
		  (format (org-impress-js--translate "Figure %d:" info)
			  (org-export-get-ordinal
			   (org-element-map paragraph 'link
			     'identity info t)
			   info nil 'org-impress-js-standalone-image-p))
		  "</span> " raw))))
	    (label (org-element-property :name paragraph)))
	(org-impress-js--wrap-image contents info caption label)))
     ;; Regular paragraph.
     (t (format "<p%s>\n%s</p>" extra contents)))))

;;;; Plain List

;; FIXME Maybe arg1 is not needed because <li value="20"> already sets
;; the correct value for the item counter
(defun org-impress-js-begin-plain-list (type &optional arg1)
  "Insert the beginning of the HTML list depending on TYPE.
When ARG1 is a string, use it as the start parameter for ordered
lists."
  (case type
    (ordered
     (format "<ol class=\"org-ol\"%s>"
	     (if arg1 (format " start=\"%d\"" arg1) "")))
    (unordered "<ul class=\"org-ul\">")
    (descriptive "<dl class=\"org-dl\">")))

(defun org-impress-js-end-plain-list (type)
  "Insert the end of the HTML list depending on TYPE."
  (case type
    (ordered "</ol>")
    (unordered "</ul>")
    (descriptive "</dl>")))

(defun org-impress-js-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* (arg1 ;; (assoc :counter (org-element-map plain-list 'item
	 (type (org-element-property :type plain-list)))
    (format "%s\n%s%s"
	    (org-impress-js-begin-plain-list type)
	    contents (org-impress-js-end-plain-list type))))

;;;; Plain Text

(defun org-impress-js-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (let ((all org-impress-js-special-string-regexps)
	e a re rpl start)
    (while (setq a (pop all))
      (setq re (car a) rpl (cdr a) start 0)
      (while (string-match re string start)
	(setq string (replace-match rpl t nil string))))
    string))

(defun org-impress-js-encode-plain-text (text)
  "Convert plain text characters from TEXT to HTML equivalent.
Possible conversions are set in `org-impress-js-protect-char-alist'."
  (mapc
   (lambda (pair)
     (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))
   org-impress-js-protect-char-alist)
  text)

(defun org-impress-js-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((output text))
    ;; Protect following characters: <, >, &.
    (setq output (org-impress-js-encode-plain-text output))
    ;; Handle smart quotes.  Be sure to provide original string since
    ;; OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :html info text)))
    ;; Handle special strings.
    (when (plist-get info :with-special-strings)
      (setq output (org-impress-js-convert-special-strings output)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output
	    (replace-regexp-in-string
	     "\\(\\\\\\\\\\)?[ \t]*\n"
	     (concat (org-impress-js-close-tag "br" nil info) "\n") output)))
    ;; Return value.
    output))


;; Planning

(defun org-impress-js-planning (planning contents info)
  "Transcode a PLANNING element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((span-fmt "<span class=\"timestamp-kwd\">%s</span> <span class=\"timestamp\">%s</span>"))
    (format
     "<p><span class=\"timestamp-wrapper\">%s</span></p>"
     (mapconcat
      'identity
      (delq nil
	    (list
	     (let ((closed (org-element-property :closed planning)))
	       (when closed
		 (format span-fmt org-closed-string
			 (org-translate-time
			  (org-element-property :raw-value closed)))))
	     (let ((deadline (org-element-property :deadline planning)))
	       (when deadline
		 (format span-fmt org-deadline-string
			 (org-translate-time
			  (org-element-property :raw-value deadline)))))
	     (let ((scheduled (org-element-property :scheduled planning)))
	       (when scheduled
		 (format span-fmt org-scheduled-string
			 (org-translate-time
			  (org-element-property :raw-value scheduled)))))))
      " "))))

;;;; Property Drawer

(defun org-impress-js-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to HTML.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (format "<pre class=\"example\">\n%s</pre>" contents)))

;;;; Quote Block

(defun org-impress-js-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "<blockquote>\n%s</blockquote>" contents))

;;;; Section

(defun org-impress-js-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (let* ((class-num (+ (org-export-get-relative-level parent info)
			   (1- org-impress-js-toplevel-hlevel)))
	     (section-number
	      (mapconcat
	       'number-to-string
	       (org-export-get-headline-number parent info) "-")))
        ;; Build return value.
	(format "<div class=\"outline-text-%d\" id=\"text-%s\">\n%s</div>\n</div>"
		class-num
		(or (org-element-property :CUSTOM_ID parent) section-number)
		contents)))))

;;;; Radio Target

(defun org-impress-js-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to HTML.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (let ((id (org-export-solidify-link-text
	     (org-element-property :value radio-target))))
    (org-impress-js--anchor id text)))

;;;; Special Block

(defun org-impress-js-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((block-type (downcase
		      (org-element-property :type special-block)))
	 (contents (or contents ""))
	 (html5-fancy (and (org-impress-js-html5-p info)
			   (plist-get info :html-html5-fancy)
			   (member block-type org-impress-js-html5-elements)))
	 (attributes (org-export-read-attribute :attr_html special-block)))
    (unless html5-fancy
      (let ((class (plist-get attributes :class)))
	(setq attributes (plist-put attributes :class
				    (if class (concat class " " block-type)
				      block-type)))))
    (setq attributes (org-impress-js--make-attribute-string attributes))
    (when (not (equal attributes ""))
      (setq attributes (concat " " attributes)))
    (if html5-fancy
	(format "<%s%s>\n%s</%s>" block-type attributes
		contents block-type)
      (format "<div%s>\n%s\n</div>" attributes contents))))

;;;; Src Block

(defun org-impress-js-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-impress-js--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
	  (caption (org-export-get-caption src-block))
	  (code (org-impress-js-format-code src-block info))
	  (label (let ((lbl (org-element-property :name src-block)))
		   (if (not lbl) ""
		     (format " id=\"%s\""
			     (org-export-solidify-link-text lbl))))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
	(format
	 "<div class=\"org-src-container\">\n%s%s\n</div>"
	 (if (not caption) ""
	   (format "<label class=\"org-src-name\">%s</label>"
		   (org-export-data caption info)))
	 (format "\n<pre class=\"src src-%s\"%s>%s</pre>" lang label code))))))

;;;; Statistics Cookie

(defun org-impress-js-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((cookie-value (org-element-property :value statistics-cookie)))
    (format "<code>%s</code>" cookie-value)))

;;;; Strike-Through

(defun org-impress-js-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to HTML.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format (or (cdr (assq 'strike-through org-impress-js-text-markup-alist)) "%s")
	  contents))

;;;; Subscript

(defun org-impress-js-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sub>%s</sub>" contents))

;;;; Superscript

(defun org-impress-js-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sup>%s</sup>" contents))

;;;; Table Cell

(defun org-impress-js-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-row (org-export-get-parent table-cell))
	 (table (org-export-get-parent-table table-cell))
	 (cell-attrs
	  (if (not org-impress-js-table-align-individual-fields) ""
	    (format (if (and (boundp 'org-impress-js-format-table-no-css)
			     org-impress-js-format-table-no-css)
			" align=\"%s\"" " class=\"%s\"")
		    (org-export-table-cell-alignment table-cell info)))))
    (when (or (not contents) (string= "" (org-trim contents)))
      (setq contents "&#xa0;"))
    (cond
     ((and (org-export-table-has-header-p table info)
	   (= 1 (org-export-table-row-group table-row info)))
      (concat "\n" (format (car org-impress-js-table-header-tags) "col" cell-attrs)
	      contents (cdr org-impress-js-table-header-tags)))
     ((and org-impress-js-table-use-header-tags-for-first-column
	   (zerop (cdr (org-export-table-cell-address table-cell info))))
      (concat "\n" (format (car org-impress-js-table-header-tags) "row" cell-attrs)
	      contents (cdr org-impress-js-table-header-tags)))
     (t (concat "\n" (format (car org-impress-js-table-data-tags) cell-attrs)
		contents (cdr org-impress-js-table-data-tags))))))

;;;; Table Row

(defun org-impress-js-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to HTML.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((rowgroup-number (org-export-table-row-group table-row info))
	   (row-number (org-export-table-row-number table-row info))
	   (start-rowgroup-p
	    (org-export-table-row-starts-rowgroup-p table-row info))
	   (end-rowgroup-p
	    (org-export-table-row-ends-rowgroup-p table-row info))
	   ;; `top-row-p' and `end-rowgroup-p' are not used directly
	   ;; but should be set so that `org-impress-js-table-row-tags' can
	   ;; use them (see the docstring of this variable.)
	   (top-row-p (and (equal start-rowgroup-p '(top))
			   (equal end-rowgroup-p '(below top))))
	   (bottom-row-p (and (equal start-rowgroup-p '(above))
			      (equal end-rowgroup-p '(bottom above))))
	   (rowgroup-tags
	    (cond
	     ;; Case 1: Row belongs to second or subsequent rowgroups.
	     ((not (= 1 rowgroup-number))
	      '("<tbody>" . "\n</tbody>"))
	     ;; Case 2: Row is from first rowgroup.  Table has >=1 rowgroups.
	     ((org-export-table-has-header-p
	       (org-export-get-parent-table table-row) info)
	      '("<thead>" . "\n</thead>"))
	     ;; Case 2: Row is from first and only row group.
	     (t '("<tbody>" . "\n</tbody>")))))
      (concat
       ;; Begin a rowgroup?
       (when start-rowgroup-p (car rowgroup-tags))
       ;; Actual table row
       (concat "\n" (eval (car org-impress-js-table-row-tags))
	       contents
	       "\n"
	       (eval (cdr org-impress-js-table-row-tags)))
       ;; End a rowgroup?
       (when end-rowgroup-p (cdr rowgroup-tags))))))

;;;; Table

(defun org-impress-js-table-first-row-data-cells (table info)
  "Transcode the first row of TABLE.
INFO is a plist used as a communication channel."
  (let ((table-row
	 (org-element-map table 'table-row
	   (lambda (row)
	     (unless (eq (org-element-property :type row) 'rule) row))
	   info 'first-match))
	(special-column-p (org-export-table-has-special-column-p table)))
    (if (not special-column-p) (org-element-contents table-row)
      (cdr (org-element-contents table-row)))))

(defun org-impress-js-table--table.el-table (table info)
  "Format table.el tables into HTML.
INFO is a plist used as a communication channel."
  (when (eq (org-element-property :type table) 'table.el)
    (require 'table)
    (let ((outbuf (with-current-buffer
		      (get-buffer-create "*org-export-table*")
		    (erase-buffer) (current-buffer))))
      (with-temp-buffer
	(insert (org-element-property :value table))
	(goto-char 1)
	(re-search-forward "^[ \t]*|[^|]" nil t)
	(table-generate-source 'html outbuf))
      (with-current-buffer outbuf
	(prog1 (org-trim (buffer-string))
	  (kill-buffer) )))))

(defun org-impress-js-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (case (org-element-property :type table)
    ;; Case 1: table.el table.  Convert it using appropriate tools.
    (table.el (org-impress-js-table--table.el-table table info))
    ;; Case 2: Standard table.
    (t
     (let* ((label (org-element-property :name table))
	    (caption (org-export-get-caption table))
	    (number (org-export-get-ordinal
		     table info nil 'org-impress-js--has-caption-p))
	    (attributes
	     (org-impress-js--make-attribute-string
	      (org-combine-plists
	       (and label (list :id (org-export-solidify-link-text label)))
	       (and (not (org-impress-js-html5-p info))
		    (plist-get info :html-table-attributes))
	       (org-export-read-attribute :attr_html table))))
	    (alignspec
	     (if (and (boundp 'org-impress-js-format-table-no-css)
		      org-impress-js-format-table-no-css)
		 "align=\"%s\"" "class=\"%s\""))
	    (table-column-specs
	     (function
	      (lambda (table info)
		(mapconcat
		 (lambda (table-cell)
		   (let ((alignment (org-export-table-cell-alignment
				     table-cell info)))
		     (concat
		      ;; Begin a colgroup?
		      (when (org-export-table-cell-starts-colgroup-p
			     table-cell info)
			"\n<colgroup>")
		      ;; Add a column.  Also specify it's alignment.
		      (format "\n%s"
			      (org-impress-js-close-tag
			       "col" (concat " " (format alignspec alignment)) info))
		      ;; End a colgroup?
		      (when (org-export-table-cell-ends-colgroup-p
			     table-cell info)
			"\n</colgroup>"))))
		 (org-impress-js-table-first-row-data-cells table info) "\n")))))
       (format "<table%s>\n%s\n%s\n%s</table>"
	       (if (equal attributes "") "" (concat " " attributes))
	       (if (not caption) ""
		 (format (if org-impress-js-table-caption-above
			     "<caption class=\"t-above\">%s</caption>"
			   "<caption class=\"t-bottom\">%s</caption>")
			 (concat
			  "<span class=\"table-number\">"
                          (format (org-impress-js--translate "Table %d:" info) number)
			  "</span> " (org-export-data caption info))))
	       (funcall table-column-specs table info)
	       contents)))))

;;;; Target

(defun org-impress-js-target (target contents info)
  "Transcode a TARGET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((id (org-export-solidify-link-text
	     (org-element-property :value target))))
    (org-impress-js--anchor id)))

;;;; Timestamp

(defun org-impress-js-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-impress-js-plain-text
		(org-timestamp-translate timestamp) info)))
    (format "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">%s</span></span>"
	    (replace-regexp-in-string "--" "&#x2013;" value))))

;;;; Underline

(defun org-impress-js-underline (underline contents info)
  "Transcode UNDERLINE from Org to HTML.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format (or (cdr (assq 'underline org-impress-js-text-markup-alist)) "%s")
	  contents))

;;;; Verbatim

(defun org-impress-js-verbatim (verbatim contents info)
  "Transcode VERBATIM from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format (or (cdr (assq 'verbatim org-impress-js-text-markup-alist)) "%s")
	  (org-impress-js-encode-plain-text (org-element-property :value verbatim))))

;;;; Verse Block

(defun org-impress-js-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  ;; Replace each newline character with line break.  Also replace
  ;; each blank line with a line break.
  (setq contents (replace-regexp-in-string
		  "^ *\\\\\\\\$" (format "%s\n" (org-impress-js-close-tag "br" nil info))
		  (replace-regexp-in-string
		   "\\(\\\\\\\\\\)?[ \t]*\n"
		   (format "%s\n" (org-impress-js-close-tag "br" nil info)) contents)))
  ;; Replace each white space at beginning of a line with a
  ;; non-breaking space.
  (while (string-match "^[ \t]+" contents)
    (let* ((num-ws (length (match-string 0 contents)))
	   (ws (let (out) (dotimes (i num-ws out)
			    (setq out (concat out "&#xa0;"))))))
      (setq contents (replace-match ws nil t contents))))
  (format "<p class=\"verse\">\n%s</p>" contents))


;;; Filter Functions

(defun org-impress-js-final-function (contents backend info)
  "Filter to indent the HTML and convert HTML entities."
  (with-temp-buffer
    (insert contents)
    (set-auto-mode t)
    (if org-impress-js-indent
	(indent-region (point-min) (point-max)))
    (when org-impress-js-use-unicode-chars
      (require 'mm-url)
      (mm-url-decode-entities))
    (buffer-substring-no-properties (point-min) (point-max))))


;;; End-user functions

;;;###autoload
(defun org-impress-js-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'html "*Org HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-impress-js-convert-region-to-html ()
  "Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in an HTML buffer and use this
command to convert it."
  (interactive)
  (org-export-replace-region-by 'html))

;;;###autoload
(defun org-impress-js-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." org-impress-js-extension))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-impress-js-coding-system))
    (org-export-to-file 'impress-js file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-impress-js-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'html filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-impress-js-extension "html"))
		      plist pub-dir))


(provide 'ox-impress-js)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-impress-js.el ends here
