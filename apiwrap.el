;;; apiwrap.el --- api-wrapping tools      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: tools, maint, convenience
;; Homepage: https://github.com/vermiculus/apiwrap.el
;; Package-Requires: ((emacs "25"))
;; Package-Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; API-Wrap.el is a tool to interface with the APIs of your favorite
;; services.  These macros make it easy to define efficient and
;; consistently-documented Elisp functions that use a natural syntax
;; for application development.

;;; Code:

(require 'cl-lib)

(defun apiwrap-resolve-api-params (object url &optional noencode)
  "Resolve parameters in URL to values in OBJECT.

Unless NOENCODE is non-nil, OBJECT values will be passed through
`url-encode-url'.

Example:

    \(apiwrap-resolve-api-params
        '\(\(name . \"Hello-World\"\)
          \(owner \(login . \"octocat\"\)\)\)
      \"/repos/:owner.login/:name/issues\"\)

    ;; \"/repos/octocat/Hello-World/issues\"

"
  (declare (indent 1))
  ;; Yes I know it's hacky, but it works and it's compile-time
  ;; (which is to say: pull-requests welcome!)
  (macroexp--expand-all
   `(let-alist ,object
      ,(let ((in-string t))
         (with-temp-buffer
           (insert url)
           (goto-char 0)
           (insert "(concat \"")
           (while (search-forward ":" nil t)
             (goto-char (1- (point)))
             (insert "\" ")
             (unless noencode (insert "(url-encode-url "))
             (insert ".")
             (setq in-string nil)
             (delete-char 1)
             (when (search-forward "/" nil t)
               (goto-char (1- (point)))
               (unless noencode (insert ")"))
               (insert " \"")
               (setq in-string t)))
           (goto-char (point-max))
           (if in-string (insert "\"")
             (unless noencode (insert ")")))
           (insert ")")
           (delete "" (read (buffer-string))))))))

(defun apiwrap-plist->alist (plist)
  "Convert PLIST to an alist.
Alist keys will be symbols and its values will be coerced into
strings."
  (when (= 1 (mod (length plist) 2))
    (error "bad plist"))
  (apiwrap--plist->alist-internal plist nil))

(defun apiwrap--plist->alist-internal (plist alist-build)
  "Recursively build ALIST-BUILD from PLIST. "
  (if plist (cons (let ((key (car plist))
                        (val (cadr plist)))
                    (cons (apiwrap--kw->sym key) val))
                  (apiwrap--plist->alist-internal (cddr plist) alist-build))))

(defun apiwrap--kw->sym (kw)
  "Convert a keyword to a symbol."
  (intern (substring (symbol-name kw) 1)))

(defun apiwrap--defresource-doc (doc object-param-doc method external-resource link)
  "Documentation string for resource-wrapping functions created
by `apiwrap--defresource'"
  (format "%s

%sPARAMS is a plist of parameters appended to the method call.

DATA is a data structure to be sent with this request.  If it's
not required, it can simply be omitted.

%s

This generated function wraps

    %s %s

which is documented at

    URL `%s'"
          doc (or (and (stringp object-param-doc)
                       (concat object-param-doc "\n\n"))
                  "")
          (make-string 20 ?-)
          (upcase (symbol-name method))
          external-resource link))

(defun apiwrap--defmethod-doc (service-name method)
  "Documentation string for macros created by
`apiwrap-new-backend'"
  (apply #'format "Define a new %s resource wrapper function.

RESOURCE is the API endpoint as written in the %s API
documentation.  This string will be used to create the symbol for
the new function.

DOC is a documentation string for the new function.  Usually,
this can be copied from the %s API documentation.

VERSION is the %s API version this resource is from.  It is
passed to LINK-FUNC as the first argument.

LINK is a link to the %s API documentation.  It is passed to
LINK-FUNC as the second argument.

If non-nil, OBJECT is a symbol that will be used to resolve
parameters in the resource and will be a required argument of the
new function.  If nil, it is ignored.  For details on this
behavior, see `apiwrap-resolve-api-params'.

If non-nil, INTERNAL-RESOURCE is the resource-string used to
resolve OBJECT to the ultimate call."
         (upcase (symbol-name method))
         (make-list 4 service-name)))

(defun apiwrap--defresource (prefix method api-func link-func
                                    standard-parameters
                                    external-resource doc version link
                                    object internal-resource)
  "Define a new resource-wrapping function.

PREFIX, METHOD, and EXTERNAL-RESOURCE will be combined to form
the function name.  METHOD is one of `get', `put', `head',
`post', `patch', or `delete' and EXTERNAL-RESOURCE should be the
calling syntax as advertised by the API.  This naming scheme is
intended to make it easy for application developers to find your
wrappers and should be followed, but you can consider aliasing
the generated function name to a different name if you so
choose.

API-FUNC is the primitive function of your API that corresponds
to METHOD.  See `apiwrap-new-backend' for details.

LINK-FUNC, see `apiwrap-new-backend'.

STANDARD-PARAMETERS, see `apiwrap-new-backend'.

DOC is a documentation string for this resource.

VERSION is the API version this resource is from.  It is passed
to LINK-FUNC as the first argument.

LINK is a link to the API documentation.  It is passed
to LINK-FUNC as the second argument.

OBJECT is a symbol that will be used to resolve dynamic
references in the resource-string.  Its documentation will be
inserted into the docstring the the new function.  When the
function is called, the parameter's value will be used to build
the actual call to the API.

INTERNAL-RESOURCE, if different from EXTERNAL-RESOURCE, will be
used to resolve OBJECT instead.  This is useful in the likely
event that the advertised resource syntax does not align with the
structure of the object it works with.  For example, GitHub's

    GET /repos/:owner/:repo/issues resource

would be created thusly

    \(prefix-defget \"/repos/:owner/:repo/issues\"
      \"List issues for a repository.\"
      3 \"issues/#list-issues-for-a-repository\"
      repo \"/repos/:owner.login/:name/issues\"\)

defining a function called `prefix-get-repos-owner-repo-issues'
and taking an object with the structure

    \(\(owner \(login . \"octocat\"\)\)
     \(name . \"hello-world\"\)

See the documentation of `apiwrap-resolve-api-params' for more
details on that behavior."
  (declare (indent defun))
  (let* ((internal-resource (or internal-resource external-resource))
         (symbol external-resource)
         (symbol (replace-regexp-in-string "/" "-" symbol t t))
         (symbol (replace-regexp-in-string ":" ""  symbol t t))
         (symbol (intern (concat prefix "-" (symbol-name method) symbol)))
         (args (append (when object (list object)) '(&optional data &rest params)))
         (object-param-doc (alist-get object standard-parameters))
         (link (funcall link-func version link)))
    (when (and object (not object-param-doc))
      (error "Standard parameter `%s' not documented" object))
    `(prog1
         (defun ,symbol ,args ,(apiwrap--defresource-doc
                                doc object-param-doc method
                                external-resource link)
                (declare (indent defun))
                (apply ',api-func
                       ,(apiwrap-resolve-api-params object internal-resource)
                       (if (keywordp data)
                           (list (apiwrap-plist->alist (cons data params)))
                         (list (apiwrap-plist->alist params) data))))
       ;; I feel like the following will be useful someday -- perhaps
       ;; some sort of report on what API end-points are currently
       ;; available
       (put ',symbol 'apiwrap-prefix       ',(intern prefix))
       (put ',symbol 'apiwrap-version       ,version)
       (put ',symbol 'apiwrap-method       ',method)
       (put ',symbol 'apiwrap-endpoint      ,external-resource)
       (put ',symbol 'apiwrap-documentation ,link))))

(defmacro apiwrap-new-backend (service-name prefix standard-parameters link-func
                                            get-func put-func head-func post-func patch-func delete-func)
  "Define a new API backend.

SERVICE-NAME is the name of the service this backend will wrap.
It will be used in docstrings of the primitive method macros.

PREFIX is the prefix to use for the macros and for the
resource-wrapping functions.

STANDARD-PARAMETERS is an alist of standard parameters that can
be used to resolve resource URLs like `/users/:user/info'.  Each
key of the alist is the parameter name (as a symbol) and its
value is the documentation to insert in the docstring of
resource-wrapping functions.

LINK-FUNC is a function to generate documentation URLs.  It takes
a version (number) and a link (string) and returns a fully
qualified URL that links to the official documentation of the
resource.

GET-FUNC, PUT-FUNC, HEAD-FUNC, POST-FUNC, PATCH-FUNC, and
DELETE-FUNC are the primitive method-functions of your API.  See
package `ghub' as an example of the kinds of primitives these
macros are design for; you may wish to consider writing wrappers.
Each function is expected to take a resource-string as the first
parameter.  The second parameter should be an alist of parameters
to the resource.  The third parameter should be an alist of data
for the resource (e.g., for posting)."
  (declare (indent defun))
  (when (memq nil (mapcar #'functionp (list get-func put-func head-func post-func patch-func delete-func)))
    (byte-compile-warn "one or more API primitives for %S service not known to exist" service-name))
  (unless (functionp link-func)
    (error "invalid link function"))

  `(prog1 ,service-name
     ,@(cl-loop for method in `((get . ,get-func) (put . ,put-func) (head . ,head-func)
                                (post . ,post-func) (patch . ,patch-func) (delete . ,delete-func))
                collect
                (let ((symbol (intern (concat prefix "-def" (symbol-name (car method))))))
                  `(defmacro ,symbol (resource doc version link &optional object internal-resource)
                     ,(apiwrap--defmethod-doc service-name (car method))
                     (declare (indent defun) (doc-string 2))
                     (apiwrap--defresource ,prefix ',(car method) ',(cdr method)
                                           ,link-func ',standard-parameters resource doc version link
                                           object internal-resource))))))

(provide 'apiwrap)
;;; apiwrap.el ends here
