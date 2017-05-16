;;; Example/test file

(require 'apiwrap)

(defun apiwrap-test--gen-link (link-alist)
  (format "https://developer.github.com/v3/%s"
          (alist-get 'link link-alist)))

(apiwrap-new-backend "GitHub" "my-github-wrapper"
                     '((repo . "REPO is a special object.")
                       (issue . "ISSUE is a special object."))
                     :get #'ghub-get :put #'ghub-put :head #'ghub-head
                     :post #'ghub-post :patch #'ghub-patch :delete #'ghub-delete
                     :link #'apiwrap-test--gen-link)

(ert-deftest apiwrap-macros ()
  (should (equal 'my-github-wrapper-get-repos-owner-repo-issues
                 (defapiget-my-github-wrapper "/repos/:owner/:repo/issues"
                   "List issues for a repository."
                   "issues/#list-issues-for-a-repository"
                   (repo) "/repos/:repo.owner.login/:repo.name/issues")))

  (should (equal 'my-github-wrapper-get-repos-owner-repo-issues-number
                 (defapiget-my-github-wrapper "/repos/:owner/:repo/issues/:number"
                   "List issues for a repository."
                   "issues/#list-issues-for-a-repository"
                   (repo issue) "/repos/:repo.owner.login/:repo.name/issues/:issue.number")))

  (should (equal 'my-github-wrapper-get-repos-owner-repo-issues-number-comments
                 (defapiget-my-github-wrapper "/repos/:owner/:repo/issues/:number/comments"
                   "List comments on an issue."
                   "issues/comments/#list-comments-on-an-issue"
                   (repo issue) "/repos/:repo.owner.login/:repo.name/issues/:issue.number/comments"))))

(ert-deftest apiwrap-usage ()
  (let* ((my-repo '((owner (login . "vermiculus"))
                    (name . "apiwrap.el")))
         (my-issue (my-github-wrapper-get-repos-owner-repo-issues-number my-repo '((number . 1))))
         (all-issues (my-github-wrapper-get-repos-owner-repo-issues my-repo :state "all")))
    (should (my-github-wrapper-get-repos-owner-repo-issues-number-comments my-repo my-issue))
    (should (= (alist-get 'id my-issue)
               (alist-get 'id (car all-issues))))))

(ert-deftest apiwrap-resolve-params ()
  (let ((obj '((name . "Hello-World")
               (owner (login . "octocat"))))
        (tests '(("/repos/:owner.login/:name/issues" . "/repos/octocat/Hello-World/issues")
                 ("/repos/:owner.login/:name" . "/repos/octocat/Hello-World")
                 ("/:owner.login/:name/issues" . "/octocat/Hello-World/issues")
                 ("/:owner.login/:name" . "/octocat/Hello-World")
                 (":owner.login" . "octocat")
                 ("/:owner.login" . "/octocat")
                 ("/:owner.login/" . "/octocat/"))))
    (dolist (test tests)
      (should (string= (cdr test) (eval (apiwrap-resolve-api-params 'obj (car test)))))))
  (should (string= (let ((obj '((name . "hello^world")
                                (owner (login . "octo^cat")))))
                     (eval (ghub-resolve-api-params 'obj "/:owner.login/:name")))
                   "/octo%5Ecat/hello%5Eworld")))

(ert-deftest apiwrap-plist-to-alist ()
  (should
   (null (cl-set-difference
          (apiwrap-plist->alist '(:one two :three four))
          '((one . two) (three . four))
          :test #'equal))))
