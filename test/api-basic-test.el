
(require 'browse-at-remote)

(ert-deftest get-git-repo-url-test ()
  "Generate github repo url from various kind of origin"

  (should (equal (browse-at-remote/get-url-from-origin "git@github.com:getgoing/airborne.git")
                 (cons `"github.com" `"https://github.com/getgoing/airborne")))
  (should (equal (browse-at-remote/get-url-from-origin "git@github.com:env0der/dotemacs.git")
                 (cons `"github.com" `"https://github.com/env0der/dotemacs")))
  (should (equal (browse-at-remote/get-url-from-origin "git@bitbucket.org:some/bome.git")
                 (cons `"bitbucket.org" `"https://bitbucket.org/some/bome")))
  (should (equal (browse-at-remote/get-url-from-origin "git@github.com:someplace/with-dash.el.git")
                 (cons `"github.com" `"https://github.com/someplace/with-dash.el")))
  (should (equal (browse-at-remote/get-url-from-origin "git@github.com:someplace/wi2th-dash.el.git")
                 (cons `"github.com" `"https://github.com/someplace/wi2th-dash.el")))
  )

(ert-deftest get-https-repo-url-test ()
  "Test origins having https in the beginning"

  (should (equal (browse-at-remote/get-url-from-origin "https://rmuslimov@bitbucket.org/some/bome.git")
				 (cons `"bitbucket.org" `"https://bitbucket.org/some/bome")))
  (should (equal (browse-at-remote/get-url-from-origin "https://github.com/syl20bnr/spacemacs")
				 (cons `"github.com" `"https://github.com/syl20bnr/spacemacs")))
  (should (equal (browse-at-remote/get-url-from-origin "https://github.com/rejeep/prodigy.el.git")
				 (cons `"github.com" `"https://github.com/rejeep/prodigy.el")))
  (should (equal (browse-at-remote/get-url-from-origin "https://github.com/rejeep/pro-digy.el.git")
				 (cons `"github.com" `"https://github.com/rejeep/pro-digy.el")))
  (should (equal (browse-at-remote/get-url-from-origin "https://github.com/rmuslimov/browse-at-remote.git")
				 (cons `"github.com" `"https://github.com/rmuslimov/browse-at-remote")))
  )

(ert-deftest get-https-repo-url-without-ending ()
  (should (equal (browse-at-remote/get-url-from-origin "https://github.com/rmuslimov/browse-at-remote")
				 (cons `"github.com" `"https://github.com/rmuslimov/browse-at-remote")))
  (should (equal (browse-at-remote/get-url-from-origin "https://github.com/rmus2limov/brows2e-at-remote")
				 (cons `"github.com" `"https://github.com/rmus2limov/brows2e-at-remote")))
  (should (equal (browse-at-remote/get-url-from-origin "git@github.com:someplace/without-ending")
                                 (cons `"github.com" `"https://github.com/someplace/without-ending")))
  )
