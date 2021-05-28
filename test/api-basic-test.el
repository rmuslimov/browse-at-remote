
(require 'browse-at-remote)

(ert-deftest get-git-repo-url-test ()
  "Generate github repo url from various kind of origin"

  (should (equal (browse-at-remote--get-url-from-remote "ssh://git@bitbucket.org:peta/project.git")
                 (cons `"bitbucket.org" `"https://bitbucket.org/peta/project")))
  (should (equal (browse-at-remote--get-url-from-remote "git@github.com:getgoing/airborne.git")
                 (cons `"github.com" `"https://github.com/getgoing/airborne")))
  (should (equal (browse-at-remote--get-url-from-remote "git@github.com:env0der/dotemacs.git")
                 (cons `"github.com" `"https://github.com/env0der/dotemacs")))
  (should (equal (browse-at-remote--get-url-from-remote "git@bitbucket.org:some/bome.git")
                 (cons `"bitbucket.org" `"https://bitbucket.org/some/bome")))
  (should (equal (browse-at-remote--get-url-from-remote "git@github.com:someplace_with_underscores/with_underscores.el.git")
                 (cons `"github.com" `"https://github.com/someplace_with_underscores/with_underscores.el")))
  (should (equal (browse-at-remote--get-url-from-remote "git@github.com:someplace/with-dash.el.git")
                 (cons `"github.com" `"https://github.com/someplace/with-dash.el")))
  (should (equal (browse-at-remote--get-url-from-remote "git@github.com:someplace/wi2th-dash.el.git")
                 (cons `"github.com" `"https://github.com/someplace/wi2th-dash.el")))
  (should (equal (browse-at-remote--get-url-from-remote "ssh://git.example.com:8080/someplace/wi2th-dash.el")
                 (cons `"git.example.com" `"https://git.example.com/someplace/wi2th-dash.el")))
  (should (equal (browse-at-remote--get-url-from-remote "ssh://user@git.example.com:8080/someplace/wi2th-dash.el")
                 (cons `"git.example.com" `"https://git.example.com/someplace/wi2th-dash.el")))
  (should (equal (browse-at-remote--get-url-from-remote "git+ssh://git.example.com:8080/someplace/wi2th-dash.el")
                 (cons `"git.example.com" `"https://git.example.com/someplace/wi2th-dash.el")))
  (should (equal (browse-at-remote--get-url-from-remote "git@gitlab.com:someplace/double-nested/wi2th-dash.el.git")
                 (cons `"gitlab.com" `"https://gitlab.com/someplace/double-nested/wi2th-dash.el")))
  )

(ert-deftest get-https-repo-url-test ()
  "Test origins having https in the beginning"

  (should (equal (browse-at-remote--get-url-from-remote "https://rmuslimov@bitbucket.org/some/bome.git")
				 (cons `"bitbucket.org" `"https://bitbucket.org/some/bome")))
  (should (equal (browse-at-remote--get-url-from-remote "https://github.com/syl20bnr/spacemacs")
				 (cons `"github.com" `"https://github.com/syl20bnr/spacemacs")))
  (should (equal (browse-at-remote--get-url-from-remote "https://github.com/rejeep/prodigy.el.git")
				 (cons `"github.com" `"https://github.com/rejeep/prodigy.el")))
  (should (equal (browse-at-remote--get-url-from-remote "https://github.com/rejeep/pro-digy.el.git")
				 (cons `"github.com" `"https://github.com/rejeep/pro-digy.el")))
  (should (equal (browse-at-remote--get-url-from-remote "https://github.com/with_underscores/pro-digy_underscores.el.git")
				 (cons `"github.com" `"https://github.com/with_underscores/pro-digy_underscores.el")))
  (should (equal (browse-at-remote--get-url-from-remote "https://github.com/rmuslimov/browse-at-remote.git")
				 (cons `"github.com" `"https://github.com/rmuslimov/browse-at-remote")))
  (should (equal (browse-at-remote--get-url-from-remote "https://gitlab.com/someplace/double-nested/wi2th-dash.el.git")
                 (cons `"gitlab.com" `"https://gitlab.com/someplace/double-nested/wi2th-dash.el")))
  (should (equal (browse-at-remote--get-url-from-remote "http://git.example.com:8000/someplace/wi2th-dash.el.git")
                 (cons `"git.example.com:8000" `"http://git.example.com:8000/someplace/wi2th-dash.el")))
  (should (equal (browse-at-remote--get-url-from-remote "https://git.example.com:8000/someplace/wi2th-dash.el.git")
                 (cons `"git.example.com:8000" `"https://git.example.com:8000/someplace/wi2th-dash.el")))
  )

(ert-deftest get-https-repo-url-without-ending ()
  (should (equal (browse-at-remote--get-url-from-remote "https://github.com/rmuslimov/browse-at-remote")
				 (cons `"github.com" `"https://github.com/rmuslimov/browse-at-remote")))
  (should (equal (browse-at-remote--get-url-from-remote "https://github.com/rmus2limov/brows2e-at-remote")
				 (cons `"github.com" `"https://github.com/rmus2limov/brows2e-at-remote")))
  (should (equal (browse-at-remote--get-url-from-remote "git@github.com:someplace/without-ending")
                                 (cons `"github.com" `"https://github.com/someplace/without-ending")))
  )

(ert-deftest get-repo-url-pagure ()
  (let ((repo-url "https://pagure.io/copr/copr")
	(location "master")
	(filename "frontend/coprs_frontend/manage.py"))

      (should (equal
	(browse-at-remote--format-region-url-as-pagure repo-url location filename)
	"https://pagure.io/copr/copr/blob/master/f/frontend/coprs_frontend/manage.py"))

      (should (equal
	(browse-at-remote--format-region-url-as-pagure repo-url location filename 12)
	"https://pagure.io/copr/copr/blob/master/f/frontend/coprs_frontend/manage.py#_12"))

      (should (equal
	(browse-at-remote--format-region-url-as-pagure repo-url location filename 12 14)
	"https://pagure.io/copr/copr/blob/master/f/frontend/coprs_frontend/manage.py#_12-14"))

      (should (equal
        (browse-at-remote--format-region-url-as-pagure repo-url location "README.md" 12 14)
        "https://pagure.io/copr/copr/blob/master/f/README.md?text=True#_12-14"))

      (should (equal
        (browse-at-remote--format-region-url-as-pagure "https://pagure.io/forks/frostyx/copr/copr" location filename)
        "https://pagure.io/fork/frostyx/copr/copr/blob/master/f/frontend/coprs_frontend/manage.py"))))

(ert-deftest get-repo-url-ado ()
  ;; GreatBanana - organization
  ;; Forest - project
  ;; Gorillas - repository
  (let ((repo-url "https://vs-ssh.visualstudio.com/v3/GreatBanana/Forest/Gorillas")
	(location "master")
	(filename "kind/silverback.el"))

      (should (equal
	(browse-at-remote--format-region-url-as-ado repo-url location filename)
	"https://GreatBanana.visualstudio.com/Forest/_git/Gorillas?version=GBmaster&path=/kind/silverback.el"))

      (should (equal
	(browse-at-remote--format-region-url-as-ado repo-url location filename 12)
	"https://GreatBanana.visualstudio.com/Forest/_git/Gorillas?version=GBmaster&path=/kind/silverback.el&line=12&lineStartColumn=1&lineEndColumn=1"))

      (should (equal
	(browse-at-remote--format-region-url-as-ado repo-url location filename 12 14)
	"https://GreatBanana.visualstudio.com/Forest/_git/Gorillas?version=GBmaster&path=/kind/silverback.el&line=12&lineEnd=15&lineStartColumn=1&lineEndColumn=1"))))

(ert-deftest format-region-url-gitiles ()
  (let ((repo-url "https://chromium-review.googlesource.com/chromiumos/platform/ec")
        (location "main")
        (filename "common/printf.c"))
    (should (equal
             (browse-at-remote--format-region-url-as-gitiles repo-url location filename)
             "https://chromium.googlesource.com/chromiumos/platform/ec/+/main/common/printf.c"))
    (should (equal
             (browse-at-remote--format-region-url-as-gitiles repo-url location filename 102)
             "https://chromium.googlesource.com/chromiumos/platform/ec/+/main/common/printf.c#102"))
    (should (equal
             (browse-at-remote--format-region-url-as-gitiles repo-url location filename 102 110)
             "https://chromium.googlesource.com/chromiumos/platform/ec/+/main/common/printf.c#102"))))

(ert-deftest format-commit-url-gitiles ()
  (let ((repo-url "https://chromium-review.googlesource.com/chromiumos/platform/ec")
        (commit "bf9979b91599ffc76018d60e780d19fa8d266ac0"))
    (should (equal
             (browse-at-remote--format-commit-url-as-gitiles repo-url commit)
             "https://chromium.googlesource.com/chromiumos/platform/ec/+/bf9979b91599ffc76018d60e780d19fa8d266ac0^!/"))))
