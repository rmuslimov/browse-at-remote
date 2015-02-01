
(require 'browse-at-remote)

(ert-deftest get-git-repo-url-test ()
  "Generate github repo url from various kind of origin"

  (should (equal (get-url-for-origin "git@github.com:getgoing/airborne.git")
                 (cons `"github.com" `"https://github.com/getgoing/airborne")))
  (should (equal (get-url-for-origin "git@bitbucket.org:some/bome.git")
                 (cons `"bitbucket.org" `"https://bitbucket.org/some/bome")))
  (should (equal (get-url-for-origin "git@github.com:someplace/with-dash.git")
                 (cons `"github.com" `"https://github.com/someplace/with-dash")))
  )

(ert-deftest get-https-repo-url-test ()
  "Test origins having https in the beginning"

  (should (equal
           (get-url-for-origin
            "https://rmuslimov@bitbucket.org/some/bome.git")
           (cons `"bitbucket.org" `"https://bitbucket.org/some/bome"))))
