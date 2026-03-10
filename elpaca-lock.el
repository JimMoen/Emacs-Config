((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id ace-window :type git :protocol https
                       :inherit t :depth treeless :ref "77115afc1b0b9f633084cf7479c767988106c196"))
 (aider :source "elpaca-menu-lock-file" :recipe
        (:package "aider" :fetcher github :repo "tninja/aider.el" :files (:defaults "snippets")
                  :source "elpaca-menu-lock-file" :id aider :type git :protocol https :inherit t
                  :depth treeless :ref "ac10526c5cfcc2f4288d3539853ff23f4ee7f915"))
 (aio :source "elpaca-menu-lock-file" :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
                ("aio.el" "README.md" "UNLICENSE") :source "elpaca-menu-lock-file" :id aio :type git
                :protocol https :inherit t :depth treeless :ref
                "0e94a06bb035953cbbb4242568b38ca15443ad4c"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia" :files
                     (:defaults ("scripts" "scripts/formatters")) :source "elpaca-menu-lock-file"
                     :id apheleia :type git :protocol https :inherit t :depth treeless :ref
                     "011e7b999552f3c0730035183a4a6dfb60c10182"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id avy :type git :protocol https :inherit t :depth
                treeless :ref "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (beacon :source "elpaca-menu-lock-file" :recipe
         (:package "beacon" :fetcher github :repo "Malabarba/beacon" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                              "LICENSE" "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id beacon :type git :protocol https :inherit t
                   :depth treeless :ref "85261a928ae0ec3b41e639f05291ffd6bf7c231c"))
 (c3-ts-mode :source "elpaca-menu-lock-file" :recipe
             (:source "elpaca-menu-lock-file" :package "c3-ts-mode" :id c3-ts-mode :host github
                      :repo "c3lang/c3-ts-mode" :files ("*.el") :type git :protocol https :inherit t
                      :depth treeless :ref "68325b8b5d797695bec4952a2f0f878b05f2ec0e"))
 (cal-china-x :source "elpaca-menu-lock-file" :recipe
              (:package "cal-china-x" :repo "xwl/cal-china-x" :fetcher github :files
                        (:defaults "jieqi.txt") :source "elpaca-menu-lock-file" :id cal-china-x
                        :type git :protocol https :inherit t :depth treeless :ref
                        "e9e3067134b0606bfe2a6925d7488c4b24ce81d5"))
 (cargo-mode :source "elpaca-menu-lock-file" :recipe
             (:package "cargo-mode" :fetcher github :repo "ayrat555/cargo-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id cargo-mode :type git :protocol https
                       :inherit t :depth treeless :ref "33528954218f8957a26f3fef506c3537823d569d"))
 (cfrs :source "elpaca-menu-lock-file" :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id cfrs :type git :protocol https :inherit t
                 :depth treeless :ref "981bddb3fb9fd9c58aed182e352975bd10ad74c8"))
 (chinese-word-at-point :source "elpaca-menu-lock-file" :recipe
                        (:package "chinese-word-at-point" :repo
                                  "xuchunyang/chinese-word-at-point.el" :fetcher github :files
                                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                   "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                  :source "elpaca-menu-lock-file" :id chinese-word-at-point :type
                                  git :protocol https :inherit t :depth treeless :ref
                                  "8223d7439e005555b86995a005b225ae042f0538"))
 (cns :source "elpaca-menu-lock-file" :recipe
      (:source "elpaca-menu-lock-file" :package "cns" :id cns :host github :repo
               "kanglmf/emacs-chinese-word-segmentation" :files ("cns.el") :build
               ((:before elpaca-check-version +cns-build)) :type git :protocol https :inherit t
               :depth treeless :ref "fd2e711dc7fd957267df5056a57575b3eb090ac4"))
 (colorful-mode :source "elpaca-menu-lock-file" :recipe
                (:package "colorful-mode" :repo
                          ("https://github.com/DevelopmentCool2449/colorful-mode" . "colorful-mode")
                          :tar "1.2.5" :host gnu :files ("*" (:exclude ".git")) :source
                          "elpaca-menu-lock-file" :id colorful-mode :type git :protocol https
                          :inherit t :depth treeless :ref "0c7b73b840c0690379f1fd4f6a712c2f779bc7a4"))
 (company :source "elpaca-menu-lock-file" :recipe
          (:package "company" :fetcher github :repo "company-mode/company-mode" :files
                    (:defaults "icons" ("images/small" "doc/images/small/*.png")) :source
                    "elpaca-menu-lock-file" :id company :type git :protocol https :inherit t :depth
                    treeless :ref "42d3897308a992cd2268ba2d4e2ec013fc6c961e"))
 (company-box :source "elpaca-menu-lock-file" :recipe
              (:package "company-box" :fetcher github :repo "sebastiencs/company-box" :files
                        (:defaults "images") :source "elpaca-menu-lock-file" :id company-box :type
                        git :protocol https :inherit t :depth treeless :ref
                        "c4f2e243fba03c11e46b1600b124e036f2be7691"))
 (company-prescient :source "elpaca-menu-lock-file" :recipe
                    (:package "company-prescient" :fetcher github :repo
                              "radian-software/prescient.el" :files ("company-prescient.el") :source
                              "elpaca-menu-lock-file" :id company-prescient :type git :protocol
                              https :inherit t :depth treeless :ref
                              "87e2d2f2ddf24f591a5f70cc90d2afb4537caa18"))
 (company-tabnine :source "elpaca-menu-lock-file" :recipe
                  (:package "company-tabnine" :repo "TommyX12/company-tabnine" :fetcher github
                            :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id company-tabnine :type git :protocol
                            https :inherit t :depth treeless :ref
                            "083d290c19b874f2ae139c77a726932e618e29b2"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
              "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                        "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :id cond-let :type git :protocol https :inherit t
             :depth treeless :ref "8bf87d45e169ebc091103b2aae325aece3aa804d"))
 (copilot :source "elpaca-menu-lock-file" :recipe
          (:package "copilot" :fetcher github :repo "copilot-emacs/copilot.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id copilot :type git :protocol https :inherit t
                    :depth treeless :ref "59a4a292236ac9bea8756c0a0613b750b14d91eb"))
 (copilot-chat :source "elpaca-menu-lock-file" :recipe
               (:package "copilot-chat" :fetcher github :repo "chep/copilot-chat.el" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id copilot-chat :type git :protocol https
                         :inherit t :depth treeless :ref "1a1e1d5c7c4437679970ae4eb231760ce3f52b01"))
 (counsel :source "elpaca-menu-lock-file" :recipe
          (:package "counsel" :repo "abo-abo/swiper" :fetcher github :files ("counsel.el") :source
                    "elpaca-menu-lock-file" :id counsel :type git :protocol https :inherit t :depth
                    treeless :ref "631df3f68c79cca2a26bc8ea1a98a332ef53751e"))
 (counsel-projectile :source "elpaca-menu-lock-file" :recipe
                     (:package "counsel-projectile" :fetcher github :repo
                               "ericdanan/counsel-projectile" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :id counsel-projectile :type git
                               :protocol https :inherit t :depth treeless :ref
                               "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b"))
 (counsel-tramp :source "elpaca-menu-lock-file" :recipe
                (:package "counsel-tramp" :repo "masasam/emacs-counsel-tramp" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id counsel-tramp :type git :protocol
                          https :inherit t :depth treeless :ref
                          "70dcc6b9da5e76fefbc92646e7d780b2a06ca93f"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files ("dash.el" "dash.texi")
                 :source "elpaca-menu-lock-file" :id dash :type git :protocol https :inherit t
                 :depth treeless :ref "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (dashboard :source "elpaca-menu-lock-file" :recipe
            (:package "dashboard" :fetcher github :repo "emacs-dashboard/dashboard" :files
                      (:defaults "banners") :source "elpaca-menu-lock-file" :id dashboard :type git
                      :protocol https :inherit t :depth treeless :ref
                      "676be25d9b0382bd3253c9119b258a28cdcfc5fb"))
 (dashboard-ls :source "elpaca-menu-lock-file" :recipe
               (:package "dashboard-ls" :repo "emacs-dashboard/dashboard-ls" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id dashboard-ls :type git :protocol https
                         :inherit t :depth treeless :ref "e6f1ae4e4f4efbc1d3c4cff84b220f44e963e6ab"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id diff-hl :type git :protocol https :inherit t
                    :depth treeless :ref "bb9af85441b0cbb3281268d30256d50f0595ebfe"))
 (dired-git-info :source "elpaca-menu-lock-file" :recipe
                 (:package "dired-git-info" :repo
                           ("https://github.com/clemera/dired-git-info" . "dired-git-info") :tar
                           "0.3.1" :host gnu :files ("*" (:exclude ".git")) :source
                           "elpaca-menu-lock-file" :id dired-git-info :type git :protocol https
                           :inherit t :depth treeless :ref
                           "91d57e3a4c5104c66a3abc18e281ee55e8979176"))
 (dired-single :source "elpaca-menu-lock-file" :recipe
               (:source "elpaca-menu-lock-file" :package "dired-single" :id dired-single :host
                        github :repo "emacsattic/dired-single" :files ("*.el") :type git :protocol
                        https :inherit t :depth treeless :ref
                        "60fce6599326e12cc2033c28d50b8bf6c6ba164a"))
 (doom-modeline :source "elpaca-menu-lock-file" :recipe
                (:package "doom-modeline" :repo "seagle0128/doom-modeline" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id doom-modeline :type git :protocol
                          https :inherit t :depth treeless :ref
                          "febde435ae62b36466154c0d41649e7b4ee5685e"))
 (doom-themes :source "elpaca-menu-lock-file" :recipe
              (:package "doom-themes" :fetcher github :repo "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el") :source
                        "elpaca-menu-lock-file" :id doom-themes :host github :branch "master" :type
                        git :protocol https :inherit t :depth treeless :ref
                        "53645a905dfb3055db52f5d418d5ef612027e062"))
 (editorconfig :source "elpaca-menu-lock-file" :recipe
               (:package "editorconfig" :fetcher github :repo "editorconfig/editorconfig-emacs"
                         :old-names (editorconfig-core editorconfig-fnmatch) :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id editorconfig :type git :protocol https
                         :inherit t :depth treeless :ref "b18fcf7fdea1ce84b7fdc60360ad8016b5c00d79"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el")) :source "elpaca-menu-lock-file"
                       :id elisp-refs :type git :protocol https :inherit t :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elixir-ts-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "elixir-ts-mode" :repo "wkirschbaum/elixir-ts-mode" :fetcher github
                           :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                      "LICENSE" "README*" "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :id elixir-ts-mode :type git :protocol
                           https :inherit t :depth treeless :ref
                           "e518e9a086d3d3c427411fbe3aa22f34c2e85614"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo "https://github.com/progfolio/elpaca.git" :ref
            "5543a4f0019edefe9d7f6697d551276d1ed2b28c" :depth 1 :inherit ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build (:not elpaca-activate) :type
            git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build (:not elpaca-build-docs)
                               :source "elpaca-menu-lock-file" :id elpaca-use-package :type git
                               :protocol https :inherit t :depth treeless :ref
                               "5543a4f0019edefe9d7f6697d551276d1ed2b28c"))
 (erlang :source "elpaca-menu-lock-file" :recipe
         (:package "erlang" :fetcher github :repo "erlang/otp" :version-regexp "OTP-%v" :files
                   ("lib/tools/emacs/*.el" (:exclude "lib/tools/emacs/erlang_appwiz.el")) :source
                   "elpaca-menu-lock-file" :id erlang :type git :protocol https :inherit t :depth
                   treeless :ref "ccab260a5fd9990728d71ad0fd9233e5c3ee7b5a"))
 (erlang-ts :source "elpaca-menu-lock-file" :recipe
            (:package "erlang-ts" :fetcher github :repo "JimMoen/emacs-erlang-ts" :files ("*.el")
                      :source "elpaca-menu-lock-file" :id erlang-ts :host github :branch
                      "feat-highlight-faces" :build (:not elpaca-check-version) :type git :protocol
                      https :inherit t :depth treeless :ref
                      "a9523914f3c7633a3e63d380f0ecbafd5fc18b21"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file" :id exec-path-from-shell :type git
                                 :protocol https :inherit t :depth treeless :ref
                                 "7552abf032a383ff761e7d90e6b5cbb4658a728a"))
 (expand-region :source "elpaca-menu-lock-file" :recipe
                (:package "expand-region" :repo "magnars/expand-region.el" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id expand-region :type git :protocol
                          https :inherit t :depth treeless :ref
                          "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                         "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :id f :type git :protocol https :inherit t :depth
              treeless :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id flycheck :type git :protocol https :inherit
                     t :depth treeless :ref "fea21359413f59b15f36ef2e237399341237259f"))
 (flycheck-aspell :source "elpaca-menu-lock-file" :recipe
                  (:package "flycheck-aspell" :fetcher github :repo "leotaku/flycheck-aspell" :files
                            ("flycheck-aspell.el") :source "elpaca-menu-lock-file" :id
                            flycheck-aspell :type git :protocol https :inherit t :depth treeless
                            :ref "abbac0f6ccd94224f19c70d8545fddfe9c27351f"))
 (flycheck-rust :source "elpaca-menu-lock-file" :recipe
                (:package "flycheck-rust" :repo "flycheck/flycheck-rust" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id flycheck-rust :type git :protocol
                          https :inherit t :depth treeless :ref
                          "b9db73a7a5980ca884d5dd0cbe79b3291a185972"))
 (frame-local :source "elpaca-menu-lock-file" :recipe
              (:package "frame-local" :fetcher github :repo "sebastiencs/frame-local" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id frame-local :type git :protocol https
                        :inherit t :depth treeless :ref "7ee1106c3bcd4022f48421f8cb1ef4f995da816e"))
 (fzf :source "elpaca-menu-lock-file" :recipe
      (:package "fzf" :repo "bling/fzf.el" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id fzf :type git :protocol https :inherit t :depth
                treeless :ref "0f6a2fd644bedfbcc061f995c8c270d084da1cba"))
 (general :source "elpaca-menu-lock-file" :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id general :wait t :type git :protocol https
                    :inherit t :depth treeless :ref "a48768f85a655fe77b5f45c2880b420da1b1b9c3"))
 (git-timemachine :source "elpaca-menu-lock-file" :recipe
                  (:package "git-timemachine" :fetcher codeberg :repo "pidu/git-timemachine" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id git-timemachine :type git :protocol
                            https :inherit t :depth treeless :ref
                            "d1346a76122595aeeb7ebb292765841c6cfd417b"))
 (go-mode :source "elpaca-menu-lock-file" :recipe
          (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher github :files ("go-mode.el")
                    :source "elpaca-menu-lock-file" :id go-mode :type git :protocol https :inherit t
                    :depth treeless :ref "0ed3c5227e7f622589f1411b4939c3ee34711ebd"))
 (hcl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id hcl-mode :type git :protocol https :inherit
                     t :depth treeless :ref "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed"))
 (heex-ts-mode :source "elpaca-menu-lock-file" :recipe
               (:package "heex-ts-mode" :repo "wkirschbaum/heex-ts-mode" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id heex-ts-mode :type git :protocol https
                         :inherit t :depth treeless :ref "7496adcad0339978ef462b92160b6d40c3595841"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id helpful :type git :protocol https :inherit t
                    :depth treeless :ref "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (highlight-thing :source "elpaca-menu-lock-file" :recipe
                  (:package "highlight-thing" :fetcher github :repo "fgeller/highlight-thing.el"
                            :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id highlight-thing :type git :protocol
                            https :inherit t :depth treeless :ref
                            "fd0309e72727e5332be3397e15edb035b426dd0b"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id hl-todo :type git :protocol https :inherit t
                    :depth treeless :ref "9540fc414014822dde00f0188b74e17ac99e916d"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                          "README*" "*-pkg.el"))
               :source "elpaca-menu-lock-file" :id ht :type git :protocol https :inherit t :depth
               treeless :ref "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (hydra :source "elpaca-menu-lock-file" :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :source "elpaca-menu-lock-file" :id hydra :type git
                  :protocol https :inherit t :depth treeless :ref
                  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (indent-bars :source "elpaca-menu-lock-file" :recipe
              (:package "indent-bars" :repo
                        ("https://github.com/jdtsmith/indent-bars" . "indent-bars") :tar "1.0.0"
                        :host gnu :files ("*" (:exclude ".git" "LICENSE")) :source
                        "elpaca-menu-lock-file" :id indent-bars :type git :protocol https :inherit t
                        :depth treeless :ref "6e6bb5484edebf22654a960073f1ae23b4fe9a1e"))
 (info-colors :source "elpaca-menu-lock-file" :recipe
              (:package "info-colors" :fetcher github :repo "ubolonton/info-colors" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id info-colors :type git :protocol https
                        :inherit t :depth treeless :ref "2e237c301ba62f0e0286a27c1abe48c4c8441143"))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
             (:package "inheritenv" :fetcher github :repo "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id inheritenv :type git :protocol https
                       :inherit t :depth treeless :ref "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (ivy :source "elpaca-menu-lock-file" :recipe
      (:package "ivy" :repo "abo-abo/swiper" :fetcher github :files
                (:defaults "doc/ivy-help.org"
                           (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el"))
                :source "elpaca-menu-lock-file" :id ivy :type git :protocol https :inherit t :depth
                treeless :ref "631df3f68c79cca2a26bc8ea1a98a332ef53751e"))
 (ivy-hydra :source "elpaca-menu-lock-file" :recipe
            (:package "ivy-hydra" :repo "abo-abo/swiper" :fetcher github :files ("ivy-hydra.el")
                      :source "elpaca-menu-lock-file" :id ivy-hydra :type git :protocol https
                      :inherit t :depth treeless :ref "631df3f68c79cca2a26bc8ea1a98a332ef53751e"))
 (ivy-prescient :source "elpaca-menu-lock-file" :recipe
                (:package "ivy-prescient" :fetcher github :repo "radian-software/prescient.el"
                          :files ("ivy-prescient.el") :source "elpaca-menu-lock-file" :id
                          ivy-prescient :type git :protocol https :inherit t :depth treeless :ref
                          "87e2d2f2ddf24f591a5f70cc90d2afb4537caa18"))
 (ivy-rich :source "elpaca-menu-lock-file" :recipe
           (:package "ivy-rich" :repo "Yevgnen/ivy-rich" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id ivy-rich :type git :protocol https :inherit
                     t :depth treeless :ref "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4"))
 (ligature :source "elpaca-menu-lock-file" :recipe
           (:package "ligature" :fetcher github :repo "mickeynp/ligature.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id ligature :type git :protocol https :inherit
                     t :depth treeless :ref "6ac1634612dbd42f7eb81ecaf022bd239aabb954"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files ("llama.el" ".dir-locals.el")
                  :source "elpaca-menu-lock-file" :id llama :type git :protocol https :inherit t
                  :depth treeless :ref "d430d48e0b5afd2a34b5531f103dcb110c3539c4"))
 (lsp-ivy :source "elpaca-menu-lock-file" :recipe
          (:package "lsp-ivy" :repo "emacs-lsp/lsp-ivy" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id lsp-ivy :type git :protocol https :inherit t
                    :depth treeless :ref "8e0b8fdec086375fd7560857a84cd78f5047ad9d"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github :files
                     (:defaults "clients/*.*") :source "elpaca-menu-lock-file" :id lsp-mode :type
                     git :protocol https :inherit t :depth treeless :ref
                     "fd4df7cf17326b34257329f2fbd898573a1d106d"))
 (lsp-pyright :source "elpaca-menu-lock-file" :recipe
              (:package "lsp-pyright" :repo "emacs-lsp/lsp-pyright" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id lsp-pyright :type git :protocol https
                        :inherit t :depth treeless :ref "21b8f487855feb08f7df669b8884fbd5861dca25"))
 (lsp-treemacs :source "elpaca-menu-lock-file" :recipe
               (:package "lsp-treemacs" :repo "emacs-lsp/lsp-treemacs" :fetcher github :files
                         (:defaults "icons") :source "elpaca-menu-lock-file" :id lsp-treemacs :type
                         git :protocol https :inherit t :depth treeless :ref
                         "49df7292c521b4bac058985ceeaf006607b497dd"))
 (lsp-ui :source "elpaca-menu-lock-file" :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github :files
                   (:defaults "lsp-ui-doc.html" "resources") :source "elpaca-menu-lock-file" :id
                   lsp-ui :type git :protocol https :inherit t :depth treeless :ref
                   "ff349658ed69086bd18c336c8a071ba15f7fd574"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el") :source
               "elpaca-menu-lock-file" :id lv :type git :protocol https :inherit t :depth treeless
               :ref "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE"
                   ".dir-locals.el" ("git-hooks" "git-hooks/*") (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :id magit :type git :protocol https :inherit t
                  :depth treeless :ref "b9f19bae4d5e5c485d2d8d7bf52364eeb7d22a6b"))
 (magit-delta :source "elpaca-menu-lock-file" :recipe
              (:package "magit-delta" :fetcher github :repo "dandavison/magit-delta" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id magit-delta :type git :protocol https
                        :inherit t :depth treeless :ref "5fc7dbddcfacfe46d3fd876172ad02a9ab6ac616"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "docs/magit-section.texi" "magit-section-pkg.el")
                          :source "elpaca-menu-lock-file" :id magit-section :type git :protocol
                          https :inherit t :depth treeless :ref
                          "b9f19bae4d5e5c485d2d8d7bf52364eeb7d22a6b"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id markdown-mode :type git :protocol
                          https :inherit t :depth treeless :ref
                          "107a368a6deffa943544c220b5a6b1304ffc9945"))
 (mcp :source "elpaca-menu-lock-file" :recipe
      (:package "mcp" :fetcher github :repo "lizqwerscott/mcp.el" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id mcp :type git :protocol https :inherit t :depth
                treeless :ref "5c105a8db470eb9777fdbd26251548dec42c03f0"))
 (mise :source "elpaca-menu-lock-file" :recipe
       (:package "mise" :fetcher github :repo "eki3z/mise.el" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id mise :type git :protocol https :inherit t
                 :depth treeless :ref "849c44b36594c80c57a555c5671dd1a5a83d3184"))
 (multiple-cursors :source "elpaca-menu-lock-file" :recipe
                   (:package "multiple-cursors" :fetcher github :repo "magnars/multiple-cursors.el"
                             :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :id multiple-cursors :type git
                             :protocol https :inherit t :depth treeless :ref
                             "ddd677091afc7d65ce56d11866e18aeded110ada"))
 (names :source "elpaca-menu-lock-file" :recipe
        (:package "names" :repo "Malabarba/names" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :id names :type git :protocol https :inherit t
                  :depth treeless :ref "45a272fae915148d9a74d4cb3c39917b272ee9c3"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
             (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el" :fetcher github :files
                       (:defaults "data") :source "elpaca-menu-lock-file" :id nerd-icons :host
                       github :type git :protocol https :inherit t :depth treeless :ref
                       "9a7f44db9a53567f04603bc88d05402cad49c64c"))
 (nerd-icons-dired :source "elpaca-menu-lock-file" :recipe
                   (:package "nerd-icons-dired" :repo "rainstormstudio/nerd-icons-dired" :fetcher
                             github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :id nerd-icons-dired :type git
                             :protocol https :inherit t :depth treeless :ref
                             "929b62f01b93d30a3f42cc507fc45c84a2457b3f"))
 (nerd-icons-ibuffer :source "elpaca-menu-lock-file" :recipe
                     (:package "nerd-icons-ibuffer" :repo "seagle0128/nerd-icons-ibuffer" :fetcher
                               github :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :id nerd-icons-ibuffer :type git
                               :protocol https :inherit t :depth treeless :ref
                               "0cf63e4fa666cc9f3717e182f72342dca9f31f67"))
 (nerd-icons-ivy-rich :source "elpaca-menu-lock-file" :recipe
                      (:package "nerd-icons-ivy-rich" :fetcher github :repo
                                "seagle0128/nerd-icons-ivy-rich" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                :source "elpaca-menu-lock-file" :id nerd-icons-ivy-rich :type git
                                :protocol https :inherit t :depth treeless :ref
                                "7714b1194186cdd8353e2d80b40ae68c75aa3cd7"))
 (nginx-mode :source "elpaca-menu-lock-file" :recipe
             (:package "nginx-mode" :fetcher github :repo "ajc/nginx-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id nginx-mode :type git :protocol https
                       :inherit t :depth treeless :ref "c4ac5de975d65c84893a130a470af32a48b0b66c"))
 (no-littering :source "elpaca-menu-lock-file" :recipe
               (:package "no-littering" :fetcher github :repo "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id no-littering :host github :wait t :type
                         git :protocol https :inherit t :depth treeless :ref
                         "3b1d39d6b37f1f6f7dbda46712e40bc700ac79d2"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
            (:package "pdf-tools" :fetcher github :repo "vedang/pdf-tools" :files
                      (:defaults "README" ("build" "Makefile") ("build" "server")) :source
                      "elpaca-menu-lock-file" :id pdf-tools :type git :protocol https :inherit t
                      :depth treeless :ref "365f88238f46f9b1425685562105881800f10386"))
 (persistent-cached-load-filter :source "elpaca-menu-lock-file" :recipe
                                (:source "elpaca-menu-lock-file" :package
                                         "persistent-cached-load-filter" :id
                                         persistent-cached-load-filter :host github :repo
                                         "include-yy/persistent-cached-load-filter" :type git
                                         :protocol https :inherit t :depth treeless :ref
                                         "812e09364da8cd2ffabb63f6bb0dcd73e32e8120"))
 (persp-mode :source "elpaca-menu-lock-file" :recipe
             (:package "persp-mode" :repo "Bad-ptr/persp-mode.el" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id persp-mode :type git :protocol https
                       :inherit t :depth treeless :ref "fab4bf76927445d2e431f06e74572acba81f47d5"))
 (pfuture :source "elpaca-menu-lock-file" :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id pfuture :type git :protocol https :inherit t
                    :depth treeless :ref "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (pkgbuild-mode :source "elpaca-menu-lock-file" :recipe
                (:package "pkgbuild-mode" :fetcher github :repo "juergenhoetzel/pkgbuild-mode"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id pkgbuild-mode :type git :protocol
                          https :inherit t :depth treeless :ref
                          "aadf3d1d19c5eb9b52c15c5b73b1a46faac5b7d5"))
 (polymode :source "elpaca-menu-lock-file" :recipe
           (:package "polymode" :fetcher github :repo "polymode/polymode" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id polymode :type git :protocol https :inherit
                     t :depth treeless :ref "4604f55cc020c75562526fb76b723e5e242c97c0"))
 (popup :source "elpaca-menu-lock-file" :recipe
        (:package "popup" :fetcher github :repo "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :id popup :type git :protocol https :inherit t
                  :depth treeless :ref "45a0b759076ce4139aba36dde0a2904136282e73"))
 (pos-tip :source "elpaca-menu-lock-file" :recipe
          (:package "pos-tip" :repo "pitkali/pos-tip" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id pos-tip :type git :protocol https :inherit t
                    :depth treeless :ref "4889e08cf9077c8589ea6fea4e2ce558614dfcde"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id posframe :type git :protocol https :inherit
                     t :depth treeless :ref "3a80911b2f45ce6926196930bb7d5cc662c7b3c8"))
 (prescient :source "elpaca-menu-lock-file" :recipe
            (:package "prescient" :fetcher github :repo "radian-software/prescient.el" :files
                      ("prescient.el") :source "elpaca-menu-lock-file" :id prescient :type git
                      :protocol https :inherit t :depth treeless :ref
                      "87e2d2f2ddf24f591a5f70cc90d2afb4537caa18"))
 (projectile :source "elpaca-menu-lock-file" :recipe
             (:package "projectile" :fetcher github :repo "bbatsov/projectile" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id projectile :type git :protocol https
                       :inherit t :depth treeless :ref "136a1e6918a4d937e054e3fc06569c989cb013f3"))
 (protobuf-mode :source "elpaca-menu-lock-file" :recipe
                (:package "protobuf-mode" :fetcher github :repo "protocolbuffers/protobuf" :files
                          ("editors/protobuf-mode.el") :source "elpaca-menu-lock-file" :id
                          protobuf-mode :type git :protocol https :inherit t :depth treeless :ref
                          "627c33533d639160298edd33c5d7ebca9df169dd"))
 (pythonic :source "elpaca-menu-lock-file" :recipe
           (:package "pythonic" :fetcher github :repo "pythonic-emacs/pythonic" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id pythonic :type git :protocol https :inherit
                     t :depth treeless :ref "bf364a29e2f21828941ee3d11a27127bc260740f"))
 (qml-mode :source "elpaca-menu-lock-file" :recipe
           (:package "qml-mode" :repo "coldnew/qml-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id qml-mode :type git :protocol https :inherit
                     t :depth treeless :ref "6c5f33ba88ae010bf201a80ee8095e20a724558c"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github :repo
                               "Fanael/rainbow-delimiters" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :id rainbow-delimiters :type git
                               :protocol https :inherit t :depth treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (region-occurrences-highlighter :source "elpaca-menu-lock-file" :recipe
                                 (:package "region-occurrences-highlighter" :repo
                                           "alvarogonzalezsotillo/region-occurrences-highlighter"
                                           :fetcher github :files
                                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                                            "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                                            "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                                            "docs/*.texinfo"
                                            (:exclude ".dir-locals.el" "test.el" "tests.el"
                                                      "*-test.el" "*-tests.el" "LICENSE" "README*"
                                                      "*-pkg.el"))
                                           :source "elpaca-menu-lock-file" :id
                                           region-occurrences-highlighter :type git :protocol https
                                           :inherit t :depth treeless :ref
                                           "98fc1020c68f339810beb753a29daba93ade57b5"))
 (request :source "elpaca-menu-lock-file"
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github :files ("request.el") :source
             "elpaca-menu-lock-file" :id request :type git :protocol https :inherit t :depth
             treeless :ref "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6"))
 (restclient :source "elpaca-menu-lock-file" :recipe
             (:package "restclient" :fetcher github :repo "emacsorphanage/restclient" :files
                       ("restclient.el") :source "elpaca-menu-lock-file" :id restclient :type git
                       :protocol https :inherit t :depth treeless :ref
                       "1800a4e367c250051617d0b8c16a7cbd7f47da69"))
 (rime :source "elpaca-menu-lock-file" :recipe
       (:package "rime" :repo "DogLooksGood/emacs-rime" :files ("*.el" "Makefile" "lib.c") :fetcher
                 github :source "elpaca-menu-lock-file" :id rime :host github :type git :protocol
                 https :inherit t :depth treeless :ref "f927d26e471e7d63de65ffa92897944242f2fd92"))
 (rust-mode :source "elpaca-menu-lock-file" :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id rust-mode :type git :protocol https
                      :inherit t :depth treeless :ref "668069ad8b6ca20bd0d2334db1c0d046809affd6"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                         "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :id s :type git :protocol https :inherit t :depth
              treeless :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (shackle :source "elpaca-menu-lock-file" :recipe
          (:package "shackle" :fetcher git :url "https://depp.brause.cc/shackle.git" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id shackle :type git :protocol https :inherit t
                    :depth treeless :ref "ae25e7e0e593520c8590440fe5e3c0ea8053dc26"))
 (shell-maker :source "elpaca-menu-lock-file" :recipe
              (:package "shell-maker" :fetcher github :repo "xenodium/shell-maker" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id shell-maker :type git :protocol https
                        :inherit t :depth treeless :ref "afd5509b99b28589bab3a0d06786118905eadba9"))
 (show-inactive-region :source "elpaca-menu-lock-file" :recipe
                       (:package "show-inactive-region" :fetcher codeberg :repo
                                 "ideasman42/emacs-show-inactive-region" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file" :id show-inactive-region :type git
                                 :protocol https :inherit t :depth treeless :ref
                                 "e5e5db361ee93da72d96f2cc3d7c1c5bf52fbb96"))
 (shrink-path :source "elpaca-menu-lock-file" :recipe
              (:package "shrink-path" :fetcher gitlab :repo "bennya/shrink-path.el" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id shrink-path :type git :protocol https
                        :inherit t :depth treeless :ref "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (sideline :source "elpaca-menu-lock-file" :recipe
           (:package "sideline" :repo "emacs-sideline/sideline" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id sideline :type git :protocol https :inherit
                     t :depth treeless :ref "b4ada1ddf7da96c2e87453130f6ff7b7d577810f"))
 (sideline-flycheck :source "elpaca-menu-lock-file" :recipe
                    (:package "sideline-flycheck" :repo "emacs-sideline/sideline-flycheck" :fetcher
                              github :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file" :id sideline-flycheck :type git
                              :protocol https :inherit t :depth treeless :ref
                              "5a4d63210acf735829b08164897fef6a2abc8bab"))
 (sideline-lsp :source "elpaca-menu-lock-file" :recipe
               (:package "sideline-lsp" :repo "emacs-sideline/sideline-lsp" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id sideline-lsp :type git :protocol https
                         :inherit t :depth treeless :ref "dfa29a7b27e5ab64788c76544444f678ae4db18d"))
 (sis :source "elpaca-menu-lock-file" :recipe
      (:package "sis" :fetcher github :repo "laishulu/emacs-smart-input-source" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id sis :type git :protocol https :inherit t :depth
                treeless :ref "515e1dbe0180f33c292660b4b70d02d47153be5b"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
              (:package "smartparens" :fetcher github :repo "Fuco1/smartparens" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id smartparens :type git :protocol https
                        :inherit t :depth treeless :ref "82d2cf084a19b0c2c3812e0550721f8a61996056"))
 (speed-type :source "elpaca-menu-lock-file" :recipe
             (:package "speed-type" :fetcher github :repo "dakra/speed-type" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id speed-type :type git :protocol https
                       :inherit t :depth treeless :ref "e27ebe21cf498ffd1718a18a77baa385c0bcca12"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo ("https://github.com/Malabarba/spinner.el" . "spinner") :tar
                    "1.7.4" :host gnu :files ("*" (:exclude ".git")) :source "elpaca-menu-lock-file"
                    :id spinner :type git :protocol https :inherit t :depth treeless :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (ssh-config-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "ssh-config-mode" :fetcher github :repo "peterhoeg/ssh-config-mode-el"
                            :files (:defaults "*.txt") :source "elpaca-menu-lock-file" :id
                            ssh-config-mode :type git :protocol https :inherit t :depth treeless
                            :ref "f21726d6f44a0e769a15f0a94620078a326774f7"))
 (sudo-edit :source "elpaca-menu-lock-file" :recipe
            (:package "sudo-edit" :repo "nflath/sudo-edit" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id sudo-edit :type git :protocol https
                      :inherit t :depth treeless :ref "74eb1e6986461baed9a9269566ff838530b4379b"))
 (swiper :source "elpaca-menu-lock-file" :recipe
         (:package "swiper" :repo "abo-abo/swiper" :fetcher github :files ("swiper.el") :source
                   "elpaca-menu-lock-file" :id swiper :type git :protocol https :inherit t :depth
                   treeless :ref "631df3f68c79cca2a26bc8ea1a98a332ef53751e"))
 (systemd :source "elpaca-menu-lock-file" :recipe
          (:package "systemd" :fetcher github :repo "holomorph/systemd-mode" :files
                    (:defaults "*.txt") :source "elpaca-menu-lock-file" :id systemd :type git
                    :protocol https :inherit t :depth treeless :ref
                    "8742607120fbc440821acbc351fda1e8e68a8806"))
 (tablist :source "elpaca-menu-lock-file" :recipe
          (:package "tablist" :fetcher github :repo "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id tablist :type git :protocol https :inherit t
                    :depth treeless :ref "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (treemacs :source "elpaca-menu-lock-file" :recipe
           (:package "treemacs" :fetcher github :repo "Alexander-Miller/treemacs" :files
                     (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el"
                                "src/scripts/treemacs*.py" (:exclude "src/extra/*"))
                     :source "elpaca-menu-lock-file" :id treemacs :type git :protocol https :inherit
                     t :depth treeless :ref "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (treemacs-nerd-icons :source "elpaca-menu-lock-file" :recipe
                      (:package "treemacs-nerd-icons" :fetcher github :repo
                                "rainstormstudio/treemacs-nerd-icons" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                :source "elpaca-menu-lock-file" :id treemacs-nerd-icons :type git
                                :protocol https :inherit t :depth treeless :ref
                                "0c5ddcb978da639f01ddb023febc40fc755171e5"))
 (treesit-fold :source "elpaca-menu-lock-file" :recipe
               (:package "treesit-fold" :repo "emacs-tree-sitter/treesit-fold" :tar "0.2.1" :host
                         github :files ("*" (:exclude ".git")) :source "elpaca-menu-lock-file" :id
                         treesit-fold :type git :protocol https :inherit t :depth treeless :ref
                         "d70c5f7240a8a48819421260c9a018c884a41111"))
 (treesit-langs :source "elpaca-menu-lock-file" :recipe
                (:source "elpaca-menu-lock-file" :package "treesit-langs" :id treesit-langs :host
                         github :repo "emacs-tree-sitter/treesit-langs" :type git :protocol https
                         :inherit t :depth treeless :ref "08c2c5bccd85019c6e600b8c936566dca097bd02"))
 (typescript-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "typescript-mode" :fetcher github :repo "emacs-typescript/typescript.el"
                            :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id typescript-mode :type git :protocol
                            https :inherit t :depth treeless :ref
                            "2535780bdb318d86761b9bd21b0347ca6a89628f"))
 (ultra-scroll :source "elpaca-menu-lock-file" :recipe
               (:package "ultra-scroll" :fetcher github :repo "jdtsmith/ultra-scroll" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id ultra-scroll :host github :type git
                         :protocol https :inherit t :depth treeless :ref
                         "08758c6772c5fbce54fb74fb5cce080b6425c6ce"))
 (use-proxy :source "elpaca-menu-lock-file" :recipe
            (:package "use-proxy" :repo "rayw000/use-proxy" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id use-proxy :type git :protocol https
                      :inherit t :depth treeless :ref "b2995563f41c162a082cd4823a499887f807176e"))
 (uv-mode :source "elpaca-menu-lock-file" :recipe
          (:package "uv-mode" :fetcher github :repo "z80dev/uv-mode" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id uv-mode :type git :protocol https :inherit t
                    :depth treeless :ref "7e7f9b90832210b65823c3d58e3255cd164394b7"))
 (vundo :source "elpaca-menu-lock-file" :recipe
        (:package "vundo" :repo ("https://github.com/casouri/vundo" . "vundo") :tar "2.4.0" :host
                  gnu :files ("*" (:exclude ".git" "test")) :source "elpaca-menu-lock-file" :id
                  vundo :type git :protocol https :inherit t :depth treeless :ref
                  "e0af8c5845abf884a644215a9cac37f39c13cd5a"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files ("wgrep.el")
                  :source "elpaca-menu-lock-file" :id wgrep :type git :protocol https :inherit t
                  :depth treeless :ref "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (which-key :source "elpaca-menu-lock-file" :recipe
            (:package "which-key" :repo "justbur/emacs-which-key" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id which-key :type git :protocol https
                      :inherit t :depth treeless :ref "38d4308d1143b61e4004b6e7a940686784e51500"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
              "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                        "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :id with-editor :type git :protocol https :inherit t
             :depth treeless :ref "64211dcb815f2533ac3d2a7e56ff36ae804d8338"))
 (x509-mode :source "elpaca-menu-lock-file" :recipe
            (:package "x509-mode" :fetcher github :repo "jobbflykt/x509-mode" :files
                      (:defaults "*.txt") :source "elpaca-menu-lock-file" :id x509-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "02e62ebd857946de629e45bff6a7de533f9022bc"))
 (xterm-color :source "elpaca-menu-lock-file" :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id xterm-color :type git :protocol https
                        :inherit t :depth treeless :ref "86fab1d247eb5ebe6b40fa5073a70dfa487cd465"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id yaml :type git :protocol https :inherit t
                 :depth treeless :ref "f2369fb4985ed054be47ae111760ff2075dff72a"))
 (yaml-pro :source "elpaca-menu-lock-file" :recipe
           (:package "yaml-pro" :repo "zkry/yaml-pro" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id yaml-pro :type git :protocol https :inherit
                     t :depth treeless :ref "9b9509188e5b88bb933e98ab36ab992519b9554b"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github :files
                      ("yasnippet.el" "snippets") :source "elpaca-menu-lock-file" :id yasnippet
                      :type git :protocol https :inherit t :depth treeless :ref
                      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (yasnippet-snippets :source "elpaca-menu-lock-file" :recipe
                     (:package "yasnippet-snippets" :repo "AndreaCrotti/yasnippet-snippets" :fetcher
                               github :files ("*.el" "snippets" ".nosearch") :source
                               "elpaca-menu-lock-file" :id yasnippet-snippets :type git :protocol
                               https :inherit t :depth treeless :ref
                               "606ee926df6839243098de6d71332a697518cb86"))
 (youdao-dictionary :source "elpaca-menu-lock-file" :recipe
                    (:package "youdao-dictionary" :repo "xuchunyang/youdao-dictionary.el" :fetcher
                              github :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file" :id youdao-dictionary :type git
                              :protocol https :inherit t :depth treeless :ref
                              "eae8efb1efd3fc82cfe87a357fe8f764116d94ef")))
