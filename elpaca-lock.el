((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id ace-window :type git :protocol https
                       :inherit t :depth nil :ref "77115afc1b0b9f633084cf7479c767988106c196"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia" :files
                     (:defaults ("scripts" "scripts/formatters")) :source "elpaca-menu-lock-file"
                     :id apheleia :type git :protocol https :inherit t :depth nil :ref
                     "14a0bb4454fb2cc3b5b377619288b742ce117da5"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id avy :type git :protocol https :inherit t :depth
                nil :ref "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (beacon :source "elpaca-menu-lock-file" :recipe
         (:package "beacon" :fetcher github :repo "Malabarba/beacon" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                              "LICENSE" "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id beacon :type git :protocol https :inherit t
                   :depth nil :ref "85261a928ae0ec3b41e639f05291ffd6bf7c231c"))
 (c3-ts-mode :source "elpaca-menu-lock-file" :recipe
             (:source "elpaca-menu-lock-file" :package "c3-ts-mode" :id c3-ts-mode :host github
                      :repo "c3lang/c3-ts-mode" :files ("*.el") :type git :protocol https :inherit t
                      :depth nil :ref "9c14336b20f1a4741e2be90430bb4c54f024ff31"))
 (cal-china-x :source "elpaca-menu-lock-file" :recipe
              (:package "cal-china-x" :repo "xwl/cal-china-x" :fetcher github :files
                        (:defaults "jieqi.txt") :source "elpaca-menu-lock-file" :id cal-china-x
                        :type git :protocol https :inherit t :depth nil :ref
                        "e9e3067134b0606bfe2a6925d7488c4b24ce81d5"))
 (cargo-mode :source "elpaca-menu-lock-file" :recipe
             (:package "cargo-mode" :fetcher github :repo "ayrat555/cargo-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id cargo-mode :type git :protocol https
                       :inherit t :depth nil :ref "33528954218f8957a26f3fef506c3537823d569d"))
 (cfrs :source "elpaca-menu-lock-file" :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id cfrs :type git :protocol https :inherit t
                 :depth nil :ref "981bddb3fb9fd9c58aed182e352975bd10ad74c8"))
 (chinese-word-at-point :source "elpaca-menu-lock-file" :recipe
                        (:package "chinese-word-at-point" :repo
                                  "xuchunyang/chinese-word-at-point.el" :fetcher github :files
                                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                   "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                  :source "elpaca-menu-lock-file" :id chinese-word-at-point :type
                                  git :protocol https :inherit t :depth nil :ref
                                  "8223d7439e005555b86995a005b225ae042f0538"))
 (cns :source "elpaca-menu-lock-file" :recipe
      (:source "elpaca-menu-lock-file" :package "cns" :id cns :host github :repo
               "kanglmf/emacs-chinese-word-segmentation" :files ("cns.el") :build
               ((:before elpaca-check-version +cns-build)) :type git :protocol https :inherit t
               :depth nil :ref "fd2e711dc7fd957267df5056a57575b3eb090ac4"))
 (colorful-mode :source "elpaca-menu-lock-file" :recipe
                (:package "colorful-mode" :repo
                          ("https://github.com/DevelopmentCool2449/colorful-mode" . "colorful-mode")
                          :tar "1.2.5" :host gnu :files ("*" (:exclude ".git")) :source
                          "elpaca-menu-lock-file" :id colorful-mode :type git :protocol https
                          :inherit t :depth nil :ref "97a1dc0b3ac6b2706ec3e2f428ea66aea2d91fb6"))
 (company :source "elpaca-menu-lock-file" :recipe
          (:package "company" :fetcher github :repo "company-mode/company-mode" :files
                    (:defaults "icons" ("images/small" "doc/images/small/*.png")) :source
                    "elpaca-menu-lock-file" :id company :type git :protocol https :inherit t :depth
                    nil :ref "1cc907ac9e46ae4209eb5a341131787e0c678406"))
 (company-box :source "elpaca-menu-lock-file" :recipe
              (:package "company-box" :fetcher github :repo "sebastiencs/company-box" :files
                        (:defaults "images") :source "elpaca-menu-lock-file" :id company-box :type
                        git :protocol https :inherit t :depth nil :ref
                        "c4f2e243fba03c11e46b1600b124e036f2be7691"))
 (company-prescient :source "elpaca-menu-lock-file" :recipe
                    (:package "company-prescient" :fetcher github :repo
                              "radian-software/prescient.el" :files ("company-prescient.el") :source
                              "elpaca-menu-lock-file" :id company-prescient :type git :protocol
                              https :inherit t :depth nil :ref
                              "5649977fa7789e4615efeca09397ed7eccd06dfc"))
 (company-tabnine :source "elpaca-menu-lock-file" :recipe
                  (:package "company-tabnine" :repo "TommyX12/company-tabnine" :fetcher github
                            :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id company-tabnine :type git :protocol
                            https :inherit t :depth nil :ref
                            "083d290c19b874f2ae139c77a726932e618e29b2"))
 (compat :source "elpaca-menu-lock-file" :recipe
         (:package "compat" :repo ("https://github.com/emacs-compat/compat" . "compat") :tar
                   "31.0.0.2" :host gnu :files ("*" (:exclude ".git")) :source
                   "elpaca-menu-lock-file" :id compat :type git :protocol https :inherit t :depth
                   nil :ref "1f3d7596173cf2851d3c4181b15d6c3573a38252"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
              "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                        "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :id cond-let :type git :protocol https :inherit t
             :depth nil :ref "c48600dfab6372670225f046cace263700c78eab"))
 (counsel :source "elpaca-menu-lock-file" :recipe
          (:package "counsel" :repo "abo-abo/swiper" :fetcher github :files ("counsel.el") :source
                    "elpaca-menu-lock-file" :id counsel :type git :protocol https :inherit t :depth
                    nil :ref "0d02f5063d36ff4fa6138f0973c83c6d3874fba0"))
 (counsel-projectile :source "elpaca-menu-lock-file" :recipe
                     (:package "counsel-projectile" :fetcher github :repo
                               "ericdanan/counsel-projectile" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :id counsel-projectile :type git
                               :protocol https :inherit t :depth nil :ref
                               "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b"))
 (counsel-tramp :source "elpaca-menu-lock-file" :recipe
                (:package "counsel-tramp" :repo "masasam/emacs-counsel-tramp" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id counsel-tramp :type git :protocol
                          https :inherit t :depth nil :ref
                          "70dcc6b9da5e76fefbc92646e7d780b2a06ca93f"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files ("dash.el" "dash.texi")
                 :source "elpaca-menu-lock-file" :id dash :type git :protocol https :inherit t
                 :depth nil :ref "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (dashboard :source "elpaca-menu-lock-file" :recipe
            (:package "dashboard" :fetcher github :repo "emacs-dashboard/dashboard" :files
                      (:defaults "banners") :source "elpaca-menu-lock-file" :id dashboard :type git
                      :protocol https :inherit t :depth nil :ref
                      "8642af9db8c996530ffaed1f56fe95c68b96a36d"))
 (dashboard-ls :source "elpaca-menu-lock-file" :recipe
               (:package "dashboard-ls" :repo "emacs-dashboard/dashboard-ls" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id dashboard-ls :type git :protocol https
                         :inherit t :depth nil :ref "9b5e35a938287fe2ad6413b424ece3eab52f64e4"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id diff-hl :type git :protocol https :inherit t
                    :depth nil :ref "91fcd4fa42fef895a754e80c4435ae6314be7822"))
 (dired-git-info :source "elpaca-menu-lock-file" :recipe
                 (:package "dired-git-info" :repo
                           ("https://github.com/clemera/dired-git-info" . "dired-git-info") :tar
                           "0.3.1" :host gnu :files ("*" (:exclude ".git")) :source
                           "elpaca-menu-lock-file" :id dired-git-info :type git :protocol https
                           :inherit t :depth nil :ref "91d57e3a4c5104c66a3abc18e281ee55e8979176"))
 (dired-single :source "elpaca-menu-lock-file" :recipe
               (:source "elpaca-menu-lock-file" :package "dired-single" :id dired-single :host
                        github :repo "emacsattic/dired-single" :files ("*.el") :type git :protocol
                        https :inherit t :depth nil :ref "60fce6599326e12cc2033c28d50b8bf6c6ba164a"))
 (doom-modeline :source "elpaca-menu-lock-file" :recipe
                (:package "doom-modeline" :repo "seagle0128/doom-modeline" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id doom-modeline :type git :protocol
                          https :inherit t :depth nil :ref
                          "017854c6484dd6a38e4b039dad04ce6dbec02f08"))
 (doom-themes :source "elpaca-menu-lock-file" :recipe
              (:package "doom-themes" :fetcher github :repo "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el") :source
                        "elpaca-menu-lock-file" :id doom-themes :host github :branch "master" :type
                        git :protocol https :inherit t :depth nil :ref
                        "cc7686783e5e6e4174a345c768047d524f0dd54f"))
 (editorconfig :source "elpaca-menu-lock-file" :recipe
               (:package "editorconfig" :fetcher github :repo "editorconfig/editorconfig-emacs"
                         :old-names (editorconfig-core editorconfig-fnmatch) :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id editorconfig :type git :protocol https
                         :inherit t :depth nil :ref "b18fcf7fdea1ce84b7fdc60360ad8016b5c00d79"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el")) :source "elpaca-menu-lock-file"
                       :id elisp-refs :type git :protocol https :inherit t :depth nil :ref
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
                           https :inherit t :depth nil :ref
                           "e518e9a086d3d3c427411fbe3aa22f34c2e85614"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo "https://github.com/progfolio/elpaca.git" :ref
            "74848674bfca8590e9286309d11e9645c8425400" :depth nil :inherit ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build (:not elpaca-activate) :type
            git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build (:not elpaca-build-docs)
                               :source "elpaca-menu-lock-file" :id elpaca-use-package :type git
                               :protocol https :inherit t :depth nil :ref
                               "74848674bfca8590e9286309d11e9645c8425400"))
 (erlang :source "elpaca-menu-lock-file" :recipe
         (:package "erlang" :fetcher github :repo "erlang/otp" :version-regexp "OTP-%v" :files
                   ("lib/tools/emacs/*.el" (:exclude "lib/tools/emacs/erlang_appwiz.el")) :source
                   "elpaca-menu-lock-file" :id erlang :type git :protocol https :inherit t :depth
                   nil :ref "3a0274d1bb90fd7811af2c589fd9564c41e7121f"))
 (erlang-ts :source "elpaca-menu-lock-file" :recipe
            (:package "erlang-ts" :fetcher github :repo "JimMoen/emacs-erlang-ts" :files ("*.el")
                      :source "elpaca-menu-lock-file" :id erlang-ts :host github :branch
                      "feat-highlight-faces" :build (:not elpaca-check-version) :type git :protocol
                      https :inherit t :depth nil :ref "a9523914f3c7633a3e63d380f0ecbafd5fc18b21"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file" :id exec-path-from-shell :type git
                                 :protocol https :inherit t :depth nil :ref
                                 "6146fdc16e9882df270be7e58ae8d628032d6bc4"))
 (expand-region :source "elpaca-menu-lock-file" :recipe
                (:package "expand-region" :repo "magnars/expand-region.el" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id expand-region :type git :protocol
                          https :inherit t :depth nil :ref
                          "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                         "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :id f :type git :protocol https :inherit t :depth nil
              :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id flycheck :type git :protocol https :inherit
                     t :depth nil :ref "356bdd3570fe38dcff0f2228ced003d54383928d"))
 (flycheck-aspell :source "elpaca-menu-lock-file" :recipe
                  (:package "flycheck-aspell" :fetcher github :repo "leotaku/flycheck-aspell" :files
                            ("flycheck-aspell.el") :source "elpaca-menu-lock-file" :id
                            flycheck-aspell :type git :protocol https :inherit t :depth nil :ref
                            "abbac0f6ccd94224f19c70d8545fddfe9c27351f"))
 (flycheck-rust :source "elpaca-menu-lock-file" :recipe
                (:package "flycheck-rust" :repo "flycheck/flycheck-rust" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id flycheck-rust :type git :protocol
                          https :inherit t :depth nil :ref
                          "42a63a9ec969d77f5c56e101833546e1afb0d884"))
 (frame-local :source "elpaca-menu-lock-file" :recipe
              (:package "frame-local" :fetcher github :repo "sebastiencs/frame-local" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id frame-local :type git :protocol https
                        :inherit t :depth nil :ref "7ee1106c3bcd4022f48421f8cb1ef4f995da816e"))
 (fzf :source "elpaca-menu-lock-file" :recipe
      (:package "fzf" :repo "bling/fzf.el" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id fzf :type git :protocol https :inherit t :depth
                nil :ref "75060d81f4eea3f8088ac2a4a3a253c686b44d44"))
 (general :source "elpaca-menu-lock-file" :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id general :wait t :type git :protocol https
                    :inherit t :depth nil :ref "a48768f85a655fe77b5f45c2880b420da1b1b9c3"))
 (git-timemachine :source "elpaca-menu-lock-file" :recipe
                  (:package "git-timemachine" :fetcher codeberg :repo "pidu/git-timemachine" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id git-timemachine :type git :protocol
                            https :inherit t :depth nil :ref
                            "d1346a76122595aeeb7ebb292765841c6cfd417b"))
 (go-mode :source "elpaca-menu-lock-file" :recipe
          (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher github :files ("go-mode.el")
                    :source "elpaca-menu-lock-file" :id go-mode :type git :protocol https :inherit t
                    :depth nil :ref "3a71d28ab47df685e54ca6046a7a3dd3e28b682c"))
 (hcl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id hcl-mode :type git :protocol https :inherit
                     t :depth nil :ref "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed"))
 (heex-ts-mode :source "elpaca-menu-lock-file" :recipe
               (:package "heex-ts-mode" :repo "wkirschbaum/heex-ts-mode" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id heex-ts-mode :type git :protocol https
                         :inherit t :depth nil :ref "7496adcad0339978ef462b92160b6d40c3595841"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id helpful :type git :protocol https :inherit t
                    :depth nil :ref "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (highlight-thing :source "elpaca-menu-lock-file" :recipe
                  (:package "highlight-thing" :fetcher github :repo "fgeller/highlight-thing.el"
                            :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id highlight-thing :type git :protocol
                            https :inherit t :depth nil :ref
                            "fd0309e72727e5332be3397e15edb035b426dd0b"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id hl-todo :type git :protocol https :inherit t
                    :depth nil :ref "527d545b8c2f36243194cbe4a8d0e6ac9d50e6a7"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                          "README*" "*-pkg.el"))
               :source "elpaca-menu-lock-file" :id ht :type git :protocol https :inherit t :depth
               nil :ref "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (hydra :source "elpaca-menu-lock-file" :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :source "elpaca-menu-lock-file" :id hydra :type git
                  :protocol https :inherit t :depth nil :ref
                  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (indent-bars :source "elpaca-menu-lock-file" :recipe
              (:package "indent-bars" :repo
                        ("https://github.com/jdtsmith/indent-bars" . "indent-bars") :tar "1.0.0"
                        :host gnu :files ("*" (:exclude ".git" "LICENSE")) :source
                        "elpaca-menu-lock-file" :id indent-bars :type git :protocol https :inherit t
                        :depth nil :ref "f95fee11d9932cba71d686807025239be0319e91"))
 (info-colors :source "elpaca-menu-lock-file" :recipe
              (:package "info-colors" :fetcher github :repo "ubolonton/info-colors" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id info-colors :type git :protocol https
                        :inherit t :depth nil :ref "2e237c301ba62f0e0286a27c1abe48c4c8441143"))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
             (:package "inheritenv" :fetcher github :repo "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id inheritenv :type git :protocol https
                       :inherit t :depth nil :ref "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (ivy :source "elpaca-menu-lock-file" :recipe
      (:package "ivy" :repo "abo-abo/swiper" :fetcher github :files
                (:defaults "doc/ivy-help.org"
                           (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el"))
                :source "elpaca-menu-lock-file" :id ivy :type git :protocol https :inherit t :depth
                nil :ref "0d02f5063d36ff4fa6138f0973c83c6d3874fba0"))
 (ivy-hydra :source "elpaca-menu-lock-file" :recipe
            (:package "ivy-hydra" :repo "abo-abo/swiper" :fetcher github :files ("ivy-hydra.el")
                      :source "elpaca-menu-lock-file" :id ivy-hydra :type git :protocol https
                      :inherit t :depth nil :ref "0d02f5063d36ff4fa6138f0973c83c6d3874fba0"))
 (ivy-prescient :source "elpaca-menu-lock-file" :recipe
                (:package "ivy-prescient" :fetcher github :repo "radian-software/prescient.el"
                          :files ("ivy-prescient.el") :source "elpaca-menu-lock-file" :id
                          ivy-prescient :type git :protocol https :inherit t :depth nil :ref
                          "5649977fa7789e4615efeca09397ed7eccd06dfc"))
 (ivy-rich :source "elpaca-menu-lock-file" :recipe
           (:package "ivy-rich" :repo "Yevgnen/ivy-rich" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id ivy-rich :type git :protocol https :inherit
                     t :depth nil :ref "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4"))
 (kdl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "kdl-mode" :fetcher github :repo "taquangtrung/emacs-kdl-mode" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id kdl-mode :host github :type git :protocol
                     https :inherit t :depth nil :ref "2d849e298199f490e4894c01764a8a83decd704a"))
 (ligature :source "elpaca-menu-lock-file" :recipe
           (:package "ligature" :fetcher github :repo "mickeynp/ligature.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id ligature :type git :protocol https :inherit
                     t :depth nil :ref "5ef919d3e3e2cf2c4622cbea28e429b29e86fc97"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files ("llama.el" ".dir-locals.el")
                  :source "elpaca-menu-lock-file" :id llama :type git :protocol https :inherit t
                  :depth nil :ref "4d4024048053b898a01521046e0f063ee47615b0"))
 (lsp-ivy :source "elpaca-menu-lock-file" :recipe
          (:package "lsp-ivy" :repo "emacs-lsp/lsp-ivy" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id lsp-ivy :type git :protocol https :inherit t
                    :depth nil :ref "b061ff4afdcf3bac606fb46be84d67b03d24cfcc"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github :files
                     (:defaults "clients/*.*") :source "elpaca-menu-lock-file" :id lsp-mode :type
                     git :protocol https :inherit t :depth nil :ref
                     "6bfc593d7b1bc0dd656f09ffce52cc085ebced05"))
 (lsp-pyright :source "elpaca-menu-lock-file" :recipe
              (:package "lsp-pyright" :repo "emacs-lsp/lsp-pyright" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id lsp-pyright :type git :protocol https
                        :inherit t :depth nil :ref "147d6d9c799f945ba4a2fb14824ddea9ade6de74"))
 (lsp-treemacs :source "elpaca-menu-lock-file" :recipe
               (:package "lsp-treemacs" :repo "emacs-lsp/lsp-treemacs" :fetcher github :files
                         (:defaults "icons") :source "elpaca-menu-lock-file" :id lsp-treemacs :type
                         git :protocol https :inherit t :depth nil :ref
                         "3519ac907ea391e18d9599375b116aeeb6f8a38a"))
 (lsp-ui :source "elpaca-menu-lock-file" :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github :files
                   (:defaults "lsp-ui-doc.html" "resources") :source "elpaca-menu-lock-file" :id
                   lsp-ui :type git :protocol https :inherit t :depth nil :ref
                   "176eca71d1c5498ed6258b5b27d73293ff7cd7ed"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el") :source
               "elpaca-menu-lock-file" :id lv :type git :protocol https :inherit t :depth nil :ref
               "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE"
                   ".dir-locals.el" ("git-hooks" "git-hooks/*") (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :id magit :type git :protocol https :inherit t
                  :depth nil :ref "639ea6c61598f196c363c9299d21a7301b7d9fa0"))
 (magit-delta :source "elpaca-menu-lock-file" :recipe
              (:package "magit-delta" :fetcher github :repo "dandavison/magit-delta" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id magit-delta :type git :protocol https
                        :inherit t :depth nil :ref "5fc7dbddcfacfe46d3fd876172ad02a9ab6ac616"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "docs/magit-section.texi" "magit-section-pkg.el")
                          :source "elpaca-menu-lock-file" :id magit-section :type git :protocol
                          https :inherit t :depth nil :ref
                          "639ea6c61598f196c363c9299d21a7301b7d9fa0"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id markdown-mode :type git :protocol
                          https :inherit t :depth nil :ref
                          "f441e8bc9951e73b12c61e9198658488dd8e86e1"))
 (mise :source "elpaca-menu-lock-file" :recipe
       (:package "mise" :fetcher github :repo "eki3z/mise.el" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id mise :type git :protocol https :inherit t
                 :depth nil :ref "849c44b36594c80c57a555c5671dd1a5a83d3184"))
 (multiple-cursors :source "elpaca-menu-lock-file" :recipe
                   (:package "multiple-cursors" :fetcher github :repo "magnars/multiple-cursors.el"
                             :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :id multiple-cursors :type git
                             :protocol https :inherit t :depth nil :ref
                             "94b8b07a4bab87f803123723b68227565429dfa1"))
 (names :source "elpaca-menu-lock-file" :recipe
        (:package "names" :repo "Malabarba/names" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :id names :type git :protocol https :inherit t
                  :depth nil :ref "45a272fae915148d9a74d4cb3c39917b272ee9c3"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
             (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el" :fetcher github :files
                       (:defaults "data") :source "elpaca-menu-lock-file" :id nerd-icons :host
                       github :type git :protocol https :inherit t :depth nil :ref
                       "674909974637ff0ec2b5ebf43f9a8aefa35d93e9"))
 (nerd-icons-dired :source "elpaca-menu-lock-file" :recipe
                   (:package "nerd-icons-dired" :repo "rainstormstudio/nerd-icons-dired" :fetcher
                             github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :id nerd-icons-dired :type git
                             :protocol https :inherit t :depth nil :ref
                             "104acd8879528b8115589f35f1bbcbe231ad732f"))
 (nerd-icons-ibuffer :source "elpaca-menu-lock-file" :recipe
                     (:package "nerd-icons-ibuffer" :repo "seagle0128/nerd-icons-ibuffer" :fetcher
                               github :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :id nerd-icons-ibuffer :type git
                               :protocol https :inherit t :depth nil :ref
                               "22dcbeea8c7677c83c8464c34247cf968250ee18"))
 (nerd-icons-ivy-rich :source "elpaca-menu-lock-file" :recipe
                      (:package "nerd-icons-ivy-rich" :fetcher github :repo
                                "seagle0128/nerd-icons-ivy-rich" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                :source "elpaca-menu-lock-file" :id nerd-icons-ivy-rich :type git
                                :protocol https :inherit t :depth nil :ref
                                "5e48ac70294b3f78bf966bb30448130396daa937"))
 (nginx-mode :source "elpaca-menu-lock-file" :recipe
             (:package "nginx-mode" :fetcher github :repo "ajc/nginx-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id nginx-mode :type git :protocol https
                       :inherit t :depth nil :ref "c4ac5de975d65c84893a130a470af32a48b0b66c"))
 (no-littering :source "elpaca-menu-lock-file" :recipe
               (:package "no-littering" :fetcher github :repo "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id no-littering :host github :wait t :type
                         git :protocol https :inherit t :depth nil :ref
                         "fa0801e5a1135cdb5ba525869f578cea87e8db0e"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
            (:package "pdf-tools" :fetcher github :repo "vedang/pdf-tools" :files
                      (:defaults "README" ("build" "Makefile") ("build" "server")) :source
                      "elpaca-menu-lock-file" :id pdf-tools :type git :protocol https :inherit t
                      :depth nil :ref "365f88238f46f9b1425685562105881800f10386"))
 (persistent-cached-load-filter :source "elpaca-menu-lock-file" :recipe
                                (:source "elpaca-menu-lock-file" :package
                                         "persistent-cached-load-filter" :id
                                         persistent-cached-load-filter :host github :repo
                                         "include-yy/persistent-cached-load-filter" :type git
                                         :protocol https :inherit t :depth nil :ref
                                         "812e09364da8cd2ffabb63f6bb0dcd73e32e8120"))
 (persp-mode :source "elpaca-menu-lock-file" :recipe
             (:package "persp-mode" :repo "Bad-ptr/persp-mode.el" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id persp-mode :type git :protocol https
                       :inherit t :depth nil :ref "fab4bf76927445d2e431f06e74572acba81f47d5"))
 (pfuture :source "elpaca-menu-lock-file" :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id pfuture :type git :protocol https :inherit t
                    :depth nil :ref "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (pkgbuild-mode :source "elpaca-menu-lock-file" :recipe
                (:package "pkgbuild-mode" :fetcher github :repo "juergenhoetzel/pkgbuild-mode"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id pkgbuild-mode :type git :protocol
                          https :inherit t :depth nil :ref
                          "aadf3d1d19c5eb9b52c15c5b73b1a46faac5b7d5"))
 (popup :source "elpaca-menu-lock-file" :recipe
        (:package "popup" :fetcher github :repo "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :id popup :type git :protocol https :inherit t
                  :depth nil :ref "3afe431e9aa2e271aaf0412cbb50c733387e8ea4"))
 (pos-tip :source "elpaca-menu-lock-file" :recipe
          (:package "pos-tip" :repo "pitkali/pos-tip" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id pos-tip :type git :protocol https :inherit t
                    :depth nil :ref "4889e08cf9077c8589ea6fea4e2ce558614dfcde"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id posframe :type git :protocol https :inherit
                     t :depth nil :ref "74c8c56131ed866db47ae4191364b72dd4852456"))
 (prescient :source "elpaca-menu-lock-file" :recipe
            (:package "prescient" :fetcher github :repo "radian-software/prescient.el" :files
                      ("prescient.el") :source "elpaca-menu-lock-file" :id prescient :type git
                      :protocol https :inherit t :depth nil :ref
                      "5649977fa7789e4615efeca09397ed7eccd06dfc"))
 (projectile :source "elpaca-menu-lock-file" :recipe
             (:package "projectile" :fetcher github :repo "bbatsov/projectile" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id projectile :type git :protocol https
                       :inherit t :depth nil :ref "a1036def9946b842917f594b1b9ef6a85cfd9138"))
 (protobuf-mode :source "elpaca-menu-lock-file" :recipe
                (:package "protobuf-mode" :fetcher github :repo "protocolbuffers/protobuf" :files
                          ("editors/protobuf-mode.el") :source "elpaca-menu-lock-file" :id
                          protobuf-mode :type git :protocol https :inherit t :depth nil :ref
                          "8815f682d81d0ce6f2dd5509dcd584bf917b4975"))
 (pythonic :source "elpaca-menu-lock-file" :recipe
           (:package "pythonic" :fetcher github :repo "pythonic-emacs/pythonic" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id pythonic :type git :protocol https :inherit
                     t :depth nil :ref "6036d5e3eeef534946de2adf0775caebd7cba45e"))
 (qml-mode :source "elpaca-menu-lock-file" :recipe
           (:package "qml-mode" :repo "coldnew/qml-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id qml-mode :type git :protocol https :inherit
                     t :depth nil :ref "6c5f33ba88ae010bf201a80ee8095e20a724558c"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github :repo
                               "Fanael/rainbow-delimiters" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :id rainbow-delimiters :type git
                               :protocol https :inherit t :depth nil :ref
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
                                           :inherit t :depth nil :ref
                                           "444789ea3567fc85ba32c7349050024ba8e5b9f8"))
 (restclient :source "elpaca-menu-lock-file" :recipe
             (:package "restclient" :fetcher github :repo "emacsorphanage/restclient" :files
                       ("restclient.el") :source "elpaca-menu-lock-file" :id restclient :type git
                       :protocol https :inherit t :depth nil :ref
                       "d280632df39a175dac06037038105e32945624be"))
 (rime :source "elpaca-menu-lock-file" :recipe
       (:package "rime" :repo "DogLooksGood/emacs-rime" :files ("*.el" "Makefile" "lib.c") :fetcher
                 github :source "elpaca-menu-lock-file" :id rime :host github :type git :protocol
                 https :inherit t :depth nil :ref "f927d26e471e7d63de65ffa92897944242f2fd92"))
 (rust-mode :source "elpaca-menu-lock-file" :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id rust-mode :type git :protocol https
                      :inherit t :depth nil :ref "93778358b6af8ad9e123a41bc2c8c91877a9ffd5"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                         "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :id s :type git :protocol https :inherit t :depth nil
              :ref "7393fa6fa305403e628058c0ec78c35d610fab05"))
 (shackle :source "elpaca-menu-lock-file" :recipe
          (:package "shackle" :fetcher git :url "https://depp.brause.cc/shackle.git" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id shackle :type git :protocol https :inherit t
                    :depth nil :ref "ae25e7e0e593520c8590440fe5e3c0ea8053dc26"))
 (show-inactive-region :source "elpaca-menu-lock-file" :recipe
                       (:package "show-inactive-region" :fetcher codeberg :repo
                                 "ideasman42/emacs-show-inactive-region" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file" :id show-inactive-region :type git
                                 :protocol https :inherit t :depth nil :ref
                                 "c870fe641c56328458e16d92fd7aedf688844a3b"))
 (shrink-path :source "elpaca-menu-lock-file" :recipe
              (:package "shrink-path" :fetcher gitlab :repo "bennya/shrink-path.el" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id shrink-path :type git :protocol https
                        :inherit t :depth nil :ref "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (sideline :source "elpaca-menu-lock-file" :recipe
           (:package "sideline" :repo "emacs-sideline/sideline" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id sideline :type git :protocol https :inherit
                     t :depth nil :ref "247cd6f6a0a841c5da20b3cc179d77c118641d66"))
 (sideline-flycheck :source "elpaca-menu-lock-file" :recipe
                    (:package "sideline-flycheck" :repo "emacs-sideline/sideline-flycheck" :fetcher
                              github :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file" :id sideline-flycheck :type git
                              :protocol https :inherit t :depth nil :ref
                              "9ff4a7478af7180a4c1aedbe41d652355ba64e88"))
 (sideline-lsp :source "elpaca-menu-lock-file" :recipe
               (:package "sideline-lsp" :repo "emacs-sideline/sideline-lsp" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id sideline-lsp :type git :protocol https
                         :inherit t :depth nil :ref "0c43128d40b93a12f37aebc89572c1178857fd77"))
 (sis :source "elpaca-menu-lock-file" :recipe
      (:package "sis" :fetcher github :repo "laishulu/emacs-smart-input-source" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id sis :type git :protocol https :inherit t :depth
                nil :ref "7ca3e115f159f22ddb4bcea85a22246ee03c422c"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
              (:package "smartparens" :fetcher github :repo "Fuco1/smartparens" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id smartparens :type git :protocol https
                        :inherit t :depth nil :ref "82d2cf084a19b0c2c3812e0550721f8a61996056"))
 (speed-type :source "elpaca-menu-lock-file" :recipe
             (:package "speed-type" :fetcher github :repo "dakra/speed-type" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id speed-type :type git :protocol https
                       :inherit t :depth nil :ref "f004cce95e0f5c5048d38f973b654587ed73abf6"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo ("https://github.com/Malabarba/spinner.el" . "spinner") :tar
                    "1.7.4" :host gnu :files ("*" (:exclude ".git")) :source "elpaca-menu-lock-file"
                    :id spinner :type git :protocol https :inherit t :depth nil :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (ssh-config-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "ssh-config-mode" :fetcher github :repo "peterhoeg/ssh-config-mode-el"
                            :files (:defaults "*.txt") :source "elpaca-menu-lock-file" :id
                            ssh-config-mode :type git :protocol https :inherit t :depth nil :ref
                            "a171e48c3cef777556961323d4dd6172a94be0f1"))
 (sudo-edit :source "elpaca-menu-lock-file" :recipe
            (:package "sudo-edit" :repo "nflath/sudo-edit" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id sudo-edit :type git :protocol https
                      :inherit t :depth nil :ref "74eb1e6986461baed9a9269566ff838530b4379b"))
 (swiper :source "elpaca-menu-lock-file" :recipe
         (:package "swiper" :repo "abo-abo/swiper" :fetcher github :files ("swiper.el") :source
                   "elpaca-menu-lock-file" :id swiper :type git :protocol https :inherit t :depth
                   nil :ref "0d02f5063d36ff4fa6138f0973c83c6d3874fba0"))
 (systemd :source "elpaca-menu-lock-file" :recipe
          (:package "systemd" :fetcher github :repo "holomorph/systemd-mode" :files
                    (:defaults "*.txt") :source "elpaca-menu-lock-file" :id systemd :type git
                    :protocol https :inherit t :depth nil :ref
                    "8742607120fbc440821acbc351fda1e8e68a8806"))
 (tablist :source "elpaca-menu-lock-file" :recipe
          (:package "tablist" :fetcher github :repo "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id tablist :type git :protocol https :inherit t
                    :depth nil :ref "01f065e387ffe6b7a41f180f257cd12551c7a9c2"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id transient :host github :built-in 'override
                      :type git :protocol https :inherit t :depth nil :ref
                      "3d20a780605f0a33d6360dc0a2ce9174c69a9a92"))
 (treemacs :source "elpaca-menu-lock-file" :recipe
           (:package "treemacs" :fetcher github :repo "Alexander-Miller/treemacs" :files
                     (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el"
                                "src/scripts/treemacs*.py" (:exclude "src/extra/*"))
                     :source "elpaca-menu-lock-file" :id treemacs :type git :protocol https :inherit
                     t :depth nil :ref "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (treemacs-nerd-icons :source "elpaca-menu-lock-file" :recipe
                      (:package "treemacs-nerd-icons" :fetcher github :repo
                                "rainstormstudio/treemacs-nerd-icons" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                :source "elpaca-menu-lock-file" :id treemacs-nerd-icons :type git
                                :protocol https :inherit t :depth nil :ref
                                "0c5ddcb978da639f01ddb023febc40fc755171e5"))
 (treesit-fold :source "elpaca-menu-lock-file" :recipe
               (:package "treesit-fold" :repo "emacs-tree-sitter/treesit-fold" :tar "0.2.1" :host
                         github :files ("*" (:exclude ".git")) :source "elpaca-menu-lock-file" :id
                         treesit-fold :type git :protocol https :inherit t :depth nil :ref
                         "cc1003b730a3167b972cc8400dffe19be7988fc7"))
 (treesit-langs :source "elpaca-menu-lock-file" :recipe
                (:source "elpaca-menu-lock-file" :package "treesit-langs" :id treesit-langs :host
                         github :repo "emacs-tree-sitter/treesit-langs" :type git :protocol https
                         :inherit t :depth nil :ref "831864cf4945e68d605d611ee470312faa05f846"))
 (typescript-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "typescript-mode" :fetcher github :repo "emacs-typescript/typescript.el"
                            :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id typescript-mode :type git :protocol
                            https :inherit t :depth nil :ref
                            "2535780bdb318d86761b9bd21b0347ca6a89628f"))
 (ultra-scroll :source "elpaca-menu-lock-file" :recipe
               (:package "ultra-scroll" :fetcher github :repo "jdtsmith/ultra-scroll" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id ultra-scroll :host github :type git
                         :protocol https :inherit t :depth nil :ref
                         "5be267d2d92c230b4347e0769f584c71aec53589"))
 (use-proxy :source "elpaca-menu-lock-file" :recipe
            (:package "use-proxy" :repo "rayw000/use-proxy" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id use-proxy :type git :protocol https
                      :inherit t :depth nil :ref "b2995563f41c162a082cd4823a499887f807176e"))
 (uv-mode :source "elpaca-menu-lock-file" :recipe
          (:package "uv-mode" :fetcher github :repo "z80dev/uv-mode" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id uv-mode :type git :protocol https :inherit t
                    :depth nil :ref "7e7f9b90832210b65823c3d58e3255cd164394b7"))
 (vundo :source "elpaca-menu-lock-file" :recipe
        (:package "vundo" :repo ("https://github.com/casouri/vundo" . "vundo") :tar "2.4.0" :host
                  gnu :files ("*" (:exclude ".git" "test")) :source "elpaca-menu-lock-file" :id
                  vundo :type git :protocol https :inherit t :depth nil :ref
                  "e0af8c5845abf884a644215a9cac37f39c13cd5a"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files ("wgrep.el")
                  :source "elpaca-menu-lock-file" :id wgrep :type git :protocol https :inherit t
                  :depth nil :ref "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (which-key :source "elpaca-menu-lock-file" :recipe
            (:package "which-key" :repo "justbur/emacs-which-key" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id which-key :type git :protocol https
                      :inherit t :depth nil :ref "38d4308d1143b61e4004b6e7a940686784e51500"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
              "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                        "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :id with-editor :type git :protocol https :inherit t
             :depth nil :ref "45bfc6084f03e3aa7f4f8db20836d559186c5957"))
 (x509-mode :source "elpaca-menu-lock-file" :recipe
            (:package "x509-mode" :fetcher github :repo "jobbflykt/x509-mode" :files
                      (:defaults "*.txt") :source "elpaca-menu-lock-file" :id x509-mode :type git
                      :protocol https :inherit t :depth nil :ref
                      "c5dc5a97469166b5a4e835b878ca3c0ed2df055a"))
 (xterm-color :source "elpaca-menu-lock-file" :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id xterm-color :type git :protocol https
                        :inherit t :depth nil :ref "ffdad85e584dfc0857f2a1fb970f5ef0f5d31ba3"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id yaml :type git :protocol https :inherit t
                 :depth nil :ref "5546f36bde24a9a8c1934e0f6ce205cd41d72537"))
 (yaml-pro :source "elpaca-menu-lock-file" :recipe
           (:package "yaml-pro" :repo "zkry/yaml-pro" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id yaml-pro :type git :protocol https :inherit
                     t :depth nil :ref "9b9509188e5b88bb933e98ab36ab992519b9554b"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github :files
                      ("yasnippet.el" "snippets") :source "elpaca-menu-lock-file" :id yasnippet
                      :type git :protocol https :inherit t :depth nil :ref
                      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (yasnippet-snippets :source "elpaca-menu-lock-file" :recipe
                     (:package "yasnippet-snippets" :repo "AndreaCrotti/yasnippet-snippets" :fetcher
                               github :files ("*.el" "snippets" ".nosearch") :source
                               "elpaca-menu-lock-file" :id yasnippet-snippets :type git :protocol
                               https :inherit t :depth nil :ref
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
                              :protocol https :inherit t :depth nil :ref
                              "eae8efb1efd3fc82cfe87a357fe8f764116d94ef")))
